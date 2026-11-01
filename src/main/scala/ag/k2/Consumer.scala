package ag.k2

import language.experimental.captureChecking
import language.experimental.saferExceptions

import java.nio.charset.StandardCharsets
import java.time.{Duration, Instant}
import scala.util.control.NonFatal

// ---- Reading a stream
//
// A consumer reads one K2 subscription, as one worker. K2 delivers at least
// once, and a producer's retry can put an event in the stream twice, so the
// consumer remembers the ids it has handled (in `dir`) and passes each event
// on once. "Once" holds unless it crashes between handling a batch and
// recording that it did: then that batch comes again.

// The ids of the events a consumer has handled, with when K2 took them in.
// Ids older than `keep` are forgotten: K2 keeps records for at most 30 days
// (unless asked for more), so no duplicate of them can still arrive.
final class Seen(file: os.Path, keep: Duration = Duration.ofDays(35).nn) {
  private val lock_file = file / os.up / s".${file.last}.lock"

  private var ids: Map[EventId, Instant] = load()

  private def load(): Map[EventId, Instant] =
    if (!os.exists(file)) Map()
    else
      (for {
        line <- os.read.lines(file)
        // (a line half-written by a crash doesn't match)
        case Array(id, ms) <- Seq(line.split(' '))
        millis <- ms.toLongOption
      } yield (EventId(id), Instant.ofEpochMilli(millis).nn)).toMap

  def contains(id: EventId): Boolean = ids.contains(id)

  // Records `handled`, and forgets ids too old to come again.
  def add(handled: Seq[Stamped], now: Instant = Instant.now().nn): Unit =
    FileLock(lock_file) {
      val cutoff = now.minus(keep).nn
      ids =
        (ids ++ handled.map(s => s.id -> s.as_at)).filter(_._2.isAfter(cutoff))
      val tmp = file / os.up / s".${file.last}.tmp"
      os.write.over(
        tmp,
        ids.map((id, at) => s"${id.value} ${at.toEpochMilli}\n").mkString,
        createFolders = true
      )
      os.move(tmp, file, replaceExisting = true, atomicMove = true)
    }
}

// `subscription`: the subscription's name; made on first use, to start at
// the stream's oldest record (`from_earliest`) or after its newest.
// `worker`: this consumer's name within the subscription; consumers that
// share a subscription share its records, each needing its own name.
final class Consumer(
    client: K2Client,
    subscription: String,
    worker: String,
    kinds: EventKinds,
    dir: os.Path,
    from_earliest: Boolean = true,
    // a record that isn't an event at all (someone else wrote it): skipped
    on_malformed: (Received, Throwable) => Unit = (r, e) =>
      System.err.nn.println(
        s"skipping a malformed record from ${r.at.toString}: ${e.getMessage}"
      )
) {
  // looked up (or made) on first use, which needs K2
  private var subscription_id_cache: Option[String] = None
  private def subscription_id: String throws K2Exception =
    subscription_id_cache.getOrElse {
      val id = client.subscription(subscription, from_earliest)
      subscription_id_cache = Some(id)
      id
    }
  private val seen = Seen(dir / "seen")

  // Takes the next batch (at most `max` events), passes the events it hasn't
  // seen before to `handle`, in the order K2 gave them, and acks it. Returns
  // how many records the batch had: 0 means nothing is waiting now. If
  // `handle` throws, the batch goes back to K2 (to come again) and the
  // exception is rethrown. A batch's lease is 5 minutes: handle it in less.
  //
  // `handle` may throw checked exceptions: it's a closure, so it carries the
  // caller's capability to throw them, and poll only calls it (capture
  // checking makes sure poll doesn't keep it).
  def poll(max: Int = 100)(
      handle: Seq[Stamped] => Unit
  ): Int throws K2Exception =
    client.consume(subscription_id, worker, max) match {
      case None        => 0
      case Some(batch) =>
        val events = batch.records.flatMap { r =>
          try {
            Some(
              Envelope.decode(
                kinds,
                ujson.read(String(r.content, StandardCharsets.UTF_8)),
                r.at
              )
            )
          } catch {
            case e: EventException =>
              on_malformed(r, e); None
            case NonFatal(e) => // e.g. not JSON
              on_malformed(r, e); None
          }
        }
        // (an event can come twice within one batch, too)
        val fresh = events
          .filterNot(s => seen.contains(s.id))
          .distinctBy(_.id)
        var handled = false
        try {
          handle(fresh)
          handled = true
        } finally
          if (!handled)
            // the lease runs out in time anyway
            try client.nack(subscription_id, worker, batch.id)
            catch { case _: K2Exception => () }
        seen.add(fresh)
        client.ack(subscription_id, worker, batch.id)
        batch.records.size
    }
}
