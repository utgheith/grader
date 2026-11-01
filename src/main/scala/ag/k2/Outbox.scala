package ag.k2

import language.experimental.captureChecking
import language.experimental.saferExceptions

import java.nio.charset.StandardCharsets
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.{APPEND, CREATE, WRITE}
import java.time.Instant

// ---- A source's local output stream
//
// Sources aren't always online, so they don't write to K2 directly: they
// append events to a local outbox, and sync it to K2 whenever they can.
// Events are on disk (fsync'ed) before append returns, and stay in the
// outbox until K2 has them.
//
// In `dir`:
//   outbox.jsonl   one event per line, in its stream form (see Envelope)
//   sent           the id of the last one K2 has (everything up to it, too)
//
// A sync that fails part way may leave K2 with a batch it then sends again
// (K2 has no deduplication); consumers drop the duplicates by event id.

// The outcome of a sync: how many events went to K2, how many are left, and
// why it stopped early (e.g. no network) if it did. Stopping early isn't an
// error: the rest go on a later sync.
final case class SyncResult(
    sent: Int,
    pending: Int,
    stopped: Option[K2Exception]
)

// A capability: a pure function can't write to one.
final class Outbox(val dir: os.Path) extends caps.SharedCapability {
  private val log = dir / "outbox.jsonl"
  private val sent_file = dir / "sent"
  // appends take this one; syncs and compaction take `sync_lock` (and,
  // to change the log, this one too)
  private val append_lock = dir / ".append.lock"
  private val sync_lock = dir / ".sync.lock"

  // a little below K2's per request limit, to be safe
  private val batch_bytes = K2.max_request_bytes * 9 / 10

  // an event's line, with its id
  private final case class Line(id: EventId, text: String)

  private def record_of(line: Line): Record =
    Record(
      line.text.getBytes(StandardCharsets.UTF_8),
      Map("type" -> ujson.read(line.text)("type").str, "id" -> line.id.value)
    )

  // what K2 counts against its per record limit
  private def size_of(r: Record): Int =
    r.content.length + r.headers.map((k, v) => k.length + v.length).sum

  // what a record adds to a produce request: its content in base64, its
  // headers, and some JSON around them
  private def request_size_of(r: Record): Int =
    (r.content.length + 2) / 3 * 4 +
      r.headers.map((k, v) => k.length + v.length + 8).sum + 64

  // Drops a line left half-written by a crash during an append (the append
  // never returned, so the event was never recorded).
  private def repair(): Unit =
    if (os.exists(log)) {
      val bytes = os.read.bytes(log)
      if (bytes.nonEmpty && bytes.last != '\n') {
        val keep = bytes.lastIndexOf('\n'.toByte) + 1
        val channel = FileChannel.open(log.toNIO, WRITE).nn
        try {
          val _ = channel.truncate(keep.toLong)
          channel.force(true)
        } finally channel.close()
      }
    }

  private def lines(): Vector[Line] =
    if (os.exists(log))
      os.read
        .lines(log)
        .toVector
        .filter(_.nonEmpty)
        .map(l => Line(EventId(ujson.read(l)("id").str), l))
    else Vector()

  // How many of `all` K2 has: those up to the last one sent. If that one
  // isn't in the log, compaction dropped it, along with everything before
  // it: all of the log is still to send.
  private def sent(all: Vector[Line]): Int =
    if (!os.exists(sent_file)) 0
    else {
      val last = EventId(os.read(sent_file).trim)
      all.indexWhere(_.id == last) + 1
    }

  private def set_sent(last: EventId): Unit = {
    val tmp = dir / ".sent.tmp"
    os.write.over(tmp, last.value)
    os.move(tmp, sent_file, replaceExisting = true, atomicMove = true)
  }

  // Records `events`, all created at `as_of`; returns their ids. Throws,
  // recording none, if one is too big for K2.
  def append(
      events: Seq[Event],
      as_of: Instant = Instant.now().nn
  ): Seq[EventId] throws EventException = {
    val stamped = events.map { e =>
      val id = EventId.fresh()
      val line = Line(id, ujson.write(Envelope.encode(id, as_of, e)))
      val size = size_of(record_of(line))
      if (size > K2.max_record_bytes)
        throw EventException(
          s"a ${e.kind.name} event is $size bytes; K2 takes at most ${K2.max_record_bytes}"
        )
      line
    }
    FileLock(append_lock) {
      os.makeDir.all(dir)
      repair()
      val channel = FileChannel.open(log.toNIO, CREATE, WRITE, APPEND).nn
      try {
        val bytes =
          stamped.map(_.text + "\n").mkString.getBytes(StandardCharsets.UTF_8)
        val _ = channel.write(java.nio.ByteBuffer.wrap(bytes))
        channel.force(true)
      } finally channel.close()
    }
    stamped.map(_.id)
  }

  // How many events K2 doesn't have yet.
  def pending: Int = {
    val all = FileLock(append_lock)(lines())
    all.size - sent(all)
  }

  // Sends what K2 doesn't have yet, in order, in batches. Stops at the first
  // failure that a later try may get past (no network, K2 unavailable, a
  // batch that may or may not have been stored); throws on any other (a bad
  // token, a malformed request: retrying won't help).
  def sync(client: K2Client): SyncResult throws K2Exception =
    FileLock(sync_lock) {
      val all = FileLock(append_lock) { repair(); lines() }
      val start = sent(all)

      // consecutive runs of events, each run's request within the limit
      val batches = Vector.newBuilder[Vector[(Line, Record)]]
      var current = Vector.empty[(Line, Record)]
      var current_size = 0
      for (l <- all.drop(start)) {
        val r = record_of(l)
        val size = request_size_of(r)
        if (current.nonEmpty && current_size + size > batch_bytes) {
          batches += current
          current = Vector()
          current_size = 0
        }
        current :+= (l, r)
        current_size += size
      }
      if (current.nonEmpty) batches += current

      var done = start
      var stopped: Option[K2Exception] = None
      val it = batches.result().iterator
      while (stopped.isEmpty && it.hasNext) {
        val batch = it.next()
        try {
          client.produce(batch.map(_._2))
          done += batch.size
          set_sent(batch.last._1.id)
        } catch {
          // (no guard: the compiler can't tell what a guarded case catches)
          case e: K2Exception =>
            if (e.retryable || e.maybe_stored) stopped = Some(e)
            else throw e
        }
      }
      SyncResult(done - start, all.size - done, stopped)
    }

  // Drops the events K2 already has, so the outbox doesn't grow forever.
  // (The sent marker stays: its event is no longer in the log, which says
  // that all of the log is still to send.)
  def compact(): Unit = FileLock(sync_lock) {
    FileLock(append_lock) {
      repair()
      val all = lines()
      val tmp = dir / ".outbox.tmp"
      os.write.over(tmp, all.drop(sent(all)).map(_.text + "\n").mkString)
      os.move(tmp, log, replaceExisting = true, atomicMove = true)
    }
  }
}
