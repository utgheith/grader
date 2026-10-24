package ag.events

import upickle.default.ReadWriter

// ---- Views: state folded from the log as it's read
//
// Like a Kafka Streams table: a view reads the log forward, once, folding
// each event into a summary of what matters at that point in the stream. To
// catch up it reads only what was appended since. Nothing identifying is in
// the clear in a view (students are StudentIds, what identifies them is in
// Boxes), so a view can be checkpointed to disk and still be shredded.

// How to fold events into a state.
trait Projection[S] {
  def initial: S
  def step(state: S, logged: Logged): S
}

// The log folded through `projection`, up to `offset`. If `checkpoint` is
// given, the state is kept there between runs (any checkpoint that can't be
// read, or is ahead of the log, is ignored: the view is rebuilt). Safe to
// share between threads; separate processes each keep their own.
final class View[S: ReadWriter](
    log: EventLog,
    projection: Projection[S],
    checkpoint: Option[os.Path] = None
) {
  private var offset: Long = 0
  private var current: S = projection.initial
  private var saved: Long = 0

  for (file <- checkpoint if os.exists(file)) {
    scala.util
      .Try(upickle.default.read[Checkpoint[S]](os.read(file)))
      .toOption
      .filter(_.offset <= log.end)
      .foreach { c =>
        offset = c.offset
        current = c.state
        saved = c.offset
      }
  }

  private def fold(logged: IterableOnce[Logged]): Unit =
    for (l <- logged.iterator) {
      if (l.offset != offset)
        throw EventLogException(
          s"view expected offset $offset, the log gave ${l.offset}"
        )
      current = projection.step(current, l)
      offset += 1
    }

  private def save(): Unit =
    for (file <- checkpoint if saved != offset) {
      val tmp = file / os.up / s".${file.last}.tmp"
      os.write.over(
        tmp,
        upickle.default.write(Checkpoint(offset, current)),
        perms = "rw-------",
        createFolders = true
      )
      os.move(tmp, file, replaceExisting = true, atomicMove = true)
      saved = offset
    }

  // The state as of the end of the log, now.
  def state: S = synchronized {
    fold(log.read(offset))
    save()
    current
  }

  // Appends what `changes` computes from the up-to-date state. If the log
  // grew meanwhile, `lost` is told what wasn't appended (to undo any side
  // effects of computing it), and it's computed again from the new state.
  // Returns what was appended.
  def update(
      changes: S => Seq[Event],
      lost: Seq[Event] => Unit = _ => ()
  ): Seq[Logged] = synchronized {
    var result: Option[Seq[Logged]] = None
    while (result.isEmpty) {
      fold(log.read(offset))
      val events = changes(current)
      result =
        if (events.isEmpty) Some(Nil) else log.append_if(offset, events)
      if (result.isEmpty) lost(events)
    }
    fold(result.get)
    save()
    result.get
  }
}

@upickle.implicits.allowUnknownKeys(false)
final case class Checkpoint[S](offset: Long, state: S)

object Checkpoint {
  given [S: ReadWriter]: ReadWriter[Checkpoint[S]] = upickle.default.macroRW
}
