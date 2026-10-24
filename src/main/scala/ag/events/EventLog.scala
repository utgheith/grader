package ag.events

import upickle.default.ReadWriter

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.{CREATE, READ, WRITE}
import java.time.Instant

// Something that happened. Events are facts: once in the log, never changed.
trait Event {
  def kind: EventKind[? <: Event]
}

// A kind of event: its name in the log, and how its stored form has evolved.
// `upgrades(i)` turns version i+1's stored data into version i+2's, so the
// current version is upgrades.size + 1. Old events stay readable forever:
// a change to an event's fields adds an upgrade, it never breaks old ones.
final class EventKind[E <: Event](
    val name: String,
    val upgrades: Seq[ujson.Value => ujson.Value] = Nil
)(using rw: ReadWriter[E]) {
  val version: Int = upgrades.size + 1

  def encode(e: Event): ujson.Value =
    upickle.default.writeJs(e.asInstanceOf[E])

  def decode(stored_version: Int, data: ujson.Value): E = {
    if (stored_version < 1 || stored_version > version)
      throw EventLogException(
        s"$name version $stored_version can't be read: this code knows versions 1 to $version"
      )
    val current =
      upgrades.drop(stored_version - 1).foldLeft(data)((d, up) => up(d))
    upickle.default.read[E](current)
  }
}

// The event kinds a log may contain, by name.
final class EventKinds(kinds: EventKind[? <: Event]*) {
  private val by_name: Map[String, EventKind[? <: Event]] = {
    val dups =
      kinds.groupBy(_.name).collect { case (n, ks) if ks.size > 1 => n }
    if (dups.nonEmpty)
      throw EventLogException(
        s"event kinds named twice: ${dups.mkString(", ")}"
      )
    kinds.map(k => k.name -> k).toMap
  }

  def apply(name: String): EventKind[? <: Event] =
    by_name.getOrElse(
      name,
      throw EventLogException(s"unknown event kind '$name'")
    )
}

final class EventLogException(message: String) extends RuntimeException(message)

// An event as recorded: where it is in the log, and when it was appended.
final case class Logged(offset: Long, at: Instant, event: Event)

// Everything that happens, in one total order. Only ever appended to; a
// consistent state of the system is "everything up to offset n".
trait EventLog {
  // Appends all of `events`, or none, at consecutive offsets.
  def append(events: Seq[Event]): Seq[Logged]

  // Appends `events` only if the log still ends at `expected_end` -- i.e.
  // nothing was appended since the caller last looked -- and None otherwise
  // (then re-read, recompute, and try again). For writers that must not
  // record the same thing twice, such as watchers of the outside world.
  def append_if(expected_end: Long, events: Seq[Event]): Option[Seq[Logged]]

  // Every event from `from` (an offset) on, in order.
  def read(from: Long = 0): Iterator[Logged]

  // The offset the next appended event will get.
  def end: Long
}

// How a logged event is stored, whatever the log:
//   {"offset":0,"at":"2026-10-24T17:05:00Z","kind":"...","v":1,"data":{...}}
object Records {
  // ISO-8601 (e.g. 2026-10-24T17:05:00Z), for people reading the log
  private given ReadWriter[Instant] =
    upickle.default.readwriter[String].bimap(_.toString, Instant.parse(_).nn)

  def encode(l: Logged): ujson.Value =
    ujson.Obj(
      "offset" -> ujson.Num(l.offset.toDouble),
      "at" -> upickle.default.writeJs(l.at),
      "kind" -> l.event.kind.name,
      "v" -> l.event.kind.version,
      "data" -> l.event.kind.encode(l.event)
    )

  def decode(kinds: EventKinds, j: ujson.Value): Logged =
    Logged(
      offset = j("offset").num.toLong,
      at = upickle.default.read[Instant](j("at")),
      event = kinds(j("kind").str).decode(j("v").num.toInt, j("data"))
    )
}

// A log in one file, one JSON object per line:
//   {"offset":0,"at":"...","kind":"...","v":1,"data":{...}}
// Appends hold the operating system's lock on the file, so several
// processes (and threads) can share it, and are flushed to disk before they
// count. A line left half-written by a crash was never appended: it's
// ignored when reading and cut off by the next append.
final class FileEventLog(val file: os.Path, kinds: EventKinds)
    extends EventLog {
  private def line(l: Logged): String = ujson.write(Records.encode(l))
  private def parse(text: String): Logged =
    Records.decode(kinds, ujson.read(text))

  // the complete lines (each ending in a newline) of the file
  private def complete_lines(): Iterator[String] =
    if (!os.exists(file)) Iterator.empty
    else {
      val text = os.read(file)
      val complete = text.substring(0, text.lastIndexOf('\n') + 1).nn
      complete.linesIterator.filter(_.nonEmpty)
    }

  def read(from: Long = 0): Iterator[Logged] =
    complete_lines().drop(from.toInt).map(parse)

  def end: Long = complete_lines().size.toLong

  // The operating system's lock keeps other processes out, but within one
  // JVM a second lock on the same file fails instead of waiting -- so
  // threads in this process take turns first.
  def append(events: Seq[Event]): Seq[Logged] =
    FileEventLog.in_process_lock(file).synchronized {
      append_locked(None, events).get
    }

  def append_if(expected_end: Long, events: Seq[Event]): Option[Seq[Logged]] =
    FileEventLog.in_process_lock(file).synchronized {
      append_locked(Some(expected_end), events)
    }

  private def append_locked(
      expected_end: Option[Long],
      events: Seq[Event]
  ): Option[Seq[Logged]] = {
    os.makeDir.all(file / os.up)
    val channel = FileChannel.open(file.toNIO, CREATE, READ, WRITE).nn
    try {
      val lock = channel.lock().nn
      try {
        // drop a line a crash left half-written
        val size = channel.size()
        val text =
          if (size == 0) ""
          else {
            val buf = ByteBuffer.allocate(size.toInt).nn
            val _ = channel.read(buf, 0)
            String(buf.array().nn, StandardCharsets.UTF_8)
          }
        val complete = text.lastIndexOf('\n') + 1
        if (complete < text.length) {
          val _ = channel.truncate(
            text
              .substring(0, complete)
              .nn
              .getBytes(StandardCharsets.UTF_8)
              .nn
              .length
              .toLong
          )
        }
        val first = text.substring(0, complete).nn.count(_ == '\n').toLong
        if (expected_end.exists(_ != first)) None
        else {
          val now = Instant.now().nn
          val logged =
            events.zipWithIndex.map((e, i) => Logged(first + i, now, e))
          val bytes = logged
            .map(line(_) + "\n")
            .mkString
            .getBytes(StandardCharsets.UTF_8)
            .nn
          val _ = channel.write(ByteBuffer.wrap(bytes), channel.size())
          channel.force(true)
          Some(logged)
        }
      } finally lock.release()
    } finally channel.close()
  }
}

object FileEventLog {
  private val locks = java.util.concurrent.ConcurrentHashMap[os.Path, Object]()
  private def in_process_lock(file: os.Path): Object =
    locks.computeIfAbsent(file, _ => Object()).nn
}
