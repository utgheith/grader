package ag.k2

import language.experimental.captureChecking
import language.experimental.saferExceptions

import upickle.default.ReadWriter

import java.time.Instant
import java.util.UUID

// ---- Events
//
// An event is a fact: something that happened, never changed once written.
// Natively it's a Scala object (an Event, usually a case class); in the
// stream it's a JSON object that names its own type:
//
//   {"id": "<uuid>", "type": "<name>", "version": 1, "as_of": "<ISO-8601>",
//    "data": {<the event's fields>}}
//
// It has two times: as_of, when its source created it, by the source's
// clock; and as_at, when K2 took it in, by K2's clock (K2 stamps every record
// on arrival, and that stamp can't be set by producers). Sources can be
// offline for a while, so as_at can be much later than as_of.
//
// Events aren't sealed: anyone who can read the stream can read them. Put
// private members in a Sealed[T].

// Pure: an event can't hold a capability, and so can't hold plaintext (a
// Plain captures its KeyStore).
trait Event extends caps.Pure {
  def kind: EventKind[? <: Event]
}

// Identifies one event, so that consumers can drop the duplicates that
// retries and at-least-once delivery produce.
final case class EventId(value: String) {
  override def toString: String = value
}

object EventId {
  def fresh(): EventId = EventId(UUID.randomUUID().toString)

  // (written out: upickle's derivation doesn't work under capture checking)
  given ReadWriter[EventId] =
    upickle.default.readwriter[String].bimap(_.value, EventId(_))
}

// A kind of event: its type name in the stream, and how its stored form has
// evolved. `upgrades(i)` turns version i+1's data into version i+2's, so the
// current version is upgrades.size + 1. Old events stay readable forever: a
// change to an event's fields adds an upgrade, it never breaks old ones.
// Upgrades are pure functions (`->`, checked): an old event reads the same
// wherever and whenever it's read.
//
// Its current version's Schema is derived at compile time, from E (so E has
// to be portable; see Schema), for bindings in other languages.
final class EventKind[E <: Event](
    val name: String,
    val upgrades: Seq[ujson.Value -> ujson.Value] = Nil
)(using rw: ReadWriter[E], described: Schema.Of[E]) {
  val version: Int = upgrades.size + 1
  val schema: Schema = described.schema

  def encode(e: Event): ujson.Value =
    upickle.default.writeJs(e.asInstanceOf[E])

  // `data` as stored at `stored_version` (1 to version), upgraded and read
  def decode(stored_version: Int, data: ujson.Value): E = {
    val current =
      upgrades.drop(stored_version - 1).foldLeft(data)((d, up) => up(d))
    upickle.default.read[E](current)
  }
}

// The event kinds a consumer understands, by name.
final class EventKinds(val all: EventKind[? <: Event]*) {
  private val by_name: Map[String, EventKind[? <: Event]] = {
    val dups =
      all.groupBy(_.name).collect { case (n, ks) if ks.size > 1 => n }
    require(dups.isEmpty, s"event kinds named twice: ${dups.mkString(", ")}")
    all.map(k => (k.name, k)).toMap
  }

  def get(name: String): Option[EventKind[? <: Event]] = by_name.get(name)
}

// Checked: an event that can't go into the stream (too big), or a record
// in it that isn't an event.
final class EventException(message: String) extends Exception(message)

// An event this consumer can't read: its type isn't one of its kinds, or
// it's a newer version than its kind knows. Not an error -- streams are
// shared, and producers may be newer than consumers -- so it's passed on
// as it is, for the consumer to skip or keep.
final case class Unrecognized(
    type_name: String,
    version: Int,
    data: ujson.Value
)

// An event as a consumer receives it.
final case class Stamped(
    id: EventId,
    as_of: Instant,
    as_at: Instant,
    event: Event | Unrecognized
)

// The stream's form of an event (without as_at, which K2 adds).
object Envelope {
  def encode(id: EventId, as_of: Instant, e: Event): ujson.Obj =
    ujson.Obj(
      "id" -> id.value,
      "type" -> e.kind.name,
      "version" -> e.kind.version,
      "as_of" -> as_of.toString,
      "data" -> e.kind.encode(e)
    )

  // Throws if `json` isn't an envelope at all, or an event of a known kind
  // and version doesn't read: those are bugs, not something to skip.
  def decode(
      kinds: EventKinds,
      json: ujson.Value,
      as_at: Instant
  ): Stamped throws EventException = {
    val obj = json.objOpt.getOrElse(throw EventException("not an event"))
    def field(name: String): ujson.Value =
      obj.getOrElse(name, throw EventException(s"event without '$name'"))
    val type_name = field("type").str
    val version = field("version").num.toInt
    val data = field("data")
    val event: Event | Unrecognized = kinds.get(type_name) match {
      case Some(kind) if version >= 1 && version <= kind.version =>
        kind.decode(version, data)
      case _ => Unrecognized(type_name, version, data)
    }
    Stamped(
      EventId(field("id").str),
      Instant.parse(field("as_of").str).nn,
      as_at,
      event
    )
  }
}
