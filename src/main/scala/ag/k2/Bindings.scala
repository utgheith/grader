package ag.k2

// ---- Event bindings for other languages
//
// Code, generated from event kinds' Schemas, that reads and writes those
// events -- envelopes included -- the way ag.k2 does in Scala.
//
// Other languages read only an event kind's current version: upgrades are
// Scala functions, so an older version reaches them as Unrecognized, as a
// newer one does.

enum Language {
  case Python
}

object Bindings {
  def apply(language: Language, kinds: Seq[EventKind[? <: Event]]): String =
    language match {
      case Language.Python => Python(kinds)
    }

  // where a language's binding goes, by default (under some directory)
  def file_of(language: Language): os.SubPath = language match {
    case Language.Python => os.sub / "python" / "events.py"
  }

  // All the records and enums the kinds use, each once. Throws if two
  // differ but share a name.
  private[k2] def decls(kinds: Seq[EventKind[? <: Event]]): Seq[Schema.Decl] = {
    val all = for {
      k <- kinds
      d <- k.schema.decls
    } yield d
    for ((name, ds) <- all.groupBy(_.name) if ds.distinct.size > 1)
      throw IllegalArgumentException(
        s"two different types are named $name; bindings need distinct names"
      )
    all.distinct
  }
}

// Python 3.11 or later, standard library only. Opening and sealing Sealed
// values also needs the `cryptography` package (imported only then).
private[k2] object Python {
  private val keywords = Set(
    "False",
    "None",
    "True",
    "and",
    "as",
    "assert",
    "async",
    "await",
    "break",
    "class",
    "continue",
    "def",
    "del",
    "elif",
    "else",
    "except",
    "finally",
    "for",
    "from",
    "global",
    "if",
    "import",
    "in",
    "is",
    "lambda",
    "nonlocal",
    "not",
    "or",
    "pass",
    "raise",
    "return",
    "try",
    "while",
    "with",
    "yield"
  )

  // names the runtime part defines, which records and enums can't have
  private val reserved = Set(
    "Event",
    "EventError",
    "Sealed",
    "KeyStore",
    "Stamped",
    "Unrecognized",
    "KINDS",
    "encode",
    "decode"
  )

  // a Python identifier for a Scala one
  private def ident(name: String): String =
    if (keywords.contains(name)) name + "_" else name

  // a Python string literal
  private def str(s: String): String = ujson.write(ujson.Str(s))

  // the runtime's description of a type
  private def desc(t: Schema.Type): String = t match {
    case Schema.Type.Str          => "(\"str\",)"
    case Schema.Type.Bool         => "(\"bool\",)"
    case Schema.Type.Int          => "(\"int\",)"
    case Schema.Type.Double       => "(\"double\",)"
    case Schema.Type.List(of)     => s"(\"list\", ${desc(of)})"
    case Schema.Type.Optional(of) => s"(\"opt\", ${desc(of)})"
    case Schema.Type.Dict(of)     => s"(\"dict\", ${desc(of)})"
    case Schema.Type.Record(n)    => s"(\"record\", ${str(n)})"
    case Schema.Type.Enum(n)      => s"(\"enum\", ${str(n)})"
    case Schema.Type.Sealed(of)   => s"(\"sealed\", ${desc(of)})"
  }

  // a type annotation
  private def hint(t: Schema.Type): String = t match {
    case Schema.Type.Str          => "str"
    case Schema.Type.Bool         => "bool"
    case Schema.Type.Int          => "int"
    case Schema.Type.Double       => "float"
    case Schema.Type.List(of)     => s"list[${hint(of)}]"
    case Schema.Type.Optional(of) => s"${hint(of)} | None"
    case Schema.Type.Dict(of)     => s"dict[str, ${hint(of)}]"
    case Schema.Type.Record(n)    => n
    case Schema.Type.Enum(n)      => n
    case Schema.Type.Sealed(of)   => s"Sealed[${hint(of)}]"
  }

  private def decl(d: Schema.Decl, kinds: Map[String, EventKind[?]]): String =
    d match {
      case Schema.Decl.Enum(name, cases) =>
        val members = cases.map(c => s"    ${ident(c)} = ${str(c)}")
        s"class $name(enum.Enum):\n${members.mkString("\n")}\n"
      case Schema.Decl.Record(name, fields) =>
        val kind = kinds.get(name)
        val base = if (kind.isDefined) "Event" else "_Record"
        val header = kind.toSeq.flatMap { k =>
          Seq(
            s"    TYPE: ClassVar[str] = ${str(k.name)}",
            s"    VERSION: ClassVar[int] = ${k.version}"
          )
        }
        val described = fields.map { f =>
          val default = f.default.fold("None")(v => str(ujson.write(v)))
          s"        (${str(f.name)}, ${str(ident(f.name))}, ${desc(f.tpe)}, $default),"
        }
        val attributes = fields.map { f =>
          val default = f.default.fold("")(v =>
            s" = field(default_factory=lambda: _read(${desc(f.tpe)}, json.loads(${str(ujson.write(v))})))"
          )
          s"    ${ident(f.name)}: ${hint(f.tpe)}$default"
        }
        (Seq(
          "@dataclass(frozen=True, kw_only=True)",
          s"class $name($base):"
        ) ++ header ++ Seq(
          "    _FIELDS: ClassVar[tuple] = ("
        ) ++ described ++ Seq("    )") ++ attributes).mkString("", "\n", "\n")
    }

  def apply(kinds: Seq[EventKind[? <: Event]]): String = {
    val decls = Bindings.decls(kinds)
    for (d <- decls if reserved.contains(d.name) || keywords.contains(d.name))
      throw IllegalArgumentException(
        s"a type named ${d.name}: the Python binding uses that name"
      )
    // each kind by its root's simple name
    val by_root =
      kinds.map(k => (k.schema.decls.head.name, k: EventKind[?])).toMap
    val kinds_table =
      kinds.map(k => s"    ${str(k.name)}: ${k.schema.decls.head.name},")
    Seq(
      runtime,
      "# ---- the events\n",
      decls.map(decl(_, by_root)).mkString("\n\n"),
      "\n# each event's class, by its type name in the stream",
      "KINDS: dict[str, type[Event]] = {",
      kinds_table.mkString("\n"),
      "}\n"
    ).mkString("\n")
  }

  private val runtime: String =
    """# Generated by grader (./run binding --language python). Do not edit:
      |# the events are defined in Scala, and this is regenerated from them.
      |#
      |# Events as ag.k2 writes them to a K2 stream: each one a JSON object
      |#   {"id", "type", "version", "as_of", "data"}
      |# encode() makes one, decode() reads one. Only each kind's current
      |# version is read; any other comes back as Unrecognized.
      |#
      |# Python 3.11 or later. Sealed values need the `cryptography` package to
      |# open or seal; nothing else does.
      |
      |from __future__ import annotations
      |
      |import base64
      |import datetime
      |import enum
      |import json
      |import re
      |import uuid
      |from dataclasses import dataclass, field
      |from pathlib import Path
      |from typing import Any, ClassVar, Generic, TypeVar
      |
      |
      |class EventError(Exception):
      |    "Something that isn't an event, or a value that doesn't fit its type."
      |
      |
      |# ---- reading and writing values, as upickle does
      |#
      |# A type is described by a tuple: ("str",), ("bool",), ("int",),
      |# ("double",), ("list", t), ("opt", t), ("dict", t), ("record", name),
      |# ("enum", name), ("sealed", t).
      |
      |def _fail(t, v):
      |    raise EventError(f"expected {t}, got {v!r}")
      |
      |
      |def _read(t, v):
      |    k = t[0]
      |    if k == "str":
      |        return v if isinstance(v, str) else _fail(t, v)
      |    if k == "bool":
      |        return v if isinstance(v, bool) else _fail(t, v)
      |    if k == "int":
      |        ok = isinstance(v, int) and not isinstance(v, bool)
      |        return v if ok and -2**31 <= v < 2**31 else _fail(t, v)
      |    if k == "double":
      |        ok = isinstance(v, (int, float)) and not isinstance(v, bool)
      |        return float(v) if ok else _fail(t, v)
      |    if k == "list":
      |        return [_read(t[1], x) for x in v] if isinstance(v, list) else _fail(t, v)
      |    if k == "opt":
      |        return None if v is None else _read(t[1], v)
      |    if k == "dict":
      |        if not isinstance(v, dict):
      |            _fail(t, v)
      |        return {key: _read(t[1], x) for key, x in v.items()}
      |    if k == "record":
      |        return globals()[t[1]]._from_json(v)
      |    if k == "enum":
      |        try:
      |            return globals()[t[1]](v)
      |        except ValueError:
      |            _fail(t, v)
      |    if k == "sealed":
      |        return Sealed._from_json(v, t[1])
      |    raise AssertionError(t)
      |
      |
      |def _write(t, v):
      |    k = t[0]
      |    if k in ("str", "bool", "int"):
      |        return v
      |    if k == "double":
      |        return float(v)
      |    if k == "list":
      |        return [_write(t[1], x) for x in v]
      |    if k == "opt":
      |        return None if v is None else _write(t[1], v)
      |    if k == "dict":
      |        return {key: _write(t[1], x) for key, x in v.items()}
      |    if k == "record":
      |        return v._to_json()
      |    if k == "enum":
      |        return v.value
      |    if k == "sealed":
      |        return v._to_json()
      |    raise AssertionError(t)
      |
      |
      |class _Record:
      |    # (JSON name, Python name, type, default as JSON text or None)
      |    _FIELDS: ClassVar[tuple] = ()
      |
      |    @classmethod
      |    def _from_json(cls, v):
      |        if not isinstance(v, dict):
      |            raise EventError(f"{cls.__name__}: expected an object, got {v!r}")
      |        args = {}
      |        for name, attr, t, default in cls._FIELDS:
      |            if name in v:
      |                args[attr] = _read(t, v[name])
      |            elif default is not None:
      |                args[attr] = _read(t, json.loads(default))
      |            else:
      |                raise EventError(f"{cls.__name__}: no {name!r}")
      |        return cls(**args)
      |
      |    def _to_json(self):
      |        return {name: _write(t, getattr(self, attr)) for name, attr, t, _ in self._FIELDS}
      |
      |
      |class Event(_Record):
      |    TYPE: ClassVar[str]
      |    VERSION: ClassVar[int]
      |
      |
      |# ---- sealed values
      |
      |class KeyStore:
      |    "A key file, as ag.k2's KeyStore writes it ({key id: base64 key}). Read only."
      |
      |    def __init__(self, file: str | Path):
      |        self.file = Path(file)
      |
      |    def get(self, key: str) -> bytes | None:
      |        if not self.file.exists():
      |            return None
      |        secret = json.loads(self.file.read_text()).get(key)
      |        return None if secret is None else base64.b64decode(secret)
      |
      |
      |T = TypeVar("T")
      |
      |
      |def _aesgcm(key: bytes):
      |    from cryptography.hazmat.primitives.ciphers.aead import AESGCM
      |    return AESGCM(key)
      |
      |
      |@dataclass(frozen=True)
      |class Sealed(Generic[T]):
      |    # A value sealed with a key: AES-256-GCM, a 12-byte nonce, and the key's
      |    # id as associated data. It opens only where the key store has its key.
      |    key: str
      |    nonce: str
      |    data: str
      |    _of: Any = field(default=None, compare=False, repr=False)
      |
      |    @staticmethod
      |    def _from_json(v, of):
      |        if not isinstance(v, dict) or not all(isinstance(v.get(k), str) for k in ("key", "nonce", "data")):
      |            raise EventError(f"expected a sealed value, got {v!r}")
      |        return Sealed(v["key"], v["nonce"], v["data"], of)
      |
      |    def _to_json(self):
      |        return {"key": self.key, "nonce": self.nonce, "data": self.data}
      |
      |    def open(self, keys: KeyStore) -> T | None:
      |        "The value, or None if the key store doesn't have the key."
      |        secret = keys.get(self.key)
      |        if secret is None:
      |            return None
      |        try:
      |            plain = _aesgcm(secret).decrypt(
      |                base64.b64decode(self.nonce, validate=True),
      |                base64.b64decode(self.data, validate=True),
      |                self.key.encode(),
      |            )
      |        except Exception as e:
      |            raise EventError(f"a value sealed with key {self.key} doesn't open with it: {e}") from e
      |        v = json.loads(plain)
      |        return v if self._of is None else _read(self._of, v)
      |
      |    @staticmethod
      |    def seal(value: T, of, key: str, keys: KeyStore) -> Sealed[T]:
      |        "`value` (of type `of`, a type description) sealed with `key`."
      |        import os
      |        secret = keys.get(key)
      |        if secret is None:
      |            raise EventError(f"no key {key} to seal with")
      |        nonce = os.urandom(12)
      |        data = _aesgcm(secret).encrypt(nonce, json.dumps(_write(of, value)).encode(), key.encode())
      |        return Sealed(key, base64.b64encode(nonce).decode(), base64.b64encode(data).decode(), of)
      |
      |
      |# ---- envelopes
      |
      |@dataclass(frozen=True)
      |class Unrecognized:
      |    "An event of a type or version this binding doesn't read."
      |    type: str
      |    version: int
      |    data: Any
      |
      |
      |@dataclass(frozen=True)
      |class Stamped:
      |    "An event read from the stream: its id, when it was made, and when K2 took it in."
      |    id: str
      |    as_of: datetime.datetime
      |    as_at: datetime.datetime | None
      |    event: Event | Unrecognized
      |
      |
      |def _instant(s: str) -> datetime.datetime:
      |    # Java writes up to 9 digits of a second; Python reads up to 6
      |    m = re.fullmatch(r"(.*T\d\d:\d\d:\d\d)(?:\.(\d+))?Z", s)
      |    if m is None:
      |        raise EventError(f"not an instant: {s!r}")
      |    micros = (m.group(2) or "").ljust(6, "0")[:6]
      |    return datetime.datetime.fromisoformat(f"{m.group(1)}.{micros}+00:00")
      |
      |
      |def _iso(t: datetime.datetime) -> str:
      |    return t.astimezone(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")
      |
      |
      |def encode(event: Event, *, id: str | None = None, as_of: datetime.datetime | None = None) -> dict:
      |    "`event` in its stream form (made now, with a fresh id, unless given)."
      |    return {
      |        "id": id or str(uuid.uuid4()),
      |        "type": event.TYPE,
      |        "version": event.VERSION,
      |        "as_of": _iso(as_of or datetime.datetime.now(datetime.timezone.utc)),
      |        "data": event._to_json(),
      |    }
      |
      |
      |def decode(envelope: Any, as_at: datetime.datetime | None = None) -> Stamped:
      |    "An event in its stream form; `as_at`: when K2 took it in (its record's timestamp)."
      |    if not isinstance(envelope, dict):
      |        raise EventError("not an event")
      |    for k in ("id", "type", "version", "as_of", "data"):
      |        if k not in envelope:
      |            raise EventError(f"event without {k!r}")
      |    cls = KINDS.get(envelope["type"])
      |    if cls is None or envelope["version"] != cls.VERSION:
      |        event = Unrecognized(envelope["type"], envelope["version"], envelope["data"])
      |    else:
      |        event = cls._from_json(envelope["data"])
      |    return Stamped(envelope["id"], _instant(envelope["as_of"]), as_at, event)
      |
      |""".stripMargin
}
