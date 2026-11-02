package ag.k2

import scala.unsafeExceptions.canThrowAny
import munit.FunSuite
import upickle.default.ReadWriter

import java.time.Instant

// a test event using every portable type
enum Outcome derives ReadWriter {
  case Pass, Fail, Timeout
}

final case class Detail(seconds: Double, output: Option[String] = None)
    derives ReadWriter

final case class Graded(
    test: String,
    outcome: Outcome,
    tries: Seq[Outcome],
    detail: Detail,
    by_phase: Map[String, Int],
    student: Sealed[String],
    note: Option[String] = None,
    weight: Double = 1.0,
    pass: Boolean = false // a Python keyword
) extends Event
    derives ReadWriter {
  def kind: EventKind[Graded] = Graded.kind
}

object Graded {
  val kind: EventKind[Graded] = EventKind[Graded]("graded")
}

class BindingTests extends FunSuite {
  private val kinds = Seq(Enrolled.kind, Ping.kind, Graded.kind)

  // the Python to test with: $K2_PYTHON (e.g. a venv's), or python3
  private val python: String =
    Option(System.getenv("K2_PYTHON")).getOrElse("python3")

  test("a kind's schema is derived from its type") {
    import Schema.*
    assertEquals(
      Graded.kind.schema,
      Schema(
        "ag.k2.Graded",
        Seq(
          Decl.Record(
            "Graded",
            Seq(
              Field("test", Type.Str, None),
              Field("outcome", Type.Enum("Outcome"), None),
              Field("tries", Type.List(Type.Enum("Outcome")), None),
              Field("detail", Type.Record("Detail"), None),
              Field("by_phase", Type.Dict(Type.Int), None),
              Field("student", Type.Sealed(Type.Str), None),
              Field("note", Type.Optional(Type.Str), Some(ujson.Null)),
              Field("weight", Type.Double, Some(ujson.Num(1.0))),
              Field("pass", Type.Bool, Some(ujson.False))
            )
          ),
          Decl.Enum("Outcome", Seq("Pass", "Fail", "Timeout")),
          Decl.Record(
            "Detail",
            Seq(
              Field("seconds", Type.Double, None),
              Field("output", Type.Optional(Type.Str), Some(ujson.Null))
            )
          )
        )
      )
    )
  }

  private def rejected(event: String, because: String): Unit = {
    val errors = Snippets.errors(s"""
      $event
      object Bad { val kind = EventKind[Bad]("bad") }
    """)
    assert(errors.exists(_.contains(because)), errors.mkString("\n"))
  }

  private def bad(fields: String): String =
    s"""final case class Bad($fields) extends Event { def kind = Bad.kind }
        object Bad0 { given upickle.default.ReadWriter[Bad] = ??? }
        import Bad0.given"""

  test("a type outside the portable subset doesn't compile") {
    rejected(bad("n: Long"), "upickle writes a big Long as a string")
    rejected(bad("m: Map[Int, String]"), "only maps with String keys")
    rejected(bad("s: Set[String]"), "not in the portable subset")
  }

  // Events written in Scala, read and written again in Python (with the
  // generated binding), and read back in Scala: they should be the same.
  test("events go from Scala to Python and back unchanged") {
    val python = os.proc("python3", "--version").call(check = false)
    assume(python.exitCode == 0, "no python3")

    val dir = os.temp.dir()
    os.write(dir / "events.py", Bindings(Language.Python, kinds))
    os.write(
      dir / "echo.py",
      """import importlib.util, json, sys
        |spec = importlib.util.spec_from_file_location("events", sys.argv[1])
        |events = importlib.util.module_from_spec(spec)
        |sys.modules["events"] = events
        |spec.loader.exec_module(events)
        |for line in sys.stdin:
        |    s = events.decode(json.loads(line))
        |    if isinstance(s.event, events.Unrecognized):
        |        print(json.dumps({"unrecognized": s.event.type}))
        |    else:
        |        print(json.dumps(events.encode(s.event, id=s.id, as_of=s.as_of)))
        |""".stripMargin
    )

    // (Python keeps time to the microsecond)
    val as_of = Instant.parse("2026-11-01T12:34:56.123456Z").nn
    val student = Sealed[String](KeyId("k1"), "bm9uY2U=", "ZGF0YQ==")
    val events: Seq[Event] = Seq(
      Ping(7),
      Enrolled("cs439", student),
      Graded(
        "t1",
        Outcome.Fail,
        Seq(Outcome.Pass, Outcome.Timeout),
        Detail(1.5),
        Map("1" -> 3, "2" -> 0),
        student
      ),
      Graded(
        "t2",
        Outcome.Pass,
        Nil,
        Detail(0.25, Some("ok")),
        Map(),
        student,
        note = Some("late"),
        weight = 2.5,
        pass = true
      )
    )
    val ids = events.map(_ => EventId.fresh())
    val input = events
      .zip(ids)
      .map((e, id) => ujson.write(Envelope.encode(id, as_of, e)))
    // a version this binding doesn't know
    val newer = ujson.write(
      Envelope.encode(EventId.fresh(), as_of, Ping(1)).tap(_("version") = 2)
    )

    val out = os
      .proc("python3", "-I", dir / "echo.py", dir / "events.py")
      .call(stdin = (input :+ newer).mkString("\n"))
      .out
      .lines()
    assertEquals(out.size, events.size + 1)

    val kinds_ = EventKinds(kinds*)
    val back = out
      .take(events.size)
      .map(l => Envelope.decode(kinds_, ujson.read(l), as_of))
    assert(back.map(_.event) == events)
    assertEquals(back.map(_.id), ids)
    assertEquals(back.map(_.as_of), events.map(_ => as_of))
    assertEquals(out.last, """{"unrecognized": "ping"}""")
  }

  // Sealed values opened in Python, and sealed there to open in Scala: the
  // same key file, both ways. Needs `cryptography` in that Python.
  test("sealed values go both ways between Scala and Python") {
    val has_crypto =
      os.proc(python, "-I", "-c", "import cryptography").call(check = false, stderr = os.Pipe)
    assume(has_crypto.exitCode == 0, s"no cryptography in $python (set K2_PYTHON)")

    val dir = os.temp.dir()
    given keys: KeyStore = KeyStore(dir / "keys.json")
    val k = KeyId.fresh()
    keys.create(k)
    val shredded = KeyId.fresh() // (never in this store)

    os.write(dir / "events.py", Bindings(Language.Python, kinds))
    os.write(
      dir / "reseal.py",
      """import dataclasses, importlib.util, json, sys
        |spec = importlib.util.spec_from_file_location("events", sys.argv[1])
        |events = importlib.util.module_from_spec(spec)
        |sys.modules["events"] = events
        |spec.loader.exec_module(events)
        |keys = events.KeyStore(sys.argv[2])
        |for line in sys.stdin:
        |    s = events.decode(json.loads(line))
        |    g = s.event
        |    opened = g.student.open(keys)
        |    try:
        |        dataclasses.replace(g.student, data="AAAA" + g.student.data[4:]).open(keys)
        |        tampered = "opened"
        |    except events.EventError:
        |        tampered = "refused"
        |    missing = dataclasses.replace(g.student, key=sys.argv[3]).open(keys)
        |    resealed = events.Sealed.seal(opened.upper(), ("str",), g.student.key, keys)
        |    print(json.dumps({
        |        "opened": opened, "tampered": tampered, "missing": missing,
        |        "event": events.encode(dataclasses.replace(g, student=resealed), id=s.id),
        |    }))
        |""".stripMargin
    )

    val event = Graded("t1", Outcome.Pass, Nil, Detail(1.0), Map(), Sealed("ab1234", k))
    val input = ujson.write(Envelope.encode(EventId.fresh(), Instant.now().nn, event))
    val out = ujson.read(
      os.proc(python, "-I", dir / "reseal.py", dir / "events.py", dir / "keys.json", shredded.value)
        .call(stdin = input)
        .out
        .text()
    )
    assertEquals(out("opened").str, "ab1234")
    assertEquals(out("tampered").str, "refused")
    assertEquals(out("missing"), ujson.Null)

    given Reveal = Reveal("tests")
    val back = Envelope.decode(EventKinds(kinds*), out("event"), Instant.now().nn).event
    back match {
      case g: Graded =>
        assertEquals(g.student.open.map(_.reveal), Some("AB1234"))
        assertEquals(g.copy(student = event.student), event)
      case other => fail(s"not a Graded: ${other.toString}")
    }
  }

  test("the default python binding's name") {
    assertEquals(Bindings.file_of(Language.Python), os.sub / "python" / "events.py")
  }
}

extension [A](a: A)
  private def tap(f: A => Unit): A = { f(a); a }
