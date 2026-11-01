package ag.k2

import scala.unsafeExceptions.canThrowAny
import munit.FunSuite
import upickle.default.ReadWriter

import java.nio.charset.StandardCharsets
import java.time.Instant

// a test event: a public field and a private one
@upickle.implicits.allowUnknownKeys(false)
final case class Enrolled(course: String, csid: Sealed[String]) extends Event
    derives ReadWriter {
  def kind: EventKind[Enrolled] = Enrolled.kind
}

object Enrolled {
  // version 1 called the course "class"
  val kind: EventKind[Enrolled] = EventKind[Enrolled](
    "enrolled",
    Seq(d => ujson.Obj("course" -> d("class"), "csid" -> d("csid")))
  )
}

@upickle.implicits.allowUnknownKeys(false)
final case class Ping(n: Int) extends Event derives ReadWriter {
  def kind: EventKind[Ping] = Ping.kind
}

object Ping {
  val kind: EventKind[Ping] = EventKind[Ping]("ping")
}

class K2Tests extends FunSuite {
  // the tests look at what opens
  private given Reveal = Reveal("tests")
  private val kinds = EventKinds(Enrolled.kind, Ping.kind)

  private val tmp = FunFixture[os.Path](
    setup = _ => os.temp.dir(prefix = "k2-tests"),
    teardown = d => os.remove.all(d)
  )

  private val k2 = FunFixture[(os.Path, FakeK2)](
    setup = _ => (os.temp.dir(prefix = "k2-tests"), FakeK2()),
    teardown = (d, f) => { f.stop(); os.remove.all(d) }
  )

  private def client(f: FakeK2) = K2Client("stream", None, f.url)

  private def drain(c: Consumer): Seq[Stamped] = {
    val got = Seq.newBuilder[Stamped]
    while (c.poll(max = 3)(got ++= _) > 0) ()
    got.result()
  }

  // ---- Sealed

  tmp.test("a sealed value opens with its key, and not once it's shredded") {
    d =>
      given keys: KeyStore = KeyStore(d / "keys.json")
      val k = KeyId.fresh()
      keys.create(k)
      val s = Sealed("ab1234", k)
      assertEquals(s.open.map(_.reveal), Some("ab1234"))
      assert(!upickle.default.write(s).contains("ab1234"))
      keys.shred(k)
      assertEquals(s.open.map(_.reveal), None)
  }

  tmp.test("a missing key isn't an error; a wrong key or tampering is") { d =>
    val producer = KeyStore(d / "producer.json")
    val consumer = KeyStore(d / "consumer.json")
    val k = KeyId.fresh()
    producer.create(k)
    val s = Sealed("ab1234", k)(using summon, producer)
    assertEquals(s.open(using consumer).map(_.reveal), None)
    consumer.add(k, producer.secret_of(k).get)
    assertEquals(s.open(using consumer).map(_.reveal), Some("ab1234"))

    val other = KeyStore(d / "other.json")
    other.create(k) // same id, another secret
    val _ = intercept[SealedException](s.open(using other))
    val tampered = s.copy[String](data =
      Crypto.b64(Crypto.unb64(s.data).map(b => (b ^ 1).toByte))
    )
    val _ = intercept[SealedException](tampered.open(using consumer))
    val not_base64 = s.copy[String](nonce = "#" + s.nonce.drop(1))
    val _ = intercept[SealedException](not_base64.open(using consumer))
    // the stored form doesn't say what type it holds
    val not_an_int = Sealed[Int](s.key, s.nonce, s.data)
    val _ = intercept[SealedException](not_an_int.open(using consumer))
    val _ = intercept[SealedException](
      Sealed("x", KeyId.fresh())(using summon, producer)
    )
  }

  tmp.test("adding a key twice is fine, with another secret it isn't") { d =>
    val a = KeyStore(d / "a.json")
    val b = KeyStore(d / "b.json")
    val k = KeyId("student-1")
    a.create(k)
    b.add(k, a.secret_of(k).get)
    b.add(k, a.secret_of(k).get)
    val _ = intercept[SealedException](b.add(k, Crypto.b64(AesGcm.fresh().bytes.copy)))
    val _ = intercept[SealedException](a.create(k))
  }

  // ---- Events

  tmp.test(
    "an event's stream form names its type and version; sealed members stay sealed"
  ) { d =>
    given keys: KeyStore = KeyStore(d / "keys.json")
    val k = KeyId.fresh()
    keys.create(k)
    val id = EventId.fresh()
    val as_of = Instant.parse("2026-10-31T12:00:00Z").nn
    val e = Enrolled("cs439", Sealed("ab1234", k))
    val json = Envelope.encode(id, as_of, e)
    assertEquals(json("type").str, "enrolled")
    assertEquals(json("version").num, 2.0)
    assertEquals(json("as_of").str, "2026-10-31T12:00:00Z")
    assert(!ujson.write(json).contains("ab1234"))

    val as_at = as_of.plusSeconds(60).nn
    val back = Envelope.decode(kinds, json, as_at)
    assertEquals(back, Stamped(id, as_of, as_at, e))
    back.event match {
      case Enrolled(_, csid) =>
        assertEquals(csid.open.map(_.reveal), Some("ab1234"))
      case other => fail(s"got ${other.toString}")
    }
  }

  test(
    "old versions are upgraded; unknown types and newer versions pass through"
  ) {
    val at = Instant.EPOCH.nn
    def env(t: String, v: Int, data: ujson.Value) = ujson.Obj(
      "id" -> "x",
      "type" -> t,
      "version" -> v,
      "as_of" -> at.toString,
      "data" -> data
    )
    val boxed = upickle.default.writeJs(Sealed[String](KeyId("k"), "n", "d"))
    def decoded(t: String, v: Int, data: ujson.Value): Event | Unrecognized =
      Envelope.decode(kinds, env(t, v, data), at).event
    assert(
      decoded("enrolled", 1, ujson.Obj("class" -> "cs439", "csid" -> boxed)) ==
        Enrolled("cs439", Sealed[String](KeyId("k"), "n", "d"))
    )
    assert(
      decoded("graded", 1, ujson.Obj()) == Unrecognized(
        "graded",
        1,
        ujson.Obj()
      )
    )
    assert(
      decoded("ping", 2, ujson.Obj("n" -> 1)) == Unrecognized(
        "ping",
        2,
        ujson.Obj("n" -> 1)
      )
    )
  }

  // ---- Outbox and consumer

  k2.test("events go from an outbox to K2 to a consumer, with both times") {
    (d, f) =>
      val sent_at = Instant.parse("2026-10-31T12:00:00Z").nn
      val k2_at = sent_at.plusSeconds(3600).nn
      f.clock = () => k2_at
      val out = Outbox(d / "out")
      val ids = out.append(Seq(Ping(1), Ping(2)), sent_at)
      assertEquals(out.pending, 2)
      assertEquals(out.sync(client(f)), SyncResult(2, 0, None))
      assertEquals(out.pending, 0)
      assertEquals(
        f.records.map(_.headers("type")).toList,
        List("ping", "ping")
      )
      assertEquals(
        f.records.map(_.headers("id")).toList,
        ids.map(_.value).toList
      )

      val got = drain(Consumer(client(f), "sub", "w1", kinds, d / "in"))
      assertEquals(
        got.map(s => (s.id, s.as_of, s.as_at, s.event)).toList,
        List(
          (ids(0), sent_at, k2_at, Ping(1)),
          (ids(1), sent_at, k2_at, Ping(2))
        )
      )
  }

  k2.test(
    "offline, events wait in the outbox; a later sync sends them, in order"
  ) { (d, f) =>
    val out = Outbox(d / "out")
    val _ = out.append(Seq(Ping(1)))
    val down = K2Client("stream", None, "http://127.0.0.1:9") // nothing listens
    val r = out.sync(down)
    assertEquals((r.sent, r.pending), (0, 1))
    assert(r.stopped.isDefined)
    val _ = out.append(Seq(Ping(2)))

    f.fail_next_produce = Some((503, 10211, true, false))
    val r2 = out.sync(client(f))
    assertEquals((r2.sent, r2.pending), (0, 2))
    assert(r2.stopped.isDefined)

    assertEquals(out.sync(client(f)), SyncResult(2, 0, None))
    assertEquals(out.sync(client(f)), SyncResult(0, 0, None))
    assertEquals(
      drain(Consumer(client(f), "sub", "w", kinds, d / "in"))
        .map(_.event)
        .toList,
      List(Ping(1), Ping(2))
    )
  }

  k2.test(
    "a batch K2 may have stored is sent again; the consumer drops the copies"
  ) { (d, f) =>
    val out = Outbox(d / "out")
    val _ = out.append(Seq(Ping(1), Ping(2)))
    f.fail_next_produce = Some((503, 10212, false, true)) // stored, but failed
    assertEquals(out.sync(client(f)).sent, 0)
    assertEquals(out.sync(client(f)).sent, 2)
    assertEquals(f.records.size, 4)
    assertEquals(
      drain(Consumer(client(f), "sub", "w", kinds, d / "in"))
        .map(_.event)
        .toList,
      List(Ping(1), Ping(2))
    )
  }

  k2.test("an error a retry won't fix is thrown") { (d, f) =>
    f.token = Some("secret")
    val out = Outbox(d / "out")
    val _ = out.append(Seq(Ping(1)))
    val e = intercept[K2Exception](out.sync(client(f)))
    assertEquals(e.code, 10209)
    assertEquals(out.pending, 1)
    assertEquals(out.sync(K2Client("stream", Some("secret"), f.url)).sent, 1)
  }

  k2.test(
    "big outboxes go in several batches, each within K2's request limit"
  ) { (d, f) =>
    val out = Outbox(d / "out")
    val big = "x" * 600_000
    val _ = out.append((1 to 12).map(i => Ping(i)))
    // a Ping is tiny; make some big ones out of a sealed blob instead
    given keys: KeyStore = KeyStore(d / "keys.json")
    val k = KeyId.fresh()
    keys.create(k)
    val _ = out.append((1 to 10).map(i => Enrolled(s"c$i", Sealed(big, k))))
    assertEquals(out.sync(client(f)), SyncResult(22, 0, None))
    assert(
      f.produce_calls >= 2,
      f.produce_calls.toString
    ) // (the fake refuses > 5 MB)
    assertEquals(
      drain(Consumer(client(f), "sub", "w", kinds, d / "in")).size,
      22
    )
  }

  tmp.test("an event too big for K2 is refused, and nothing is recorded") { d =>
    given keys: KeyStore = KeyStore(d / "keys.json")
    val k = KeyId.fresh()
    keys.create(k)
    val out = Outbox(d / "out")
    val _ = intercept[EventException](
      out.append(Seq(Ping(1), Enrolled("c", Sealed("x" * 1_000_000, k))))
    )
    assertEquals(out.pending, 0)
  }

  k2.test("a line half-written by a crash is dropped; the rest survive") {
    (d, f) =>
      val out = Outbox(d / "out")
      val _ = out.append(Seq(Ping(1)))
      os.write.append(d / "out" / "outbox.jsonl", """{"id":"torn","ty""")
      val _ = out.append(Seq(Ping(2)))
      assertEquals(out.sync(client(f)).sent, 2)
      assertEquals(
        drain(Consumer(client(f), "sub", "w", kinds, d / "in"))
          .map(_.event)
          .toList,
        List(Ping(1), Ping(2))
      )
  }

  k2.test("compaction drops what K2 has and keeps the rest to send") { (d, f) =>
    val out = Outbox(d / "out")
    val _ = out.append(Seq(Ping(1), Ping(2)))
    assertEquals(out.sync(client(f)).sent, 2)
    val _ = out.append(Seq(Ping(3)))
    out.compact()
    assertEquals(os.read.lines(d / "out" / "outbox.jsonl").size, 1)
    assertEquals(out.pending, 1)
    assertEquals(out.sync(client(f)).sent, 1)
    out.compact()
    assertEquals(out.pending, 0)
    assertEquals(out.sync(client(f)).sent, 0)
    assertEquals(f.records.size, 3)
  }

  k2.test("a failed handler gives the batch back; it comes again") { (d, f) =>
    val out = Outbox(d / "out")
    val _ = out.append(Seq(Ping(1), Ping(2)))
    val _ = out.sync(client(f))
    val c = Consumer(client(f), "sub", "w", kinds, d / "in")
    val _ = intercept[IllegalStateException](
      c.poll()(_ => throw IllegalStateException("boom"))
    )
    assertEquals(drain(c).map(_.event).toList, List(Ping(1), Ping(2)))
    // and a restarted consumer remembers what it handled
    assertEquals(drain(Consumer(client(f), "sub", "w", kinds, d / "in")), Nil)
  }

  k2.test("records that aren't events are skipped") { (d, f) =>
    client(f).produce(
      Seq(Record("hello".getBytes(StandardCharsets.UTF_8), Map()))
    )
    val out = Outbox(d / "out")
    val _ = out.append(Seq(Ping(1)))
    val _ = out.sync(client(f))
    var skipped = 0
    val c = Consumer(
      client(f),
      "sub",
      "w",
      kinds,
      d / "in",
      on_malformed = (_, _) => skipped += 1
    )
    assertEquals(drain(c).map(_.event).toList, List(Ping(1)))
    assertEquals(skipped, 1)
  }

  k2.test("a new subscription can start after the newest record") { (d, f) =>
    val out = Outbox(d / "out")
    val _ = out.append(Seq(Ping(1)))
    val _ = out.sync(client(f))
    val c =
      Consumer(client(f), "late", "w", kinds, d / "in", from_earliest = false)
    assertEquals(drain(c), Nil)
    val _ = out.append(Seq(Ping(2)))
    val _ = out.sync(client(f))
    assertEquals(drain(c).map(_.event).toList, List(Ping(2)))
  }
}
