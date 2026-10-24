package ag.events

import munit.FunSuite

import scala.util.Try

class IdentityTests extends FunSuite {
  private val alice = StudentId.fresh()
  private val bob = StudentId.fresh()

  private def store(): KeyStore =
    KeyStore(os.temp.dir(deleteOnExit = true) / "keys.json")

  test("a box opens only while its student's key exists") {
    given keys: KeyStore = store()
    keys.create(alice)
    val box = Box.seal("ada123", alice)
    assertEquals(box.get, Some("ada123"))
    assert(
      !upickle.default.write(box).contains("ada123"),
      "not readable as stored"
    )
    val stored = upickle.default.read[Box[String]](upickle.default.write(box))
    assertEquals(stored.get, Some("ada123"), "opens after a round trip")
    keys.shred(alice)
    assertEquals(box.get, None, "shredded")
    assertEquals(stored.get, None)
  }

  test("a box tampered with, or moved to another student, fails to open") {
    given keys: KeyStore = store()
    keys.create(alice)
    keys.create(bob)
    val box = Box.seal("ada123", alice)
    assert(Try(box.copy[String](key = bob).get).isFailure, "moved to another student")
    val bytes = Crypto.unb64(box.data)
    // flip a bit (setting a byte could leave it unchanged)
    val tampered =
      box.copy[String](data = Crypto.b64(bytes.updated(0, (bytes(0) ^ 1).toByte)))
    assert(Try(tampered.get).isFailure, "tampered with")
  }

  test("sealing the same value twice gives different boxes") {
    given keys: KeyStore = store()
    keys.create(alice)
    assertNotEquals(Box.seal("x", alice), Box.seal("x", alice))
  }

  test(
    "the key store keeps keys across processes' worth of objects, privately"
  ) {
    val file = os.temp.dir(deleteOnExit = true) / "keys.json"
    val store = KeyStore(file)
    store.create(alice)
    val key = store.get(alice).get
    assertEquals(KeyStore(file).get(alice).map(_.toSeq), Some(key.toSeq))
    assertEquals(os.perms(file).toString, "rw-------")
    val _ = intercept[IllegalStateException](store.create(alice))
    val other = KeyStore(file) // e.g. another process
    other.shred(alice)
    assertEquals(store.get(alice), None, "seen through the cache")
  }

  private def setup(): (FileEventLog, KeyStore) = {
    val dir = os.temp.dir(deleteOnExit = true)
    (
      FileEventLog(dir / "private.jsonl", EventKinds(StudentIdentity.kind)),
      KeyStore(dir / "keys.json")
    )
  }

  private def identities(log: EventLog, keys: KeyStore): Identities = {
    given KeyStore = keys
    Identities(CourseState.view(log))
  }

  test("a student gets one identity, found again by CSID or by id") {
    val (log, keys) = setup()
    val ids = identities(log, keys)
    val a = ids.student_for("ada123")
    assertEquals(ids.student_for("ada123"), a, "the same, not a second one")
    val b = ids.student_for("bob456")
    assertNotEquals(a, b)
    assertEquals(ids.find("ada123"), Some(a))
    assertEquals(ids.csid_of(b), Some("bob456"))
    assertEquals(log.read().size, 2)
    assert(
      !os.read(log.file).contains("ada123"),
      "no CSID in the log in the clear"
    )
  }

  test(
    "shredding a student: their events stay, but nothing ties them to a person"
  ) {
    val (log, keys) = setup()
    val ids = identities(log, keys)
    val a = ids.student_for("ada123")
    keys.shred(a)
    assertEquals(log.read().size, 1, "the log is untouched")
    assertEquals(ids.csid_of(a), None)
    assertEquals(ids.find("ada123"), None)
    // enrolling again starts over, unlinked to the old identity
    val again = ids.student_for("ada123")
    assertNotEquals(again, a)
  }

  test("concurrent creators record only one identity per student") {
    val (log, keys) = setup()
    val got = (1 to 8)
      .map(_ => ag.task.Task(identities(log, keys).student_for("ada123")))
      .map(_.block)
    assertEquals(got.distinct.size, 1)
    assertEquals(log.read().size, 1)
    // losers' unused keys were removed again
    assertEquals(
      upickle.default.read[Map[String, String]](os.read(keys.file)).keySet,
      Set(got.head.uuid)
    )
  }
}
