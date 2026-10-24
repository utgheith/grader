package ag.ondisk

import munit.FunSuite
import upickle.default.{ReadWriter, read, write}

import scala.collection.SortedMap

class OnDiskTests extends FunSuite {
  case class Big(rows: SortedMap[String, Seq[Int]]) derives ReadWriter
  case class Summary(count: Int, details: OnDisk[Big]) derives ReadWriter

  private def big(n: Int): Big =
    Big((1 to n).map(i => s"r$i" -> Seq.fill(20)(i)).to(SortedMap))

  private def fresh_store(): Store = Store(os.temp.dir(deleteOnExit = true))
  private def blobs(store: Store): Seq[os.Path] =
    os.walk(store.root).filter(os.isFile)

  test("a value round-trips, and the reference serializes as just its hash") {
    given Store = fresh_store()
    val d = OnDisk(big(100))
    assertEquals(d.get, big(100))
    assertEquals(write(d), s"\"${d.hash.hex}\"")
  }

  test("inside another value, only the hash is serialized") {
    given store: Store = fresh_store()
    val s = Summary(100, OnDisk(big(100)))
    val json = write(s)
    assert(json.length < 100, json)
    val back = read[Summary](json)
    assertEquals(back, s)
    assertEquals(back.details.get, big(100))
  }

  test("the same content is stored once, under the same hash") {
    given store: Store = fresh_store()
    val a = OnDisk(big(10))
    val b = OnDisk(big(10))
    assertEquals(a, b)
    assertEquals(blobs(store).size, 1)
    assertNotEquals(OnDisk(big(11)).hash, a.hash)
  }

  test("while the value is held, get doesn't read the disk") {
    given store: Store = fresh_store()
    val value = big(10)
    val d = OnDisk(value)
    val _ = os.remove(store.path(d.hash)) // reading the disk would now fail
    assert(d.in_memory)
    assert(d.get eq value)
  }

  test("a reference read back starts empty, then reads and caches the value") {
    given store: Store = fresh_store()
    val d = read[OnDisk[Big]](write(OnDisk(big(10))))
    assert(!d.in_memory)
    val value = d.get
    assertEquals(value, big(10))
    assert(d.in_memory)
    assert(d.get eq value, "cached, not decoded again")
  }

  test("once nothing else holds the value, it's dropped from memory") {
    given store: Store = fresh_store()
    def make(): OnDisk[Big] = OnDisk(big(1000)) // no strong reference escapes
    val d = make()
    var tries = 0
    while (d.in_memory && tries < 50) {
      System.gc()
      Thread.sleep(10)
      tries += 1
    }
    assert(!d.in_memory, "the weak reference was never cleared")
    assertEquals(d.get, big(1000), "read back from the store")
  }

  test("a missing or corrupted blob is an error, not a wrong value") {
    given store: Store = fresh_store()
    val missing = OnDisk.of[Big](Hash.of("never stored".getBytes.nn))
    val _ = intercept[BlobUnavailable](missing.get)

    val h = OnDisk(big(10)).hash
    os.write.over(store.path(h), "{}")
    val _ = intercept[BlobUnavailable](OnDisk.of[Big](h).get)
  }

  test("storing the same content concurrently is safe") {
    given store: Store = fresh_store()
    val refs = (1 to 16).map(_ => ag.task.Task(OnDisk(big(50)))).map(_.block)
    assertEquals(refs.map(_.hash).distinct.size, 1)
    assertEquals(blobs(store).size, 1, "one blob, no leftover temporary files")
  }
}
