package ag.common

import upickle.default.{read, write}

import java.time.Instant

class ExtensionsTests extends munit.FunSuite {
  test("ReadWriterForInstant") {
    val now = Instant.now()
    val s = write(now)
    val v = read[Instant](s)
    assert(clue(v) == clue(now))
  }
  test("runWhere") {
    val sv = ScopedValue.newInstance[String]()
    assert(sv.orElse("x") == "x")
    sv.runWhere("foo") {
      assert(sv.orElse("z") == "foo")
    }
    assert(sv.orElse("y") == "y")
  }
}
