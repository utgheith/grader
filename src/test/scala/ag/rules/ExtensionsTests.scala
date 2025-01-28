package ag.rules

import upickle.default.{read, write}

import java.time.Instant

class ExtensionsTests extends munit.FunSuite {
  test("ReadWriterForInstant") {
    val now = Instant.now()
    val s = write(now)
    val v = read[Instant](s)
    assert(clue(v) == clue(now))
  }
}
