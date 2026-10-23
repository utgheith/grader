package ag.grader

import munit.FunSuite

import java.time.{ZoneOffset, ZonedDateTime}

class PrepareInfoTests extends FunSuite {
  private val info = PrepareInfo(
    commit_time = ZonedDateTime.of(2026, 10, 1, 12, 0, 0, 0, ZoneOffset.UTC),
    sha = "abc123",
    has_report = true
  )

  test("results saved before commit_source existed load as Latest") {
    val old = ujson.read(upickle.default.write(info))
    val _ = old.obj.remove("commit_source")
    val back = upickle.default.read[PrepareInfo](old)
    assertEquals(back.commit_source, CommitSource.Latest)
  }

  test("FromFile round-trips with its file name and sha") {
    val pinned =
      info.copy(commit_source = CommitSource.FromFile("commit_id", "deadbeef"))
    val json = upickle.default.write(pinned)
    assertEquals(upickle.default.read[PrepareInfo](json), pinned)
  }
}
