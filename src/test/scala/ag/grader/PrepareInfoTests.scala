package ag.grader

import munit.FunSuite
import upickle.default.{read, write}

import java.time.{ZoneOffset, ZonedDateTime}
import scala.util.Try

class PrepareInfoTests extends FunSuite {
  private val info = PrepareInfo(
    commit_time = ZonedDateTime.of(2026, 10, 1, 12, 0, 0, 0, ZoneOffset.UTC),
    sha = "abc123",
    has_report = true,
    push_time = None,
    commit_source = CommitSource.Latest
  )

  // A saved result that doesn't match the class exactly fails to load, so
  // the target that saved it is computed again instead of being trusted.
  test("a result saved before commit_source existed fails to load") {
    val old = ujson.read(write(info))
    val _ = old.obj.remove("commit_source")
    assert(Try(read[PrepareInfo](old)).isFailure)
  }

  test("a result with a field this version doesn't know fails to load") {
    val extra = ujson.read(write(info))
    extra("from_the_future") = 1
    assert(Try(read[PrepareInfo](extra)).isFailure)
  }

  test("every field is written, including Latest and an empty push_time") {
    val json = ujson.read(write(info))
    assertEquals(
      json.obj.keySet.toSet,
      Set("commit_time", "sha", "has_report", "push_time", "commit_source")
    )
    assertEquals(read[PrepareInfo](json), info)
  }

  test("FromFile round-trips with its file name and sha") {
    val pinned =
      info.copy(commit_source = CommitSource.FromFile("commit_id", "deadbeef"))
    assertEquals(read[PrepareInfo](write(pinned)), pinned)
  }
}
