package ag.grader.json

import ag.grader.name.Alias
import munit.FunSuite

import scala.collection.SortedMap

// Reading the runs out of a published results file, whichever grader wrote it
class PublishedRunsTests extends FunSuite {
  import OutcomeStatus.{pass, timeout}
  private val failed = OutcomeStatus.fail

  test(
    "r2's shape and r3's both give each student's sha and runs; what can't be read is left out"
  ) {
    val r2 = """[[1, {"alias": 1, "has_test": false,
                     "prepare_info": {"sha": "abc", "commit_time": "x", "days_late": 0, "from_commit_file": null,
                                      "has_report": false, "push_gap": null, "push_time": null},
                     "outcomes": [[{"external_name": "t0"}, {"project": "p1", "test_id": {"external_name": "t0"},
                                                             "outcomes": [["pass", 1.5], ["fail", null]]}],
                                  [{"external_name": "t1"}, {"outcomes": "garbled"}]]}],
                 [2, {"prepare_info": {}}],
                 "not a pair"]"""
    assertEquals(
      PublishedRuns.parse(r2),
      SortedMap(
        Alias(1) -> PublishedRuns(
          "abc",
          SortedMap("t0" -> Seq((pass, Some(1.5)), (failed, None)))
        )
      )
    )
    val r3 = """[[7, {"alias": 7, "has_test": true, "prepare_info": {"sha": "def"},
                     "outcomes": [[{"external_name": "t2"}, {"course": "c1", "project": "p1",
                                    "test_id": {"external_name": "t2"}, "outcomes": [["timeout", 10]]}]]}]]"""
    assertEquals(
      PublishedRuns.parse(r3),
      SortedMap(
        Alias(7) -> PublishedRuns(
          "def",
          SortedMap("t2" -> Seq((timeout, Some(10.0))))
        )
      )
    )
    assertEquals(PublishedRuns.parse("{}"), SortedMap[Alias, PublishedRuns]())
  }
}
