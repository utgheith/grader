package ag.grader

import munit.FunSuite
import upickle.default.write

import java.time.{ZoneOffset, ZonedDateTime}
import scala.collection.SortedMap

class RedactionTests extends FunSuite {
  private val project = Project(Course("cs439t_f26"), "p4")
  private val cutoff =
    ZonedDateTime.of(2026, 10, 1, 23, 59, 0, 0, ZoneOffset.UTC).nn
  private val commit = cutoff.plusHours(6)

  // as publish_student_results builds it: a classmate's test is keyed by
  // alias, though its TestId's internal name is the classmate's CSID
  private val classmate_test =
    TestId(external_name = "042", internal_name = "csid_of_classmate")
  private val mine = StudentResults(
    csid = CSID("csid_of_me"),
    alias = Some(Alias(7)),
    has_test = true,
    prepare_info = PrepareInfo(
      commit_time = commit,
      sha = "0123456789abcdef0123456789abcdef01234567",
      has_report = true,
      push_time = Some(commit.plusHours(3).toInstant.nn),
      commit_source = CommitSource.FromFile("commit_id_2", "fedcba98")
    ),
    outcomes = SortedMap(
      classmate_test.redacted -> RedactedOutcome(
        project,
        classmate_test.redacted,
        Seq(OutcomeStatus.pass -> Some(1.0))
      )
    )
  )

  test("what's published names no student and pins down no commit") {
    val published = write(mine.redacted(cutoff))
    for (
      secret <- Seq(
        "csid_of_me",
        "csid_of_classmate",
        "0123456789abcdef",
        "fedcba98",
        "commit_id_2",
        "2026-10-0"
      )
    )
      assert(!published.contains(secret), s"leaks $secret: $published")
  }

  test("what's published keeps the facts the report needs") {
    val info = mine.redacted(cutoff).prepare_info
    assertEqualsDouble(info.days_late, 0.25, 1e-9)
    assert(info.from_commit_file)
    assert(info.push_gap)
    assert(info.has_report)
  }

  test("a student's own results don't name their classmates") {
    val own = write(mine)
    assert(own.contains("0123456789abcdef"), "their own commit is there")
    assert(
      !own.contains("csid_of_classmate"),
      s"a classmate's CSID leaked: $own"
    )
  }
}
