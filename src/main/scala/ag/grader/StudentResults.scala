package ag.grader

import upickle.default.ReadWriter
import scala.collection.SortedMap

@upickle.implicits.allowUnknownKeys(false)
case class StudentFailures(
    alias: Option[Alias],
    has_test: Boolean,
    failed_tests: SortedMap[RedactedTestId, Option[OutcomeStatus]],
    total_tests: Int,
    prepare_info: PrepareInfo
) derives ReadWriter

object StudentFailures {
  def apply(rsr: StudentResults): StudentFailures = StudentFailures(
    alias = rsr.alias,
    has_test = rsr.has_test,
    failed_tests = (for {
      (test, outcome) <- rsr.outcomes
      if !outcome.is_happy
    } yield (test, outcome.last)).to(SortedMap),
    total_tests = rsr.outcomes.size,
    prepare_info = rsr.prepare_info
  )
}

// Published to everyone (results repos readable by all, the public report
// pages): must hold nothing that identifies the student. Tests are named by
// alias, and the commit by facts only (see RedactedPrepareInfo).
@upickle.implicits.allowUnknownKeys(false)
case class RedactedStudentResults(
    alias: Option[Alias],
    has_test: Boolean,
    prepare_info: RedactedPrepareInfo,
    outcomes: SortedMap[RedactedTestId, RedactedOutcome]
) derives ReadWriter

// A student's own results: goes to their private results repo (and the
// emails made from it), so it may hold their own CSID and commit details.
// Other students must stay anonymous, though: tests are keyed by
// RedactedTestId, since a student-written test's TestId.internal_name is its
// author's CSID.
@upickle.implicits.allowUnknownKeys(false)
case class StudentResults(
    csid: CSID,
    alias: Option[Alias],
    has_test: Boolean,
    prepare_info: PrepareInfo,
    outcomes: SortedMap[RedactedTestId, RedactedOutcome]
) derives ReadWriter {
  // what may be published about it
  def redacted(code_cutoff: java.time.ZonedDateTime): RedactedStudentResults =
    RedactedStudentResults(
      alias = alias,
      has_test = has_test,
      prepare_info = prepare_info.redacted(code_cutoff),
      outcomes = outcomes
    )
}
