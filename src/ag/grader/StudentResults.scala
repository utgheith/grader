package ag.grader

import upickle.default.ReadWriter
import scala.collection.SortedMap

case class StudentFailures(
    alias: Option[Alias],
    has_test: Boolean,
    failed_tests: SortedMap[RedactedTestId, Option[OutcomeStatus]],
    total_tests: Int,
    prepare_info: PrepareInfo
) derives ReadWriter

object StudentFailures {
  def apply(rsr: RedactedStudentResults): StudentFailures = StudentFailures(
    alias = rsr.alias,
    has_test = rsr.has_test,
    failed_tests = (for {
      (test, outcome) <- rsr.outcomes
      if !outcome.outcome.contains(OutcomeStatus.Pass)
    } yield (test, outcome.outcome)).to(SortedMap),
    total_tests = rsr.outcomes.size,
    prepare_info = rsr.prepare_info
  )
}

case class RedactedStudentResults(
    alias: Option[Alias],
    has_test: Boolean,
    prepare_info: PrepareInfo,
    outcomes: SortedMap[RedactedTestId, RedactedOutcome]
) derives ReadWriter

case class StudentResults(
    csid: CSID,
    alias: Option[Alias],
    has_test: Boolean,
    prepare_info: PrepareInfo,
    outcomes: SortedMap[TestId, Outcome]
) derives ReadWriter {
  lazy val redacted: RedactedStudentResults = RedactedStudentResults(
    alias = alias,
    has_test = has_test,
    prepare_info = prepare_info,
    outcomes = (for {
      (test_id, outcome) <- outcomes.toSeq
    } yield (test_id.redacted, outcome.redacted)).to(SortedMap)
  )
}
