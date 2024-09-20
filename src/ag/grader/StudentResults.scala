package ag.grader

import upickle.default.ReadWriter
import scala.collection.SortedMap

case class RedactedStudentResults(
  alias: Option[Alias],
  has_test: Boolean, 
  prepare_info: PrepareInfo, 
  outcomes: SortedMap[RedactedTestId, RedactedOutcome]
) derives ReadWriter

case class StudentResults (
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
