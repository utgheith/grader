package ag.grader

import upickle.default.ReadWriter

case class RedactedOutcome(
    course_name: String,
    project_name: String,
    test_id: RedactedTestId,
    outcome: Option[String],
    time: Option[Double]
) derives ReadWriter

case class Outcome(
    course_name: String,
    project_name: String, 
    csid: CSID, 
    test_id: TestId, 
    outcome: Option[String], 
    time: Option[Double]
) derives ReadWriter {
    lazy val redacted: RedactedOutcome = RedactedOutcome(
        course_name = course_name,
        project_name = project_name,
        test_id = test_id.redacted,
        outcome = outcome,
        time = time
    )
}
