package ag.grader

import upickle.default.ReadWriter

enum OutcomeStatus derives ReadWriter:
  case pass
  case fail
  case timeout
  case unknown

  def isHappy: Boolean = this == pass

@upickle.implicits.allowUnknownKeys(false)
case class RedactedOutcome(
    project: Project,
    test_id: RedactedTestId,
    outcome: Option[OutcomeStatus],
    time: Option[Double],
    tries: Int
) derives ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class Outcome(
    project: Project,
    csid: CSID,
    test_id: TestId,
    outcome: Option[OutcomeStatus],
    time: Option[Double],
    tries: Int
) derives ReadWriter {
  lazy val redacted: RedactedOutcome = RedactedOutcome(
    project = project,
    test_id = test_id.redacted,
    outcome = outcome,
    time = time,
    tries = tries
  )
}
