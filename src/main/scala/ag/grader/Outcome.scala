package ag.grader

import upickle.default.{readwriter, ReadWriter}

enum OutcomeStatus:
  case Pass
  case Fail
  case Timeout
  case Unknown

  def label: String = this match
    case Pass    => "pass"
    case Fail    => "fail"
    case Timeout => "timeout"
    case Unknown => "unknown"

object OutcomeStatus {
  implicit val rw: ReadWriter[OutcomeStatus] =
    readwriter[String].bimap[OutcomeStatus](
      status => status.label,
      {
        case "pass"    => Pass
        case "fail"    => Fail
        case "timeout" => Timeout
        case "unknown" => Unknown
        case _         => Unknown
      }
    )
}

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
