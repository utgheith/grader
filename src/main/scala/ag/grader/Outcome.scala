package ag.grader

import language.experimental.namedTuples

import upickle.default.ReadWriter

enum OutcomeStatus derives ReadWriter:
  case pass
  case fail
  case timeout
  case unknown

  def isHappy: Boolean = this == pass

trait OutcomeApi {
  def project: Project
  def csid_opt: Option[CSID]
  def outcomes: Seq[(OutcomeStatus, Option[Double])]
  def test_id_opt: TestId|RedactedTestId
  def commit_id: Option[String]

  lazy val min_max: Option[(Double, Double)] = {
    val times = outcomes.flatMap(_._2.toSeq)
    if (times.isEmpty) None
    else Some((times.min, times.max))
  }

  lazy val is_happy: Boolean = (!outcomes.isEmpty) && outcomes.forall(_._1.isHappy)

  lazy val last: Option[OutcomeStatus] = outcomes.lastOption.map(_._1)
}

@upickle.implicits.allowUnknownKeys(false)
case class RedactedOutcome(
    project: Project,
    test_id: RedactedTestId,
    outcomes: Seq[(OutcomeStatus, Option[Double])]
) extends OutcomeApi derives ReadWriter {
  def commit_id: Option[String] = None
  def test_id_opt: RedactedTestId|TestId = test_id
  def csid_opt: Option[CSID] = None
}

@upickle.implicits.allowUnknownKeys(false)
case class Outcome(
    project: Project,
    csid: CSID,
    test_id: TestId,
    outcomes: Seq[(OutcomeStatus, Option[Double])],
    commit_id: Option[String]
) extends OutcomeApi derives ReadWriter {

  def csid_opt: Option[CSID] = Some(csid)
  def test_id_opt: TestId|RedactedTestId = test_id

  lazy val redacted: RedactedOutcome = RedactedOutcome(
    project = project,
    test_id = test_id.redacted,
    outcomes = outcomes
  )
}
