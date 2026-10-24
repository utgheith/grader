package ag.grader

import upickle.default.ReadWriter

import mainargs.TokensReader

import java.time.LocalDateTime

// How likely a project is to be the one `publish_results` grades when no
// project is named: picked with probability proportional to `weight`, each
// level twice as likely as the one below.
enum Priority(val weight: Int):
  case High extends Priority(4)
  case Medium extends Priority(2)
  case Low extends Priority(1)

object Priority {
  // One of `items`, each with probability weight / total weight; `draw` is
  // uniform in [0, 1). None if there are none.
  def pick[A](items: Seq[(A, Priority)], draw: Double): Option[A] = {
    val total = items.map(_._2.weight).sum
    val target = draw * total
    items
      .scanLeft((Option.empty[A], 0.0)) { case ((_, upto), (a, p)) =>
        (Some(a), upto + p.weight)
      }
      .collectFirst { case (Some(a), upto) if target < upto => a }
  }
}

// Which of a project's tests a grading run covers:
//   all     every test
//   good    all but those marked ignored (`bad_tests`)
//   chosen  the ones the correctness score counts: (phase 1 + phase 2) - ignored
type TestSet = "all" | "good" | "chosen"

object TestSet {
  // mainargs has no reader for a union of literals. (Import it with
  // `TestSet.given`: an alias's companion isn't searched for givens.)
  given TokensReader.Simple[TestSet] with {
    override def shortName: String = "all|good|chosen"
    override def read(strings: Seq[String]): Either[String, TestSet] =
      strings.head match {
        case t @ ("all" | "good" | "chosen") => Right[String, TestSet](t)
        case s                               =>
          Left(s"Invalid test set '$s'; possible sets are all, good or chosen")
      }
  }
}

// Where a project is relative to its two deadlines (test_cutoff, then
// code_cutoff), and what that means for grading it. Worked out from the
// clock on every run, so moving a deadline takes effect right away.
enum ProjectState(
    val description: String,
    val priority: Priority,
    val keep_going: Boolean,
    val tests: TestSet
):
  case BeforeFirstDeadline
      extends ProjectState(
        "before the first deadline",
        Priority.Medium,
        false,
        "all"
      )
  case BetweenDeadlines
      extends ProjectState(
        "between the deadlines",
        Priority.High,
        false,
        "good"
      )
  case AfterSecondDeadline
      extends ProjectState(
        "after the second deadline",
        Priority.Low,
        true,
        "chosen"
      )

object ProjectState {
  given ReadWriter[ProjectState] = upickle.default
    .readwriter[String]
    .bimap(_.toString, ProjectState.valueOf)

  def at(
      now: LocalDateTime,
      test_cutoff: LocalDateTime,
      code_cutoff: LocalDateTime
  ): ProjectState =
    if (now.isBefore(test_cutoff)) BeforeFirstDeadline
    else if (now.isBefore(code_cutoff)) BetweenDeadlines
    else AfterSecondDeadline
}
