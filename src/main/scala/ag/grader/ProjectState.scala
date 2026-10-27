package ag.grader

import upickle.default.ReadWriter

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
//   phase1  phase 1's chosen tests - ignored (what phase 1's score counts)
//   phase2  phase 2's chosen tests - ignored (what phase 2's score counts)
type TestSet = "all" | "good" | "chosen" | "phase1" | "phase2"

// Where a project is relative to its two deadlines (test_cutoff, then
// code_cutoff), and what that means for grading it. Worked out from the
// clock on every run, so moving a deadline takes effect right away.
enum ProjectState(
    val description: String,
    val priority: Priority,
    // for the latest results: keep running a test after it fails?
    val latest_keeps_going: Boolean,
    // for the latest results: which tests
    val latest_tests: TestSet
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

  // Whether to keep running a test after it fails when grading `phase`: a
  // phase's commits are fixed (its results come after its deadline), so it
  // always gets its full runs; the latest results follow the state.
  def keeps_going(phase: Phase): Boolean =
    phase != Phase.Latest || latest_keeps_going

  // Which tests grading `phase` runs: a phase's results are for its score,
  // so just that phase's chosen tests; the latest results follow the state.
  def tests(phase: Phase): TestSet = phase match
    case Phase.Latest => latest_tests
    case Phase.One    => "phase1"
    case Phase.Two    => "phase2"

  // Whether `phase`'s results are published: once its deadline has passed
  // (the latest results always are).
  def publishes(phase: Phase): Boolean = phase match
    case Phase.Latest => true
    case Phase.One    => this != BeforeFirstDeadline
    case Phase.Two    => this == AfterSecondDeadline

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
