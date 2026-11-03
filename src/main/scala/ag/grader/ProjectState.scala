package ag.grader

import upickle.default.ReadWriter

import java.time.LocalDateTime

// How urgent a project's grading work is, most urgent first:
//   LiveFirst: a live project's (second deadline not passed) first tries,
//              up to its iterations.min: quick feedback
//   Other:     a project that isn't live, up to its iterations.max
//   LiveRest:  a live project's remaining tries, up to its iterations.max
enum Band {
  case LiveFirst, Other, LiveRest

  def description: String = this match {
    case LiveFirst => "live, first tries"
    case Other     => "not live"
    case LiveRest  => "live, remaining tries"
  }
}

// Which project `publish_results` grades when none is named, and how many
// tries: the most urgent band's (see Band), then the lowest numbered (p1
// before p2, p9 before p10).
object Priority {
  // `items`: (item, project name, band). None if there are none.
  def pick[A](items: Seq[(A, String, Band)]): Option[A] =
    items
      .minByOption((_, name, band) => (band.ordinal, name.length, name))
      .map(_._1)

  // A project's band, and the tries to run its tests to, if it has work to
  // do: `has_work(n)` says whether it has tests with fewer than n tries.
  def band(
      live: Boolean,
      iterations: Iterations,
      has_work: Int => Boolean
  ): Option[(Band, Int)] =
    if (live) {
      if (has_work(iterations.min)) Some((Band.LiveFirst, iterations.min))
      else if (has_work(iterations.max)) Some((Band.LiveRest, iterations.max))
      else None
    } else Option.when(has_work(iterations.max))((Band.Other, iterations.max))
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
    // for the latest results: keep running a test after it fails?
    val latest_keeps_going: Boolean,
    // for the latest results: which tests
    val latest_tests: TestSet
):
  case BeforeFirstDeadline
      extends ProjectState(
        "before the first deadline",
        false,
        "all"
      )
  case BetweenDeadlines
      extends ProjectState(
        "between the deadlines",
        false,
        "good"
      )
  case AfterSecondDeadline
      extends ProjectState(
        "after the second deadline",
        true,
        "chosen"
      )

  // Whether the project is still taking submissions for its second deadline
  def is_live: Boolean = this != AfterSecondDeadline

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
