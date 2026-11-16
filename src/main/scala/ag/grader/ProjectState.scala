package ag.grader

import ag.grader.json.{RawMilestone, Iterations}

import upickle.default.ReadWriter

import java.time.LocalDateTime

// How urgent a project's grading work is, most urgent first:
//   LiveFirst: a live project's (final deadline not passed) first tries,
//              up to its iterations.min: quick feedback
//   Other:     a project that isn't live, up to its iterations.max
//   LiveRest:  a live project's remaining tries, up to its iterations.max
//   Late:      only Live's (work pushed after the final deadline)
enum Band {
  case LiveFirst, Other, LiveRest, Late

  def description: String = this match {
    case LiveFirst => "live, first tries"
    case Other     => "not live"
    case LiveRest  => "live, remaining tries"
    case Late      => "pushed after the final deadline"
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
  // do: `has_work(n)` says whether its milestones have tests with fewer than
  // n tries, `late_work(n)` whether Live has.
  def band(
      live: Boolean,
      iterations: Iterations,
      has_work: Int => Boolean,
      late_work: Int => Boolean
  ): Option[(Band, Int)] =
    if (live) {
      if (has_work(iterations.min)) Some((Band.LiveFirst, iterations.min))
      else if (has_work(iterations.max)) Some((Band.LiveRest, iterations.max))
      else None
    } else if (has_work(iterations.max)) Some((Band.Other, iterations.max))
    else Option.when(late_work(iterations.max))((Band.Late, iterations.max))
}

// Which of a project's tests a grading run covers:
//   All     every test
//   Good    all but those marked ignored (`bad_tests`)
//   Chosen  the ones some milestone counts, less the ignored ones
//   Of(m)   milestone m's tests, less the ignored ones (what its score counts)
enum TestSet derives ReadWriter:
  case All, Good, Chosen
  case Of(milestone: String)

  def description: String = this match
    case All      => "all"
    case Good     => "good"
    case Chosen   => "chosen"
    case Of(name) => s"$name's"

// Where a project is relative to its milestones' deadlines (their `names`,
// in deadline order, the first `passed` of them past), and what that means
// for grading it. Worked out from the clock on every run, so moving a
// deadline takes effect right away.
//
// A milestone is open until its deadline: its commit is the head of master,
// and grading it is the students' feedback, so it runs and shows the good
// tests and stops a test at its first failure. Once closed, it runs its own
// tests to the end. Every milestone's results are published. Live (pushes
// after the final deadline) is published once the final deadline has
// passed, and runs the chosen tests to the end, showing the good ones.
final case class ProjectState(names: Seq[String], passed: Int)
    derives ReadWriter {
  require(0 <= passed && passed <= names.size)

  def all_milestones: Seq[Milestone] =
    names.map(Milestone.Named(_)) :+ Milestone.Live

  // Whether the project is still taking submissions for its final deadline
  def is_live: Boolean = passed < names.size

  private def is_open(name: String): Boolean =
    names.indexOf(name) >= passed

  // what the students are working toward now: the first open milestone, or
  // Live once they're all closed
  def current: Milestone =
    names.lift(passed).fold(Milestone.Live)(Milestone.Named(_))

  def description: String =
    if (!is_live) "after the final deadline"
    else if (passed == 0) s"before ${names.head}'s deadline"
    else
      s"after ${names(passed - 1)}'s deadline, before ${names(passed)}'s"

  // Whether to keep running a test after it fails when grading `milestone`
  def keeps_going(milestone: Milestone): Boolean = milestone match
    case Milestone.Named(name) => !is_open(name)
    case Milestone.Live        => true

  // Which tests grading `milestone` runs
  def tests(milestone: Milestone): TestSet = milestone match
    case Milestone.Named(name) =>
      if (is_open(name)) TestSet.Good else TestSet.Of(name)
    case Milestone.Live => TestSet.Chosen

  // Which tests `milestone`'s results show: those it runs (see tests), and, for
  // Live, the other good tests too, with whatever runs they have already
  // (none are run for them)
  def shows(milestone: Milestone): TestSet = milestone match
    case Milestone.Live => TestSet.Good
    case _              => tests(milestone)

  // Whether `milestone`'s results are published
  def publishes(milestone: Milestone): Boolean = milestone match
    case Milestone.Named(_) => true
    case Milestone.Live     => !is_live
}

object ProjectState {
  def at(now: LocalDateTime, milestones: Seq[RawMilestone]): ProjectState =
    ProjectState(
      milestones.map(_.name),
      milestones.count(m => !now.isBefore(m.deadline))
    )
}
