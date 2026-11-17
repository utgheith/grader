package ag.grader

import ag.grader.json.RawMilestone

import upickle.default.ReadWriter

import java.time.LocalDateTime

// Whom a milestone's grading serves, most urgent first:
//   Open:   students: a milestone they're working toward (its deadline not
//           passed), for fresh feedback
//   Closed: TAs: a milestone whose deadline has passed, for its grades
//   Late:   Live: work pushed after the final deadline
enum Work {
  case Open, Closed, Late

  def description: String = this match {
    case Open   => "open milestones (students)"
    case Closed => "closed milestones (TAs)"
    case Late   => "pushed after the final deadline"
  }
}

// One round of a project's grading: its milestones of one kind of `work`
// that have tests with fewer than `tries` tries, run to that many: one more
// try for the tests furthest behind. A run does one round and ends (time
// boxing): the next run, from a fresh mirror, picks again, so new pushes
// wait for at most one round.
final case class Round(work: Work, milestones: Seq[Milestone], tries: Int) {
  def description: String =
    s"${work.description}, to ${tries.toString} tries"
}

// Which round runs next: the most urgent kind of work (see Work), then the
// fewest tries (first tries before retries), then the lowest numbered
// project (p1 before p2, p9 before p10).
object Priority {
  // `items`: (item, project name, round). None if there are none.
  def pick[A](items: Seq[(A, String, Round)]): Option[A] =
    items
      .minByOption((_, name, r) => (r.work.ordinal, r.tries, name.length, name))
      .map(_._1)

  // A project's next round, if it has work to do: `has_work(m, n)` says
  // whether milestone m (one it publishes) has tests with fewer than n tries.
  // Tests get up to `max` tries.
  def round(
      st: ProjectState,
      max: Int,
      has_work: (Milestone, Int) => Boolean
  ): Option[Round] =
    (for {
      work <- Work.values.to(LazyList)
      milestones = st.all_milestones.filter(m =>
        st.publishes(m) && st.work(m) == work
      )
      tries <- (1 to max).find(n => milestones.exists(has_work(_, n))).toSeq
    } yield Round(
      work,
      milestones.filter(has_work(_, tries)),
      tries
    )).headOption
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

  // Whom grading `milestone` serves (see Work)
  def work(milestone: Milestone): Work = milestone match
    case Milestone.Named(name) => if (is_open(name)) Work.Open else Work.Closed
    case Milestone.Live        => Work.Late

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
