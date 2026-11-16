package ag.rules

import ag.common.ToRelPath

// A Scope whose base is derived from its fields: the target definitions
// for one thing (a repo, a course, a project), as a case class whose fields
// say which, with `derives ToRelPath`. Its targets are
// named by where they're defined (see Scoped) and then by all its fields
// (but `source`, which says what they work from, not which they are): the
// name is derived, so no field can be left out of it.
//
// For example, with
//
//   final case class SubmissionMilestone(submission: Submission, milestone: Milestone)
//       extends DerivedScope[SubmissionMilestone] derives ToRelPath {
//     lazy val results: Int => Target[...] = fun { (n: Int) => ... }
//   }
//
// SubmissionMilestone(Submission(Project(Course(c1, source), p1), s1),
// Milestone.Named("m1")).results(3) is the target
//
//   ag/grader/targets/SubmissionMilestone/results/c1/p1/s1/m1/3
//
// where it's defined, then the fields (a field that's itself derived gives
// all of its own, so Submission gives c1/p1/s1; Course's source is left
// out), then fun's argument.
trait DerivedScope[S](using key: ToRelPath[S]) extends Scoped { self: S =>
  val source: Source
  lazy val base: os.RelPath = key(self)
}
