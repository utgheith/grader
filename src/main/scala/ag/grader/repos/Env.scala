package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{Definitions, Dropbox, Grading, Submissions, TestRuns}
import ag.r3.Source

// What the target definitions work from: the local mirror and the dropbox.
// Passed explicitly to every definition's targets; it isn't part of a
// target's name, so one State works with one Env.
//
// For now it also builds the layers whose targets haven't moved into
// definitions of their own yet; they go as their targets move. A target is
// known by its name, so a layer built here and one built elsewhere on the
// same mirror have the same targets.
final class Env(val source: Source, val dropbox: Dropbox = Dropbox(None)) {
  lazy val defs: Definitions = Definitions(source)
  lazy val subs: Submissions = Submissions(defs)
  lazy val runs: TestRuns = TestRuns(subs)
  lazy val grading: Grading = Grading(runs)
}

object Env {
  // never part of a target's name (see Instance)
  given ToRelPath[Env] = ToRelPath.Omitted()
}
