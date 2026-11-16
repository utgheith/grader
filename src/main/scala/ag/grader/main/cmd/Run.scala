package ag.grader.main.cmd

import ag.grader.main.misc.{CommonArgs, in_r3_workspace, run_projects}

import ag.grader.Milestone

// Runs the picked projects' tests, here, up to their limit (see
// run_projects): nothing is published (grade publishes it). -s, -t and
// --milestone run just those students, tests and milestone.
object Run {
  def apply(
      commonArgs: CommonArgs,
      tries: Option[Int],
      milestone: Option[Milestone]
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val narrow = milestone.isDefined ||
        commonArgs.students_.regex != ".*" || commonArgs.tests_.regex != ".*"
      val _ = run_projects(s, commonArgs, tries, narrow, milestone)
    }
}
