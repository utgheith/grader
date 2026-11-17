package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  outbox_of,
  in_r3_workspace,
  run_projects
}

// Runs the picked projects (as run does: with no -p, one round of one
// project), then records their results, as far as they got, in the outbox, for `sync` to push: what the project
// publishes (override, aliases, submitted tests, tests), its results, and
// each student's own results and prepared code (see repos.Grade.all).
// The r3 form of publish_results. With -s or -t, only the students and
// tests they pick are run (as run's), and only the picked students' own
// repos are recorded. What's recorded is everyone's results as far as
// they've run (see repos.Grade.so_far), with runs not saved here taken
// from the published results (see TestRun.runs).
object Grade {
  def apply(commonArgs: CommonArgs, tries: Option[Int]): Unit =
    in_r3_workspace(commonArgs) { s =>
      val picking =
        commonArgs.students_.regex != ".*" || commonArgs.tests_.regex != ".*"
      val projects = run_projects(s, commonArgs, tries, picking, None)
      // as far as everyone has run: the round run is only some of it
      val wanted = s.track_all(projects.flatMap { project =>
        val students = s.track(project.students_with_submission).toSeq
        ag.grader.repos.Grade.so_far(
          project,
          // as run_projects picked them (its report counts them)
          if (picking)
            students.filter(csid => commonArgs.students_.matches(csid.value))
          else students
        )
      })
      val ob = outbox_of(commonArgs.workspace)
      val k = wanted.count(ob.record)
      println(
        s"${projects.size} projects, ${wanted.size} repos, $k to push; ${ob.pending.size} pending in all"
      )
    }
}
