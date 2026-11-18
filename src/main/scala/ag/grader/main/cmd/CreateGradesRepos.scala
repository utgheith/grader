package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  outbox_of,
  source_of,
  active_courses,
  in_r3_workspace
}

// Records each picked active course's grades branch in the outbox, for
// `sync` to push (see repos.GradesRepo): made with a README if it isn't
// there, then left to the staff.
object CreateGradesRepos {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val ob = outbox_of(commonArgs.workspace)
      val cs = active_courses(s, commonArgs, source)
      for (course <- cs) {
        val repo = ag.grader.repos.GradesRepo(course)
        val todo = ob.record(s.track(repo.latest))
        println(
          s"${repo.repo_name}: ${if (todo) "to push" else "up to date"}"
        )
      }
      println(s"${ob.pending.size} pending in all")
      s.tally.report("create_grades_repos", cs.size, commonArgs.verbose.value)
    }
}
