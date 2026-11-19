package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  publishing,
  source_of,
  active_courses,
  in_r3_workspace
}

// Publishes each picked active course's grades branch (see
// repos.GradesRepo, Publishing): made with a README if it isn't
// there, then left to the staff.
object CreateGradesRepos {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val cs = active_courses(s, commonArgs, source)
      publishing(commonArgs.workspace) { pub =>
        for (course <- cs) {
          val repo = ag.grader.repos.GradesRepo(course)
          val todo = pub.record(s.track(repo.latest))
          println(
            s"${repo.repo_name}: ${if (todo) "to push" else "up to date"}"
          )
        }
      }
      s.tally.report("create_grades_repos", cs.size, commonArgs.verbose.value)
    }
}
