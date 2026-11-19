package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  publishing,
  source_of,
  active_projects,
  in_r3_workspace
}

// Publishes each picked student's work repo for each picked active project
// (see repos.WorkRepo, Publishing)
object PublishWorkRepos {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val found = publishing(commonArgs.workspace) { pub =>
        (for (project <- active_projects(s, commonArgs, source)) yield {
          val csids =
            project.course.enrolled(commonArgs.students_).tally(s).toSeq
          val made = s.track_all(
            csids.map(csid =>
              ag.grader.repos.WorkRepo(project.submission(csid)).latest
            )
          )
          val k = made.count(pub.record)
          println(s"${project.name}: ${csids.size} students, $k to make")
          csids.size
        }).sum
      }
      s.tally.report("publish_work_repos", found, commonArgs.verbose.value)
    }
}
