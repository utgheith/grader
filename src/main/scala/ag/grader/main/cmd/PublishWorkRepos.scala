package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  outbox_of,
  source_of,
  active_projects,
  in_r3_workspace
}

object PublishWorkRepos {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val ob = outbox_of(commonArgs.workspace)
      var found = 0
      for (project <- active_projects(s, commonArgs, source)) {
        val csids =
          project.course.enrolled(commonArgs.students_).tally(s).toSeq
        val made = s.track_all(
          csids.map(csid =>
            ag.grader.repos.WorkRepo(project.submission(csid)).latest
          )
        )
        val k = made.count(ob.record)
        println(s"${project.name}: ${csids.size} students, $k to make")
        found += csids.size
      }
      println(s"${ob.pending.size} pending in all")
      s.tally.report("publish_work_repos", found, commonArgs.verbose.value)
    }
}
