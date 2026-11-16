package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_projects,
  in_r3_workspace
}

// the picked active projects of the picked active courses, one per line:
// course_project
object Projects {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val ps = active_projects(s, commonArgs, source).map(_.name)
      ps.foreach(println)
      s.tally.report("projects", ps.size, commonArgs.verbose.value)
    }
}
