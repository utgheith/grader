package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_courses,
  in_r3_workspace
}

// How many students each picked active course has enrolled, as its
// enrollment branch on the local mirror says
object Enrollment {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      for (course <- active_courses(s, commonArgs, source)) {
        println(s"\n------ ${course.c} -------")
        println(s.track(course.enrollment).size)
      }
    }
}
