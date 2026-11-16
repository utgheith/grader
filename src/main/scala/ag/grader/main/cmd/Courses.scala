package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_courses,
  in_r3_workspace
}

// the picked active courses, one per line
object Courses {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val cs = active_courses(s, commonArgs, source)
      cs.foreach(course => println(course.c))
      s.tally.report("courses", cs.size, commonArgs.verbose.value)
    }
}
