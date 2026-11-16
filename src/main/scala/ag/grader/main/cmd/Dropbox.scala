package ag.grader.main.cmd

import ag.grader.main.misc.{CommonArgs, source_of, dropbox_of, in_r3_workspace}

import ag.common.given_ReadWriter_SortedMap
import ag.grader.repos.CoursesConfig

// The keys in the dropbox (see ag.grader.targets.Dropbox), for each active course
// the arguments pick
object Dropbox {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val dropbox = dropbox_of()
      for {
        c <- s
          .track(CoursesConfig(source).active_courses(commonArgs.courses_))
          .toSeq
      } {
        println(s"\n------ $c -------")
        println(
          upickle.default.write(s.track(dropbox.keys(c)), indent = 2)
        )
      }
    }
}
