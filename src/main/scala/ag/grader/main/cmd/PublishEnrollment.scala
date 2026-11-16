package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  outbox_of,
  source_of,
  active_courses,
  dropbox_of,
  in_r3_workspace
}

// Records each picked active course's enrollment, from the dropbox, in
// the outbox, for `sync` to push (see repos.EnrollmentRepo): a course
// the dropbox has no directory for is left alone.
object PublishEnrollment {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val dropbox = dropbox_of()
      if (dropbox.root.isEmpty)
        println("no dropbox_path in config.json: enrollments left alone")
      else {
        val ob = outbox_of(commonArgs.workspace)
        for (course <- active_courses(s, commonArgs, source)) {
          println(s"\n------ ${course.c} -------")
          s.track(dropbox.course_keys(course.c)) match {
            case None => println("no directory in the dropbox: left alone")
            case Some(keys) =>
              val todo = ob.record(
                s.track(ag.grader.repos.EnrollmentRepo(course, dropbox).latest)
              )
              println(
                s"${keys.size} students; ${
                    if (todo) "to push" else "up to date"
                  }"
              )
          }
        }
        println(s"${ob.pending.size} pending in all")
      }
    }
}
