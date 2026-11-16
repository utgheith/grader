package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_courses,
  in_r3_workspace
}

// Each picked active course's enrolled students (picked by -s), each with
// the picked active projects they have a submission in
object Students {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      var shown = 0
      for (course <- active_courses(s, commonArgs, source)) {
        val ps = s.track(course.active_projects(commonArgs.projects_)).toSeq
        val submitted = s.track_all(
          ps.map(course.project(_).students_with_submission)
        )
        val csids = course.enrolled(commonArgs.students_).tally(s).toSeq
        println(s"\n${course.c} (${csids.size.toString} students):")
        val width = csids.map(_.value.length).maxOption.getOrElse(0)
        for (csid <- csids) {
          val in = for {
            (p, with_submission) <- ps.zip(submitted)
            if with_submission.contains(csid)
          } yield p.value
          println(
            String.format(s"  %-${width}s  %s", csid.value, in.mkString(" "))
          )
        }
        shown += csids.size
      }
      s.tally.report("students", shown, commonArgs.verbose.value)
    }
}
