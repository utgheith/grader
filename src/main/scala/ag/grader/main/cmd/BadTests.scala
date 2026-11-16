package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_projects,
  in_r3_workspace
}

// The tests (picked by -t) no picked enrolled student is happy with, in
// their published results (see Submission.published_outcomes):
// course_project:test
object BadTests {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val r = (for {
        project <- active_projects(s, commonArgs, source)
        csid <- project.course.enrolled(commonArgs.students_).tally(s).toSeq
        submission = project.submission(csid)
        // counted on its own, to tell no results from results without tests
        _ <- submission.published_results.tally(s).toSeq
        (test, outcome) <- submission
          .published_outcomes(commonArgs.tests_)
          .tally(s)
          .toSeq
      } yield ((project.name, test), !outcome.is_happy))
        .groupMapReduce(_._1)(_._2)(_ && _)

      val bad = r.toSeq.sortBy(_._1).filter(_._2).map(_._1)
      for ((name, t) <- bad)
        println(s"$name:${t.external_name}")
      s.tally.report("bad_tests", bad.size, commonArgs.verbose.value)
    }
}
