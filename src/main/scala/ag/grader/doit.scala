package ag.grader

import language.experimental.namedTuples

import ag.common.{down}
import ag.r2.say
import ag.rules.{Optional, run}
import scala.collection.SortedSet

// This is the queen bee, everything else is done to prepare the arguments and run this function
// in the correct context.
//

def doit(
    project: Project, // the project
    csid: CSID, // the student
    cores: Int, // the number of cores to use
    prepared_path: os.Path, // prepared submission
    test_id: TestId, // test
    test_path: os.Path, // where to find the test code
    test_extensions: SortedSet[String], // expected test extensions
    n: Int, // run itertion (for logging)
    out_path: os.Path // where to deposit the detailed results
): (OutcomeStatus, Option[Double]) = {

  started_runs.incrementAndGet()
  try {
    if (cores > limit) {
      throw new Exception(s"need $cores cores, limit is $limit")
    }

    // all tests for the same project/csid share the same prepared directory
    Project.run_lock(prepared_path) {
      governor.down(cores) {
        project.copy_test(
          test_id,
          test_path,
          prepared_path,
          test_extensions
        )
        val tn = test_id.external_name
        val m =
          s"$tn/${test_id.internal_name} for ${project.course.course_name}_${project.project_name}_$csid"
        say(f"running#$n $m on $cores cores")

        val start = System.currentTimeMillis()
        var run_time: Option[Double] = None
        val (_, _, stderr) =
          try {
            val _ = os
              .proc("make", "-k", "clean")
              .run(cwd = prepared_path, check = false)
            os.proc("make", "-k", s"$tn.test")
              .run(cwd = prepared_path, check = false)
          } finally {
            val end = System.currentTimeMillis()
            run_time = Some((end - start).toDouble / 1000)
          }

        val result_path = prepared_path / s"$tn.result"
        val outcome_str =
          if (os.isFile(result_path))
            os.read.lines(result_path).headOption
          else None

        val time_path = prepared_path / s"$tn.time"
        val qemu_runtime_str =
          if (os.isFile(time_path))
            os.read.lines(time_path).headOption
          else None

        val qemu_runtime = qemu_runtime_str match
          case Some(Project.TimeFormat(Optional(None), min, sec)) =>
            Some(min.toDouble * 60 + sec.toDouble)
          case Some(
                Project.TimeFormat(Optional(Some(hours)), min, sec)
              ) =>
            Some(
              hours.toDouble * 3600 + min.toDouble * 60 + sec.toDouble
            )
          case Some("timeout") => None
          case _               => None

        val outcome: OutcomeStatus = (
          outcome_str.map(_.toLowerCase.nn),
          qemu_runtime_str.map(_.toLowerCase.nn)
        ) match
          case (_, Some("timeout")) => OutcomeStatus.timeout
          case (Some("pass"), _)    => OutcomeStatus.pass
          case (Some("fail"), _)    => OutcomeStatus.fail
          case (_, _)               => OutcomeStatus.unknown

        val how_long = run_time.map(t => f"$t%.2f")
        val out = (if (outcome.isHappy) fansi.Color.Green
                   else fansi.Color.Red) (outcome.toString)

        say(s"    [${finished_runs.get()}/${started_runs
            .get()}] finished [$out] $m in $how_long seconds")

        stderr.foreach { stderr =>
          os.copy(
            from = stderr,
            to = out_path / s"$tn.err",
            createFolders = true,
            replaceExisting = true,
            followLinks = false
          )
        }
        project.copy_results(prepared_path, out_path, test_id)

        (outcome, qemu_runtime.orElse(run_time))
      }
    }

  } finally {
    val _ = finished_runs.incrementAndGet()
  }
}
