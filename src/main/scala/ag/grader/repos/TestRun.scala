package ag.grader.repos

import ag.common.{down, shout}
import ag.grader.{
  Docker,
  OutcomeStatus,
  Project,
  TestId,
  finished_runs,
  governor,
  limit,
  started_runs
}
import ag.r3.{Keyed, Producer, Target, Tree, WithData, create_data}
import ag.rules.run
import upickle.default.ReadWriter

import scala.util.control.NonFatal

// A test's runs, kept by what they ran on (the fields): the prepared
// submission and the test's files (by their trees' ids, which are their
// contents' hashes), how many cores, and the docker image. That's
// everything a run reads, so the runs stay good whatever else changes (what
// the targets that worked out the inputs are called, say): they're
// observations, slow and flaky, that can't be worked out again (see Keyed).
// Runs on the same inputs are the same runs, whoever's submission they're
// for.
@upickle.implicits.allowUnknownKeys(false)
final case class TestRun(
    prepared: Tree,
    test: Tree,
    // the test's name, as the Makefile runs it (<test_name>.test)
    test_name: String,
    cores: Int,
    docker_file: Option[String],
    // the image built from the docker file, named by its contents
    image: Option[String]
) extends Keyed[TestRun]("test_runs") derives ReadWriter {

  // The runs, the latest's output files as their data: runs again while
  // there are fewer than `n` runs, but stops at a failure unless
  // `keep_going`. With n = 0, never runs. `id` and `what` (whose
  // submission) only say what's running.
  def runs(
      id: TestId,
      what: String,
      keep_going: Boolean,
      n: Int
  ): Target[WithData[TestRun.History]] =
    keyed_target[WithData[TestRun.History]] { (producer, saved) =>
      val history = saved.toSeq.flatMap(_.value.value.outcomes)
      saved match {
        case s @ Some(_)
            if history.size >= n || !(keep_going || history.forall(
              _._1 == OutcomeStatus.pass
            )) =>
          s
        case _ if n == 0 =>
          () => create_data(producer)(_ => TestRun.History(this, Seq()))
        case _ =>
          () =>
            create_data(producer) { out =>
              val res = TestRun.run_test(
                producer,
                what,
                id,
                prepared,
                test,
                cores,
                docker_file,
                history.size + 1,
                out
              )
              TestRun.History(this, history :+ res)
            }
      }
    }
}

object TestRun {
  // the outcome of each run so far, and what they ran on
  @upickle.implicits.allowUnknownKeys(false)
  final case class History(
      inputs: TestRun,
      outcomes: Seq[(OutcomeStatus, Option[Double])]
  ) derives ReadWriter

  // a run's directory, gone: whatever the test left in it (read-only files
  // or directories too); a failure to is only logged
  private def remove(work: os.Path): Unit =
    try {
      val _ = os.proc("chmod", "-R", "u+rwx", work).call(check = false)
      os.remove.all(work)
    } catch {
      case NonFatal(e) =>
        shout(s"can't remove ${work.toString}: ${e.getMessage}")
    }

  // the files a run leaves that are kept, by extension
  private val kept =
    Seq("ok", "out", "err", "result", "time", "stdout", "stderr")

  // One run (as doit): a copy of the prepared submission, less anything
  // named like the test (a student's own <test>.result, say), and the test's
  // files; then `make -k clean` and `make -k <test>.test` there. Its output
  // files go to `out`.
  private[repos] def run_test(
      producer: Producer[?],
      what: String,
      id: TestId,
      prepared: Tree,
      test: Tree,
      cores: Int,
      docker_file: Option[String],
      n: Int,
      out: os.Path
  ): (OutcomeStatus, Option[Double]) = {
    ag.rules.refuse_if_read_only(
      s"run ${id.external_name} on $what (${n.toString})"
    )
    val _ = started_runs.incrementAndGet()
    try {
      if (cores > limit)
        throw Exception(s"need $cores cores, limit is $limit")
      governor.down(cores) {
        val root = producer.state.scratch_root
        os.makeDir.all(root)
        val work = os.temp.dir(dir = root, deleteOnExit = false)
        try {
          producer.copy_to(prepared, work)
          val tn = id.external_name
          for (f <- os.list(work) if f.baseName == tn) os.remove.all(f)
          producer.copy_to(test, work)
          val image = docker_file.map(f => Docker.of(os.pwd / os.RelPath(f)))

          val m = s"$tn/${id.internal_name} for $what"
          shout(f"running#$n $m on $cores cores (Docker: ${image.toString})")

          val start = System.currentTimeMillis()
          val _ = os.proc("make", "-k", "clean").run(cwd = work, check = false)
          val (_, stdout, stderr) = os
            .proc(Docker.dockerCommand(work, image), "make", "-k", s"$tn.test")
            .run(cwd = work, check = false)
          val run_time =
            Some((System.currentTimeMillis() - start).toDouble / 1000)

          def first_line(ext: String): Option[String] = {
            val f = work / s"$tn.$ext"
            if (os.isFile(f)) os.read.lines(f).headOption else None
          }
          val result = first_line("result")
          val time = first_line("time")
          val outcome = (
            result.map(_.toLowerCase.nn),
            time.map(_.toLowerCase.nn)
          ) match {
            case (_, Some("timeout")) => OutcomeStatus.timeout
            case (Some("pass"), _)    => OutcomeStatus.pass
            case (Some("fail"), _)    => OutcomeStatus.fail
            case (_, _)               => OutcomeStatus.unknown
          }

          val how_long = run_time.map(t => f"$t%.2f")
          val shown = (if (outcome.isHappy) fansi.Color.Green
                       else fansi.Color.Red) (outcome.toString)
          shout(
            s"    [${finished_runs.get().toString}/${started_runs.get().toString}] finished [${shown.toString}] $m in ${how_long.toString} seconds"
          )

          // make's own output (not the test's .out/.err), when there is
          // some, its first 10K: stdout only if the test didn't pass
          val max_std_size = 10 * 1024
          val kept_stdout = if (outcome.isHappy) None else Some(stdout)
          for {
            (f, ext) <- Seq(kept_stdout -> "stdout", stderr -> "stderr")
            f <- f
            size = os.size(f)
            if size > 0
          } os.write.over(
            out / s"$tn.$ext",
            os.read.bytes(f, 0, math.min(size, max_std_size.toLong).toInt),
            createFolders = true
          )
          for {
            ext <- kept
            src = work / s"$tn.$ext"
            if os.isFile(src)
          } os.copy.over(
            from = src,
            to = out / s"$tn.$ext",
            followLinks = false,
            createFolders = true
          )

          (outcome, Project.parseTime(time).orElse(run_time))
        } finally remove(work)
      }
    } finally { val _ = finished_runs.incrementAndGet() }
  }
}
