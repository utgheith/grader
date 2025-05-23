package ag.grader

import ag.common.{given_ReadWriter_LocalDateTime, given_ReadWriter_SortedSet}

import ag.rules.{
  check,
  Maker,
  Optional,
  Rule,
  SignedPath,
  down,
  lines,
  run,
  say
}
import upickle.default.ReadWriter

import java.time.LocalDateTime
import scala.collection.{SortedMap, SortedSet, mutable}
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import scala.collection.concurrent.TrieMap
import java.util.concurrent.atomic.AtomicLong
import java.time.{Duration, Instant, ZoneId, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.util.Try
import scala.util.matching.Regex

// TODO: introduce a better abstraction
val limit = Runtime.getRuntime.nn.availableProcessors() - 1
val governor = new Semaphore(limit)

enum CutoffTime:
  case Manual(cutoff: ZonedDateTime)
  case Default
  case None

  def label: String = this match
    case Manual(time) =>
      time
        .withZoneSameInstant(ZoneId.systemDefault)
        .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        .replace(":", "_")
    case Default => "deadline"
    case None    => "latest"

object CutoffTime:
  def fromString(s: Option[String]): CutoffTime = s match
    case Some("default") | Some("deadline") => CutoffTime.Default
    case Some("none") | scala.None          => CutoffTime.None
    case Some(t) =>
      CutoffTime.Manual(
        ZonedDateTime.of(LocalDateTime.parse(t), ZoneId.systemDefault)
      )

private val started_runs = AtomicLong(0)
private val finished_runs = AtomicLong(0)

case class Project(course: Course, project_name: String) derives ReadWriter {
  lazy val scope: os.RelPath = os.RelPath(course.course_name) / project_name

  lazy val info: Maker[RawProject] =
    Gitolite.raw_project(course.course_name, project_name)

  lazy val docker_file: Maker[Option[String]] = Rule(
    info,
    null
  ) { info =>
    info.docker_file
  }

  lazy val docker_image: Maker[Option[String]] = Rule(docker_file, null) {
    docker_file => docker_file.map(fn => Docker.of(os.pwd / fn))
  }

  lazy val active: Maker[Boolean] =
    Rule(
      course.active *: info *: Gitolite.repo_info(project_repo_name),
      scope
    ) {
      case (course_is_active, info, RepoInfo(_, _, Some(_))) =>
        course_is_active && info.active
      case (_, _, RepoInfo(_, _, None)) =>
        false
    }

  lazy val phase1_tests: Maker[SortedSet[String]] =
    Rule(info, scope)(p => p.phase1_tests.to(SortedSet))

  lazy val phase1_weight: Maker[Int] =
    Rule(info, scope)(p => p.phase1_weight)

  lazy val phase2_tests: Maker[SortedSet[String]] =
    Rule(info, scope)(p => p.phase2_tests.to(SortedSet))

  lazy val phase2_weight: Maker[Int] =
    Rule(info, scope)(p => p.phase2_weight)

  lazy val test_weights: Maker[Seq[Weight]] =
    Rule(info, scope)(p => p.test_weights)

  lazy val bad_tests: Maker[SortedSet[String]] =
    Rule(info, scope)(p => SortedSet(p.bad_tests*))

  private lazy val cores: Maker[Int] =
    Rule(info, scope)(p => p.cores)

  lazy val test_cutoff: Maker[LocalDateTime] =
    Rule(info, scope) { p =>
      p.test_cutoff
    }
  lazy val code_cutoff: Maker[LocalDateTime] =
    Rule(info, scope) { p =>
      p.code_cutoff
    }
  lazy val test_extensions: Maker[SortedSet[String]] =
    Rule(info, scope) { p =>
      SortedSet(p.test_extensions*)
    }

  private lazy val project_repo_name: String =
    s"${course.course_name}_$project_name"

  private lazy val project_repo: Maker[SignedPath[Boolean]] =
    Gitolite.mirror(project_repo_name)

  lazy val staff: Maker[SortedSet[CSID]] =
    Rule(info, scope) { p =>
      SortedSet(p.staff*)
    }

  ///////////////////
  // Override Repo //
  ///////////////////

  private lazy val override_repo_name = s"${project_repo_name}__override"

  lazy val publish_override_repo: Maker[SignedPath[Unit]] =
    SignedPath.rule(
      Gitolite.repo_info(
        override_repo_name
      ) *: test_extensions *: project_repo *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) {
      case (
            dir,
            (override_repo_info, test_extensions, project_repo, can_push_repo)
          ) =>
        override_repo_info.update(
          path = dir,
          fork_from = Some("empty"),
          msg = "update override",
          readers = Seq(s"@all"),
          writers = Seq(course.staff_group_name),
          can_push_repo
        ) { _ =>

          val initialized_marker = dir / ".initialized"

          if (!os.exists(initialized_marker)) {

            /* find the original tests */
            for {
              f <- os.list(project_repo.path)
              if !f.last.startsWith(".")
              if test_extensions.contains(f.ext)
            } {
              val target = dir / f.relativeTo(project_repo.path)
              if (!os.exists(target)) {
                os.copy(
                  from = f,
                  to = target,
                  followLinks = false,
                  createFolders = true,
                  copyAttributes = true
                )
              }
            }

            /* copy other files */
            os.walk
              .stream(path = project_repo.path, followLinks = false)
              .foreach { f =>
                if (
                  os.isFile(f) && Project.automatic_override_names.contains(
                    f.last
                  )
                ) {
                  val target = dir / f.relativeTo(project_repo.path)
                  if (!os.exists(target)) {
                    os.copy(
                      from = f,
                      to = target,
                      followLinks = false,
                      createFolders = true,
                      copyAttributes = true
                    )
                  }
                }
              }

            os.write(initialized_marker, "")
          }

        }
    }

  //////////////////////////
  // Work repo (per csid) //
  //////////////////////////

  def work_repo_name(csid: CSID): String = s"${project_repo_name}_${csid.value}"

  def work_repo(csid: CSID): Maker[SignedPath[Boolean]] =
    Gitolite.mirror(work_repo_name(csid))

  def publish_work_repo(csid: CSID): Maker[SignedPath[Unit]] =
    SignedPath.rule(
      Gitolite.repo_info(
        work_repo_name(csid)
      ) *: course.notifications *: Config.can_send_mail *: Config.can_push_repo,
      SortedSet(".git"),
      scope / csid.value
    ) { case (dir, (repo_info, n, can_send_mail, can_push_repo)) =>
      repo_info.update(
        path = dir,
        fork_from = Some(project_repo_name),
        readers = Seq(csid.value, course.staff_group_name),
        writers = Seq(csid.value),
        msg = s"create",
        can_push_repo
      ) { forked =>
        if (forked) {
          n.send_repo_created(this, repo_info, csid, can_send_mail, dir)
        }
        ()
      }
    }

  private def submission(csid: CSID): Maker[Option[SignedPath[Unit]]] =
    Rule(work_repo(csid) *: project_repo, scope / csid.value) {
      case (work_repo, project_repo) =>
        if (
          (!work_repo.data) || (work_repo.signature == project_repo.signature)
        ) {
          None
        } else {
          Some(work_repo.copy(data = ()))
        }
    }

  lazy val submissions: Maker[SortedMap[CSID, SignedPath[Unit]]] = Rule(
    course.enrollment.map(_.keySet).flatMapSeq { (csid: CSID) =>
      for {
        s <- submission(csid)
      } yield (csid, s)
    },
    scope
  ) { pairs =>
    val valid_pairs = pairs.collect { case (csid, Some(sp)) =>
      (csid, sp)
    }
    SortedMap(valid_pairs*)
  }

  lazy val students_with_submission: Maker[SortedSet[CSID]] =
    Rule(submissions, scope)(_.keySet)

  private val report_name: String = "REPORT.txt"

  private val initial_report: Maker[SignedPath[Unit]] = SignedPath.rule(
    project_repo,
    SortedSet(".git"),
    scope
  ) { case (dir, project_repo) =>
    val src = project_repo.path / report_name
    val dest = dir / report_name
    if (project_repo.data && os.isFile(src)) {
      os.copy(
        from = src,
        to = dest,
        followLinks = false,
        replaceExisting = true,
        createFolders = true
      )
    } else {
      os.remove.all(dest)
    }
  }

  private def student_report(csid: CSID): Maker[SignedPath[Unit]] =
    SignedPath.rule(
      submission(csid),
      SortedSet(".git"),
      scope / csid.value
    ) { case (dir, submission) =>
      val dest = dir / report_name
      os.remove.all(dest)
      submission.foreach { submission =>
        val src = submission.path / report_name
        if (os.isFile(src)) {
          os.copy(
            from = src,
            to = dest,
            followLinks = false,
            replaceExisting = true,
            createFolders = true
          )
        }
      }
    }

  private def has_student_report(csid: CSID): Maker[Boolean] = Rule(
    initial_report *: student_report(csid),
    scope / csid.value
  ) { case (initial_report, student_report) =>
    initial_report.signature != student_report.signature
  }

  def copy_test(
      test_info: TestInfo,
      from: os.Path,
      to: os.Path,
      test_extensions: SortedSet[String]
  ): Unit = {
    val name = test_info.id.external_name
    // remove all files that start with this name
    if (os.exists(to)) {
      for {
        p <- os.list(to)
        if p.baseName == name
      } {
        os.remove.all(p)
      }
    }

    // copy the test files
    for {
      ext <- test_extensions
      name_ext = s"$name.$ext"
      src = os.followLink(from / name_ext)
      if src.exists(p => os.exists(p))
    } {
      os.copy(
        from = src.get,
        to = to / name_ext,
        followLinks = false,
        replaceExisting = true,
        createFolders = true
      )
    }
  }

  def prepare(
      csid: CSID,
      cutoff: CutoffTime,
      commit_id_file: String
  ): Maker[SignedPath[Option[PrepareInfo]]] =
    SignedPath.rule(
      submission(csid) *: has_student_report(
        csid
      ) *: publish_override_repo *: test_extensions *: code_cutoff,
      SortedSet(".git"),
      scope / csid.value / cutoff.label / commit_id_file
    ) {
      case (
            dir,
            (
              Some(submission_repo),
              has_student_report,
              override_repo,
              test_extensions,
              code_cutoff
            )
          ) =>
        os.remove.all(dir)

        // (1) prepare code, no checkout
        val _ = os
          .proc(
            "git",
            "clone",
            "--shared",
            "--template=",
            "--no-checkout",
            submission_repo.path,
            dir
          )
          .run()

        // fetch notes (e.g. PushTime)
        os.proc("git", "fetch", "origin", "refs/notes/*:refs/notes/*")
          .check(cwd = dir)

        val default_branch = "master"

        // (2) what commit should we use
        val commit_id_path = submission_repo.path / commit_id_file
        val cutoff_time = cutoff match
          case CutoffTime.Manual(cutoff_time) => Some(cutoff_time)
          case CutoffTime.Default =>
            Some(ZonedDateTime.of(code_cutoff, ZoneId.systemDefault))
          case CutoffTime.None => None
        val commit_id =
          if (
            os.exists(commit_id_path) && (os.stat(commit_id_path).size != 0)
          ) {
            os.read.lines(commit_id_path).head.trim.nn
          } else {
            cutoff_time match
              case Some(cutoff_time) =>
                // Find the last commit in the main branch before the cutoff time.
                // Example: git log master --first-parent --before="2024-09-12 16:00" --pretty=format:"%H %cI %f"

                // Note that git log shows author time by default, not commit time; this can
                // lead to cases where it looks like this fails to select a commit before the
                // date. Add --pretty=fuller to make git log show both times.

                // --first-parent is used to prevent this from using commits from other branches
                // that were merged into master after the cutoff point.

                val cutoff_string =
                  cutoff_time.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
                os.proc(
                  "git",
                  "log",
                  default_branch,
                  "--before",
                  cutoff_string,
                  "--first-parent",
                  "--pretty=format:%H"
                ).lines(cwd = dir)
                  .head
                  .trim
                  .nn
              case None => default_branch
          }

        // (3) checkout the correct commit_id
        val _ =
          try {
            os.proc("git", "checkout", commit_id).run(cwd = dir)
          } catch {
            case ex: Throwable =>
              ex.printStackTrace()
              say(
                s"??????????????? checking out '$commit_id' failed, trying 'master'"
              )
              os.proc("git", "checkout", "master").run(cwd = dir)
          }

        val git_sha =
          os.proc("git", "rev-parse", "HEAD").lines(cwd = dir).head.trim.nn

        // commit time
        val commit_time = os
          .proc("git", "show", "-s", "--format=%ct")
          .lines(cwd = dir)
          .head
          .trim
          .nn
          .toLong
        val zdt = ZonedDateTime.ofInstant(
          Instant.ofEpochSecond(commit_time),
          ZoneOffset.UTC
        )

        // push time
        val push_time = (for {
          line <- os.proc("git", "show", "-s", "--format=%N").lines(cwd = dir)
          parts = line.split(' ')
          if parts.length >= 2
          if parts(0) == "PushTime:"
          v <- Try(parts(1).toLong).toOption
        } yield v).maxOption.map(s => Instant.ofEpochSecond(s, 0))

        // git log
        val commit_log = os
          .proc(
            "git",
            "log",
            "--graph",
            "--pretty=format:%Cred%h%Creset -%C(yellow)%d%Creset %s %C(bold blue)<%an>%Creset",
            "--abbrev-commit"
          )
          .lines(cwd = dir)
        os.write.over(
          dir / "git_log",
          commit_log.mkString("", "\n", "\n")
        )

        // (4) override
        os.copy(
          override_repo.path,
          dir,
          followLinks = false,
          replaceExisting = true,
          createFolders = true,
          mergeFolders = true
        )

        // (5) remove all tests
        for {
          p <- os.list(dir)
          if test_extensions.contains(p.ext)
        } os.remove.all(p)

        // (6) remove .git
        os.remove.all(dir / ".git")

        // (7) write commit info to file
        val late_message = cutoff_time match
          case Some(cutoff_time) if zdt.isAfter(cutoff_time) => "late"
          case _                                             => "onTime"
        os.write.over(
          dir / "prepared_commit",
          s"$git_sha\n$zdt\n$late_message\n"
        )

        Some(
          PrepareInfo(
            commit_time = zdt,
            sha = git_sha,
            has_report = has_student_report,
            push_time = push_time
          )
        )

      case (_, (None, _, _, _, _)) =>
        None

    }

  def is_late(
      csid: CSID,
      cutoff_time: CutoffTime,
      commit_id_file: String
  ): Maker[Boolean] = Rule(
    prepare(csid, cutoff_time, commit_id_file) *: code_cutoff,
    scope / csid.value / cutoff_time.label / commit_id_file
  ) { case (prepare, code_cutoff) =>
    prepare.data
      .map { info =>
        info.commit_time.isAfter(
          ZonedDateTime.of(code_cutoff, ZoneId.systemDefault())
        )
      }
      .getOrElse(true)
  }

  def late_commits(
      csid: CSID,
      cutoff: CutoffTime
  ): Maker[SignedPath[Seq[LateCommit]]] =
    SignedPath.rule(
      submission(csid) *: code_cutoff,
      SortedSet(".git"),
      scope / csid.value / cutoff.label
    ) {
      case (dir, (Some(submission_repo), code_cutoff)) =>
        os.remove.all(dir)

        val _ = os
          .proc(
            "git",
            "clone",
            "--shared",
            "--template=",
            "--no-checkout",
            submission_repo.path,
            dir
          )
          .run()

        val default_branch = "master"

        val cutoff_time = cutoff match
          case CutoffTime.Manual(cutoff_time) => Some(cutoff_time)
          case CutoffTime.Default =>
            Some(ZonedDateTime.of(code_cutoff, ZoneId.systemDefault))
          case CutoffTime.None => None

        cutoff_time match
          case Some(cutoff_time) =>
            val cutoff_string =
              cutoff_time.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
            val lines = os
              .proc(
                "git",
                "log",
                default_branch,
                "--since",
                cutoff_string,
                "--first-parent",
                "--reverse",
                "--pretty=format:%H%x00%ct%x00%s"
              )
              .lines(cwd = dir)

            lines.map(line => {
              val (hash, commit_timestamp, message) = line.split("\u0000") match
                case Array(hash, timestamp, message) =>
                  (hash, timestamp, message)
                case Array(hash, timestamp) => (hash, timestamp, "")

              val commit_time = ZonedDateTime.ofInstant(
                Instant.ofEpochSecond(commit_timestamp.trim.nn.toLong),
                ZoneOffset.UTC
              )
              val delay = Duration.between(cutoff_time, commit_time)
              LateCommit(hash, commit_time, delay, message)
            })

          case None => Seq()

      case (_, (None, _)) =>
        Seq()

    }

  private def copy_results(
      src_dir: os.Path,
      dest_dir: os.Path,
      test_id: TestId
  ): Unit = {
    def one(ext: String): Unit = {
      val name = s"${test_id.external_name}.$ext"
      val src = src_dir / name
      if (os.isFile(src)) {
        os.copy.over(
          from = src,
          to = dest_dir / name,
          followLinks = false,
          createFolders = true
        )
      }
    }

    one("ok")
    one("out")
    one("err")
    one("result")
    one("time")
    one("cycles")
  }

  private def dockerCommand(
      workingDir: os.Path,
      dockerImage: Option[String]
  ): os.Shellable = {
    dockerImage.toSeq.flatMap { dockerImage =>
      Seq(
        "docker",
        "run",
        "--rm",
        "-v",
        s"${workingDir.toString}:/work",
        "-w",
        "/work",
        "-u",
        s"${Project.uid}:${Project.gid}",
        "-t",
        dockerImage
      )
    }
  }

  // Run a submission/test combination once and report the outcome
  def run_one(
      csid: CSID,
      cutoff: CutoffTime,
      test_id: TestId,
      n: Int,
      commit_id_file: String
  ): Maker[SignedPath[Outcome]] =
    SignedPath.rule(
      test_info(test_id) *: test_extensions *: prepare(
        csid,
        cutoff,
        commit_id_file
      ) *: cores *: docker_image,
      SortedSet(),
      scope / csid.value / cutoff.label / test_id.external_name / test_id.internal_name / n
        .max(1)
        .toString
        / commit_id_file
    ) {
      case (
            out_path,
            (test_info, test_extensions, prepared, cores, docker_image)
          ) =>
        started_runs.incrementAndGet()
        try {
          if (cores > limit) {
            throw new Exception(s"need $cores cores, limit is $limit")
          }

          // all tests for the same project/csid share the same prepared directory
          Project.run_lock(prepared.path) {
            governor.down(cores) {
              copy_test(
                test_info.data,
                test_info.path,
                prepared.path,
                test_extensions
              )
              val tn = test_id.external_name
              val m =
                s"$tn/${test_id.internal_name} for ${course.course_name}_${project_name}_$csid"
              say(
                f"running#$n $m on $cores cores (docker_image: $docker_image)"
              )

              val start = System.currentTimeMillis()
              var run_time: Option[Double] = None
              val (_, _, stderr) =
                try {
                  val _ = os
                    .proc("make", "-k", "clean")
                    .run(cwd = prepared.path, check = false)
                  os.proc(
                    dockerCommand(prepared.path, docker_image),
                    "make",
                    "-k",
                    s"$tn.test"
                  ).run(cwd = prepared.path, check = false)
                } finally {
                  val end = System.currentTimeMillis()
                  run_time = Some((end - start).toDouble / 1000)
                }

              val result_path = prepared.path / s"$tn.result"
              val outcome_str =
                if (os.isFile(result_path))
                  os.read.lines(result_path).headOption
                else None

              val time_path = prepared.path / s"$tn.time"
              val qemu_runtime_str =
                if (os.isFile(time_path)) os.read.lines(time_path).headOption
                else None

              val qemu_runtime = qemu_runtime_str match
                case Some(Project.TimeFormat(Optional(None), min, sec)) =>
                  Some(min.toDouble * 60 + sec.toDouble)
                case Some(
                      Project.TimeFormat(Optional(Some(hours)), min, sec)
                    ) =>
                  Some(hours.toDouble * 3600 + min.toDouble * 60 + sec.toDouble)
                case Some("timeout") => None
                case _               => None

              val outcome = (
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
              copy_results(prepared.path, out_path, test_id)
              Outcome(
                this,
                csid,
                test_id,
                Some(outcome),
                // In the case of a timeout, show the outer runtime
                time = qemu_runtime.orElse(run_time),
                tries = n,
                prepared.data.map(_.sha)
              )
            }
          }
        } finally {
          val _ = finished_runs.incrementAndGet()
        }
    }

  // Run a submission/test combination up to "n" times, up to the first failure and report the last outcome
  def run(
      csid: CSID,
      cutoff: CutoffTime,
      test_id: TestId,
      n: Int,
      commit_id_file: String
  ): Maker[SignedPath[Outcome]] = {
    val m = n.max(1)
    Rule(
      if (m == 1) {
        run_one(csid, cutoff, test_id, 1, commit_id_file)
      } else {
        val prev_maker = run(csid, cutoff, test_id, n - 1, commit_id_file)
        for {
          prev <- prev_maker
          it <-
            if (prev.data.outcome.contains(OutcomeStatus.pass))
              run_one(csid, cutoff, test_id, m, commit_id_file)
            else prev_maker
        } yield it
      },
      scope / csid.value / cutoff.label / test_id.external_name / test_id.internal_name / m.toString
    )(x => x)
  }

  def student_results_repo_name(csid: CSID): String =
    s"${course.course_name}_${project_name}_${csid.value}_results"

  private def publish_student_results(
      csid: CSID,
      n: Int,
      commit_id_file: String
  ): Maker[SignedPath[StudentResults]] =
    SignedPath.rule(
      prepare(csid, CutoffTime.None, commit_id_file) *:
        Gitolite.repo_info(student_results_repo_name(csid)) *:
        test_ids.flatMapSeq(test_id =>
          run(csid, CutoffTime.None, test_id, n, commit_id_file)
        ) *:
        csid_to_alias(csid) *:
        csid_has_test(csid) *:
        Config.can_push_repo,
      SortedSet(".git"),
      scope / csid.value / n.toString / commit_id_file
    ) {
      case (
            dir,
            (
              prepared,
              student_results_repo,
              outcomes,
              alias,
              has_test,
              can_push_repo
            )
          ) =>
        student_results_repo.update(
          path = dir,
          fork_from = Some("empty"),
          msg = "updated results",
          readers = Seq(csid.value, course.staff_group_name),
          writers = Seq(),
          can_push_repo
        ) { _ =>

          for {
            f <- os.list(dir)
            if !f.last.startsWith(".")
          } {
            os.remove.all(f)
          }

          for (outcome <- outcomes) {
            copy_results(outcome.path, dir, outcome.data.test_id)
          }
          val pairs = for {
            outcome <- outcomes
          } yield (outcome.data.test_id, outcome.data)

          val res = StudentResults(
            csid = csid,
            alias = alias,
            has_test = has_test,
            prepare_info = prepared.data.get,
            outcomes = pairs.to(SortedMap)
          )
          os.write.over(
            dir / "the_results.json",
            upickle.default.write(res.redacted)
          )
          res
        }
    }

  def get_student_results(csid: CSID): Maker[Option[RedactedStudentResults]] =
    Rule(Gitolite.mirror(student_results_repo_name(csid)), scope / csid.value) {
      sp =>
        if (sp.data) {
          val f = sp.path / "the_results.json"

          if (os.isFile(f)) {
            Some(
              upickle.default.read[RedactedStudentResults](os.read(f))
            )
          } else {
            None
          }
        } else {
          None
        }
    }

  private def get_student_failures(csid: CSID): Maker[Option[StudentFailures]] =
    Rule(
      get_student_results(csid),
      scope / csid.value
    ) {
      _.map { rdr =>
        StudentFailures(rdr)
      }
    }

  private def has_spamon(csid: CSID): Maker[Boolean] = Rule(
    work_repo(csid),
    scope / csid.value
  ) { work_repo => os.exists(work_repo.path / "spamon") }

  def notify_student_results(csid: CSID): Maker[Unit] =
    Rule(
      get_student_failures(
        csid
      ) *: course.notifications *: Config.gitolite *: has_spamon(
        csid
      ) *: Config.can_send_mail,
      scope / csid.value
    ) {
      case (Some(sr), n, g, has_spamon, can_send_mail) =>
        if (has_spamon) {
          n.send_result_update(this, csid, g, sr, can_send_mail)
        } else {
          say(s"no spamon for ${course.course_name}:$project_name:$csid")
        }
      case (None, _, _, _, _) =>
    }

  lazy val project_results_repo_name: String =
    s"${course.course_name}_${project_name}__results"

  def publish_results(
      n: Int,
      commit_id_file: String
  ): Maker[SignedPath[SortedMap[Alias, RedactedStudentResults]]] =
    SignedPath.rule(
      Gitolite.repo_info(project_results_repo_name) *:
        students_with_submission.flatMapSeq((csid: CSID) =>
          publish_student_results(csid, n, commit_id_file).map(r => (csid, r))
        ) *:
        publish_aliases *: Config.can_push_repo,
      SortedSet(".git"),
      scope / n.toString / commit_id_file
    ) { case (dir, (repo_info, results, aliases, can_push_repo)) =>
      repo_info.update(
        path = dir,
        fork_from = Some("empty"),
        msg = "update results",
        readers = Seq("@all"),
        writers = Seq(),
        can_push_repo
      ) { _ =>
        val data = (for {
          (csid, student_results) <- results
          alias <- aliases.data.get(csid).toSeq
        } yield (alias, student_results.data.redacted)).to(SortedMap)
        os.write.over(
          dir / "results.json",
          upickle.default.write(data, indent = 2)
        )
        data
      }
    }

  lazy val results: Maker[Option[SortedMap[Alias, RedactedStudentResults]]] =
    Rule(
      Gitolite.mirror(project_results_repo_name),
      scope
    ) { results_repo =>
      if (results_repo.data) {
        val f = results_repo.path / "results.json"
        if (os.exists(f)) {
          val d = upickle.default
            .read[SortedMap[Alias, RedactedStudentResults]](os.read(f))
          Some(d)
        } else {
          None
        }
      } else {
        None
      }
    }

  ///////////////////
  // student tests //
  ///////////////////

  private def student_test(csid: CSID): Maker[SignedPath[Option[String]]] =
    SignedPath.rule(
      submission(csid) *: test_extensions *: test_cutoff,
      SortedSet(".git"),
      scope / csid.value
    ) {
      case (dir, (Some(submission), test_extensions, test_cutoff)) =>
        os.remove.all(dir)
        // clone, no checkout
        val _ = os
          .proc(
            "git",
            "clone",
            submission.path,
            "--shared",
            "--template=",
            "--no-checkout",
            dir
          )
          .run(cwd = os.pwd)
        // find the latest commit after the cutoff
        val rev = os
          .proc(
            "git",
            "rev-list",
            "-n",
            "1",
            "--first-parent",
            "--until",
            test_cutoff.toString,
            "master"
          )
          .lines(cwd = dir)
          .head
          .trim
          .nn
        // get a list of top level file names
        val file_names = os
          .proc("git", "ls-tree", "--name-only", rev)
          .lines(cwd = dir)
          .to(SortedSet)

        val looking_for = test_extensions.map(e => s"$csid.$e")
        val out = if (looking_for.subsetOf(file_names)) {
          looking_for.foreach { name =>
            val _ = os.proc("git", "checkout", rev, name).run(cwd = dir)
          }
          true
        } else {
          false
        }
        // os.write(dir / ".rev", rev)
        os.remove.all(dir / ".git")
        if (out) Some(rev) else None
      case (dir, (None, _, _)) =>
        os.remove.all(dir)
        os.makeDir.all(dir)
        None
    }

  private lazy val student_tests_by_csid
      : Maker[SortedMap[CSID, SignedPath[String]]] =
    Rule(
      for {
        csids <- students_with_submission
        ids = csids.toSeq
        possible_tests <- Maker.sequence(ids.map(student_test))
      } yield ids.zip(possible_tests),
      scope
    ) { possible_tests =>
      (for {
        (csid, test) <- possible_tests
        git_sha <- test.data.toSeq
      } yield (csid, test.copy(data = git_sha))).to(SortedMap)
    }

  private lazy val csids_with_tests: Maker[SortedSet[CSID]] =
    Rule(student_tests_by_csid, scope) { student_tests =>
      student_tests.keySet
    }

  //////////////////
  // Aliases repo //
  //////////////////

  private lazy val aliases_repo_name: String =
    s"${course.course_name}_${project_name}__aliases"

  private lazy val publish_aliases: Maker[SignedPath[SortedMap[CSID, Alias]]] =
    SignedPath.rule(
      Gitolite.repo_info(
        aliases_repo_name
      ) *: students_with_submission *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) { case (dir, (repo_info, students_with_submission, can_push_repo)) =>
      repo_info.update(
        path = dir,
        fork_from = Some("empty"),
        msg = "more aliases",
        readers = Seq(course.staff_group_name),
        writers = Seq(),
        can_push_repo
      ) { _ =>
        val file = dir / "aliases.json"
        val old_aliases = if (os.exists(file)) {
          upickle.default.read[SortedMap[CSID, Alias]](os.read(file))
        } else {
          SortedMap[CSID, Alias]()
        }

        var next_alias = old_aliases.values.foldLeft(0)(_ max _.value) + 1
        val covered_csids = old_aliases.keySet

        var new_aliases = old_aliases.toMap

        for {
          csid <- students_with_submission
          if !covered_csids.contains(csid)
        } {
          new_aliases = new_aliases.updated(csid, Alias(next_alias))
          next_alias += 1
        }

        val out = new_aliases.to(SortedMap)
        os.write.over(file, upickle.default.write(out, indent = 2))
        out
      }
    }

  lazy val get_aliases: Maker[SortedMap[CSID, Alias]] =
    Rule(Gitolite.mirror(aliases_repo_name), scope) { aliases =>
      val path = aliases.path / "aliases.json"
      if (os.isFile(path)) {
        upickle.default.read[SortedMap[CSID, Alias]](os.read(path))
      } else {
        SortedMap[CSID, Alias]()
      }
    }

  lazy val anti_aliases: Maker[SortedMap[Alias, CSID]] =
    Rule(publish_aliases, scope) { aliases =>
      val pairs = for {
        (csid, alias) <- aliases.data.toSeq
      } yield (alias, csid)
      pairs.to(SortedMap)
    }

  private def csid_to_alias(csid: CSID): Maker[Option[Alias]] =
    Rule(publish_aliases, scope / csid.value) { aliases =>
      aliases.data.get(csid)
    }

  ///////////
  // Tests //
  ///////////

  private lazy val override_test_names: Maker[SortedSet[String]] =
    Rule(
      publish_override_repo *: test_extensions,
      scope
    ) { case (override_repo, test_extensions) =>
      val possible_tests = (for {
        f <- os.list(override_repo.path)
        if test_extensions.contains(f.ext)
        if !f.last.startsWith(".")
      } yield f).groupBy(f => f.baseName)

      val complete_tests =
        possible_tests.filter(_._2.size == test_extensions.size)

      complete_tests.keySet.to(SortedSet)
    }

  private def override_test(test_name: String): Maker[SignedPath[String]] =
    SignedPath.rule(
      publish_override_repo *: test_extensions,
      SortedSet(),
      scope / test_name
    ) { case (dir, (override_repo, test_extensions)) =>
      os.remove.all(dir)
      os.makeDir.all(dir)
      for (ext <- test_extensions) {
        val f = s"$test_name.$ext"
        os.copy(
          from = override_repo.path / f,
          to = dir / f,
          createFolders = true,
          followLinks = false
        )
      }
      ""
    }

  private lazy val override_tests
      : Maker[SortedMap[String, SignedPath[String]]] =
    Rule(
      for {
        test_names <- override_test_names.map(_.toSeq)
        tests <- Maker.sequence(
          test_names.map(test_name => override_test(test_name))
        )
      } yield test_names.zip(tests),
      scope
    ) { pairs =>
      pairs.to(SortedMap)
    }

  lazy val publish_tests: Maker[SignedPath[SortedMap[TestId, TestInfo]]] =
    SignedPath.rule(
      Gitolite.repo_info(
        tests_repo_name
      ) *: student_tests_by_csid *: publish_aliases *: publish_submitted_tests *: override_tests *: test_extensions *: bad_tests *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) {
      case (
            dir,
            (
              tests_repo_info,
              student_tests_by_csid,
              aliases,
              submitted_tests,
              override_tests,
              test_extensions,
              bad_tests,
              can_push_repo
            )
          ) =>
        tests_repo_info.update(
          path = dir,
          fork_from = Some("empty"),
          msg = "tests",
          readers = Seq("@all"),
          writers = Seq(),
          can_push_repo
        ) { _ =>
          // delete all tests
          for {
            f <- os.list(dir)
            if !f.last.startsWith(".")
          } {
            os.remove.all(f)
          }

          val info = mutable.Map[TestId, TestInfo]()

          // copy student tests
          for {
            (csid, test_sp) <- student_tests_by_csid.toSeq
            alias <- aliases.data.get(csid).toSeq
            if !bad_tests.contains(alias.toString) && !bad_tests.contains(
              csid.value
            )
          } {
            for (ext <- test_extensions) {
              val test_id = TestId(
                external_name = alias.toString,
                internal_name = csid.toString
              )
              val test_info = TestInfo(test_id, test_sp.copy(data = ()))
              info.update(test_id, test_info)
              val src = test_sp.path / s"${csid.value}.$ext"
              val dest = dir / s"$alias.$ext"
              os.copy(
                from = src,
                to = dest,
                createFolders = true,
                followLinks = false
              )
            }
          }

          // copy overrides
          for {
            (test_name, test_sp) <- override_tests.toSeq
          } {
            for (ext <- test_extensions) {
              val test_id = TestId(
                external_name = test_name,
                internal_name = test_name
              )
              val test_info = TestInfo(test_id, test_sp.copy(data = ()))
              info.update(test_id, test_info)
              os.copy.over(
                from = test_sp.path / s"$test_name.$ext",
                to = dir / s"$test_name.$ext",
                createFolders = true,
                followLinks = false
              )
            }
          }

          info.to(SortedMap)

        }

    }

  lazy val test_ids: Maker[SortedSet[TestId]] =
    Rule(publish_tests, scope) { tests =>
      tests.data.keySet
    }

  lazy val phase1_test_ids: Maker[SortedSet[TestId]] =
    Rule(test_ids *: phase1_tests, scope) { case (tests, chosen) =>
      for {
        test <- tests
        if chosen.contains(test.external_name) || chosen.contains(
          test.internal_name
        )
      } yield test
    }

  lazy val phase2_test_ids: Maker[SortedSet[TestId]] =
    Rule(test_ids *: phase2_tests, scope) { case (tests, chosen) =>
      for {
        test <- tests
        if chosen.contains(test.external_name) || chosen.contains(
          test.internal_name
        )
      } yield test
    }

  def test_info(test_id: TestId): Maker[SignedPath[TestInfo]] =
    SignedPath.rule(
      publish_tests *: test_extensions,
      SortedSet(),
      scope / test_id.external_name / test_id.internal_name
    ) { case (dir, (tests, test_extensions)) =>
      os.remove.all(dir)
      tests.data.get(test_id) match {
        case Some(info) =>
          copy_test(info, tests.path, dir, test_extensions)
          info
        case None =>
          throw Exception(s"no info for $test_id")
      }
    }

  private def csid_has_test(csid: CSID): Maker[Boolean] =
    Rule(csids_with_tests, scope / csid.value)(_.contains(csid))

  val tests_repo_name = s"${course.course_name}_${project_name}__tests"

  ///////////////////////////
  // Test Case Submissions //
  //////////////////////////

  lazy val publish_submitted_tests
      : Maker[SignedPath[SortedMap[TestId, TestInfo]]] =
    SignedPath.rule(
      Gitolite.repo_info(
        submitted_tests_repo_name
      ) *: student_tests_by_csid *: publish_aliases *: test_extensions *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) {
      case (
            dir,
            (
              submitted_tests_repo_info,
              student_tests_by_csid,
              aliases,
              test_extensions,
              can_push_repo
            )
          ) =>
        submitted_tests_repo_info.update(
          path = dir,
          fork_from = Some("empty"),
          msg = "tests",
          readers = Seq(course.staff_group_name),
          writers = Seq(),
          can_push_repo
        ) { _ =>
          // delete all tests
          for {
            f <- os.list(dir)
            if !f.last.startsWith(".")
          } {
            os.remove.all(f)
          }

          val info = mutable.Map[TestId, TestInfo]()

          // copy student tests
          for {
            (csid, test_sp) <- student_tests_by_csid.toSeq
            alias <- aliases.data.get(csid).toSeq
          } {
            for (ext <- test_extensions) {
              val test_id = TestId(
                external_name = alias.toString,
                internal_name = csid.toString
              )
              val test_info = TestInfo(test_id, test_sp.copy(data = ()))
              info.update(test_id, test_info)
              val src = test_sp.path / s"${csid.value}.$ext"
              val dest = dir / s"$alias.$ext"
              os.copy(
                from = src,
                to = dest,
                createFolders = true,
                followLinks = false
              )
            }
          }

          info.to(SortedMap)

        }

    }

  val submitted_tests_repo_name =
    s"${course.course_name}_${project_name}__submitted_tests"

  ////////////
  // Scores //
  ////////////

  def compute_scores(
      csid: CSID,
      cutoff_time: CutoffTime,
      n: Int,
      commit_id_file: String,
      phase: Int
  ): Maker[SortedSet[TestId]] = Rule(
    for {
      ids <-
        (if (phase == 1) phase1_test_ids
         else if (phase == 2) phase2_test_ids
         else test_ids)
      outs <- Maker.sequence(
        ids.toSeq.map(id => run_one(csid, cutoff_time, id, n, commit_id_file))
      )
    } yield outs,
    scope / csid.value / cutoff_time.label / n.toString / commit_id_file
  ) { outs =>
    (for {
      sp <- outs
      d = sp.data
      if d.outcome.contains(OutcomeStatus.pass)
    } yield d.test_id).to(SortedSet)
  }

  def compute_scores(
      cutoff_time: CutoffTime,
      n: Int,
      commit_id_file: String,
      phase: Int
  ): Maker[SortedMap[CSID, SortedSet[TestId]]] = Rule(
    for {
      csids <- students_with_submission
      pairs <- Maker.sequence(
        csids.toSeq.map(csid =>
          compute_scores(csid, cutoff_time, n, commit_id_file, phase).map(g =>
            (csid, g)
          )
        )
      )
    } yield pairs,
    scope / cutoff_time.label / n.toString / commit_id_file
  ) { pairs =>
    pairs.to(SortedMap)
  }
}

object Project {
  given Ordering[Project] = Ordering.by(p => (p.course, p.project_name))

  // The output of time's %E format, output by the Makefile build system for test runtimes
  val TimeFormat: Regex = """(?:(\d+):)?(\d+):(\d+\.\d+)""".r

  private val run_locks = TrieMap[os.Path, ReentrantLock]()

  private def run_lock[A](path: os.Path)(f: => A): A = {
    val lock = run_locks.getOrElseUpdate(path, new ReentrantLock())
    try {
      lock.lock()
      f
    } finally {
      lock.unlock()
    }
  }

  private lazy val all_projects: Maker[Seq[Project]] = Rule(
    for {
      cs: Seq[Course] <- Course.all
      pms: Seq[SortedMap[String, Project]] <- Maker.sequence(cs.map(_.projects))
    } yield (for {
      pm <- pms
      (_, p) <- pm.toSeq
    } yield p).toList.sorted,
    null
  )(identity)

  lazy val active_projects: Maker[Seq[Project]] =
    Maker.select(all_projects)(_.active)

  private val automatic_override_names: Set[String] =
    Set("Makefile", "makefile")

  lazy val uid: String = os.proc("id", "-u").call().out.lines().head
  lazy val gid: String = os.proc("id", "-g").call().out.lines().head
}
