package ag.grader

import ag.rules.{
  down,
  Maker,
  Rule,
  SignedPath,
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedSet,
  lines,
  run,
  say,
  Optional
}
import upickle.default.ReadWriter

import java.time.LocalDateTime
import scala.collection.{mutable, SortedMap, SortedSet}
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import scala.collection.concurrent.TrieMap
import java.util.concurrent.atomic.AtomicLong
import java.time.{Duration, Instant, ZonedDateTime, ZoneId, ZoneOffset}
import java.time.format.DateTimeFormatter

// TODO: introduce a better abstraction
val limit = Runtime.getRuntime.nn.availableProcessors() - 1
val governor = new Semaphore(limit)

@upickle.implicits.allowUnknownKeys(false)
case class Weight(
    pattern: String,
    weight: Int
) derives ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class RawProject(
    active: Boolean,
    cores: Int = 4,
    code_cutoff: LocalDateTime,
    test_cutoff: LocalDateTime,
    ignore_tests: Boolean,
    chosen: Seq[String],
    bad_tests: Seq[String],
    docker_file: Option[String],
    test_extensions: Seq[String],
    weights: Seq[Weight],
    staff: Seq[CSID]
) derives ReadWriter

enum CutoffTime:
  case Manual(cutoff: ZonedDateTime)
  case Default
  case None

  def label: String = this match
    case Manual(time) =>
      time
        .withZoneSameInstant(ZoneId.systemDefault)
        .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
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

  lazy val chosen: Maker[SortedSet[String]] =
    Rule(info, scope)(p => p.chosen.to(SortedSet))

  lazy val weights: Maker[Seq[Weight]] =
    Rule(info, scope)(p => p.weights)

  lazy val bad_tests: Maker[SortedSet[String]] =
    Rule(info, scope)(p => SortedSet(p.bad_tests*))

  lazy val cores: Maker[Int] =
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

  lazy val project_repo_name: String = s"${course.course_name}_${project_name}"

  lazy val project_repo: Maker[SignedPath[Boolean]] =
    Gitolite.mirror(project_repo_name)

  lazy val staff: Maker[SortedSet[CSID]] =
    Rule(info, scope) { p =>
      SortedSet(p.staff*)
    }

  ///////////////////
  /* Override Repo */
  ///////////////////

  lazy val override_repo_name = s"${project_repo_name}__override"

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

  /** *********************
    */
  /* Work repo (per csid) */
  /** *********************
    */

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

  def submission(csid: CSID): Maker[Option[SignedPath[Unit]]] =
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

  def has_submission(csid: CSID): Maker[Boolean] =
    Rule(students_with_submission, scope / csid.value)(_.contains(csid))

  val report_name = "REPORT.txt"

  val initial_report = SignedPath.rule(
    project_repo,
    SortedSet(".git"),
    scope
  ) { case (dir, (project_repo)) =>
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

  def student_report(csid: CSID) = SignedPath.rule(
    submission(csid),
    SortedSet(".git"),
    scope / csid.value
  ) { case (dir, (submission)) =>
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

  def has_student_report(csid: CSID) = Rule(
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
      if src.map(p => os.exists(p)).getOrElse(false)
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
      cutoff: CutoffTime
  ): Maker[SignedPath[Option[PrepareInfo]]] =
    SignedPath.rule(
      submission(csid) *: has_student_report(
        csid
      ) *: publish_override_repo *: test_extensions *: code_cutoff,
      SortedSet(".git"),
      scope / csid.value / cutoff.label
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

        val default_branch = "master"

        // (2) what commit should we use
        val commit_id_file = submission_repo.path / "commit_id"
        val cutoff_time = cutoff match
          case CutoffTime.Manual(cutoff_time) => Some(cutoff_time)
          case CutoffTime.Default =>
            Some(ZonedDateTime.of(code_cutoff, ZoneId.systemDefault))
          case CutoffTime.None => None
        val commit_id = if (os.exists(commit_id_file)) {
          os.read.lines(commit_id_file).head.trim.nn
        } else {
          cutoff_time match
            case Some(cutoff_time) => {
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
            }
            case None => default_branch
        }

        // (3) checkout the correct commit_id
        val _ = os.proc("git", "checkout", commit_id).run(cwd = dir)

        val git_sha =
          os.proc("git", "rev-parse", "HEAD").lines(cwd = dir).head.trim.nn
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

        // (7) mark if prepared commit is after the specified deadline
        cutoff_time match
          case Some(cutoff_time) =>
            if (zdt.isAfter(cutoff_time)) {
              os.write(dir / "late", "")
            }
          case None => None

        Some(
          PrepareInfo(
            commit_time = zdt,
            sha = git_sha,
            has_report = has_student_report
          )
        )

      case (_, (None, _, _, _, _)) =>
        None

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
          case Some(cutoff_time) => {
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
          }
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
  }

  def empty_run(
      csid: CSID,
      cutoff: CutoffTime,
      test_id: TestId
  ): Maker[SignedPath[Outcome]] =
    SignedPath.rule(
      prepare(csid, cutoff),
      SortedSet(),
      scope / csid.value / cutoff.label / test_id.external_name / test_id.internal_name / "0"
    ) { case (out_path, _) =>
      Outcome(
        this,
        csid,
        test_id,
        None,
        None,
        tries = 0
      )
    }

  // The output of time's %E format, output by the Makefile build system for test runtimes
  private val TimeFormat = """(?:(\d+):)?(\d+):(\d+\.\d+)""".r

  def run(
      csid: CSID,
      cutoff: CutoffTime,
      test_id: TestId,
      n: Int,
      run_all: Boolean = false
  ): Maker[SignedPath[Outcome]] = {

    val rest =
      os.RelPath(
        csid.value
      ) / cutoff.label / test_id.external_name / test_id.internal_name / n.toString
    val the_scope = if (run_all) scope / "_all_" / rest else scope / rest

    SignedPath.rule(
      test_info(test_id) *: test_extensions *: prepare(
        csid,
        cutoff
      ) *: cores *: (if (n == 1)
                       empty_run(csid, cutoff, test_id)
                     else
                       run(csid, cutoff, test_id, n - 1, run_all)),
      SortedSet(),
      the_scope
    ) { case (out_path, (test_info, test_extensions, prepared, cores, prev)) =>
      if (
        run_all || (n == 1) || (prev.data.outcome == Some(OutcomeStatus.Pass))
      ) {
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
                s"$tn/${test_id.internal_name} for ${course.course_name}_${project_name}_${csid}"
              say(f"running#$n $m on $cores cores")

              val start = System.currentTimeMillis()
              var run_time: Option[Double] = None
              val (rc, stdout, stderr) =
                try {
                  val _ = os
                    .proc("make", "-k", "clean")
                    .run(cwd = prepared.path, check = false)
                  os.proc("make", "-k", s"$tn.test")
                    .run(cwd = prepared.path, check = false)
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
                case Some(TimeFormat(Optional(None), min, sec)) =>
                  Some(min.toDouble * 60 + sec.toDouble)
                case Some(TimeFormat(Optional(Some(hours)), min, sec)) =>
                  Some(hours.toDouble * 3600 + min.toDouble * 60 + sec.toDouble)
                case Some("timeout") => None
                case _               => None

              val outcome = (outcome_str, qemu_runtime_str) match
                case (_, Some("timeout")) => OutcomeStatus.Timeout
                case (Some("pass"), _)    => OutcomeStatus.Pass
                case (Some("fail"), _)    => OutcomeStatus.Fail
                case (_, _)               => OutcomeStatus.Unknown

              val how_long = run_time.map(t => f"$t%.2f")
              val out = outcome match
                case OutcomeStatus.Pass    => fansi.Color.Green("pass")
                case OutcomeStatus.Fail    => fansi.Color.Red("fail")
                case OutcomeStatus.Timeout => fansi.Color.Red("timeout")
                case OutcomeStatus.Unknown => fansi.Color.Red("???")

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
                tries = n
              )
            }
          }
        } finally {
          finished_runs.incrementAndGet()
        }
      } else {
        copy_results(prev.path, out_path, test_id)
        prev.data
      }
    }
  }

  def student_results_repo_name(csid: CSID): String =
    s"${course.course_name}_${project_name}_${csid.value}_results"

  def publish_student_results(
      csid: CSID,
      n: Int
  ): Maker[SignedPath[StudentResults]] =
    SignedPath.rule(
      prepare(csid, CutoffTime.None) *:
        Gitolite.repo_info(student_results_repo_name(csid)) *:
        test_ids.flatMapSeq(test_id =>
          run(csid, CutoffTime.None, test_id, n)
        ) *:
        csid_to_alias(csid) *:
        csid_has_test(csid) *:
        course.notifications.peek *: Config.can_push_repo,
      SortedSet(".git"),
      scope / csid.value / n.toString
    ) {
      case (
            dir,
            (
              prepared,
              student_results_repo,
              outcomes,
              alias,
              has_test,
              notifications,
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

  def notify_student_results(csid: CSID) =
    Rule(
      get_student_results(
        csid
      ) *: course.notifications *: Config.gitolite *: work_repo(
        csid
      ) *: Config.can_send_mail,
      scope / csid.value
    ) {
      case (Some(sr), n, g, work_repo, can_send_mail) =>
        if (os.exists(work_repo.path / "spamon")) {
          n.send_result_update(this, csid, g, sr, can_send_mail)
        } else {
          say(s"no spamon for ${course.course_name}:${project_name}:$csid")
        }
      case (None, _, _, _, _) =>
    }

  lazy val project_results_repo_name =
    s"${course.course_name}_${project_name}__results"

  def publish_results(
      n: Int
  ): Maker[SignedPath[SortedMap[Alias, RedactedStudentResults]]] =
    SignedPath.rule(
      Gitolite.repo_info(project_results_repo_name) *:
        students_with_submission.flatMapSeq((csid: CSID) =>
          publish_student_results(csid, n).map(r => (csid, r))
        ) *:
        publish_aliases *: Config.can_push_repo,
      SortedSet(".git"),
      scope / n.toString
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

  /** **************
    */
  /* student tests */
  /** **************
    */

  def student_test(csid: CSID): Maker[SignedPath[Option[String]]] =
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
        //os.write(dir / ".rev", rev)
        os.remove.all(dir / ".git")
        if (out) Some(rev) else None
      case (dir, (None, _, _)) =>
        os.remove.all(dir)
        os.makeDir.all(dir)
        None
    }

  lazy val student_tests_by_csid: Maker[SortedMap[CSID, SignedPath[String]]] =
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

  lazy val csids_with_tests: Maker[SortedSet[CSID]] =
    Rule(student_tests_by_csid, scope) { student_tests =>
      student_tests.keySet
    }

  //////////////////
  // Aliases repo //
  //////////////////

  lazy val aliases_repo_name: String =
    s"${course.course_name}_${project_name}__aliases"

  lazy val publish_aliases: Maker[SignedPath[SortedMap[CSID, Alias]]] =
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

  def csid_to_alias(csid: CSID): Maker[Option[Alias]] =
    Rule(publish_aliases, scope / csid.value) { aliases =>
      aliases.data.get(csid)
    }

  def alias_to_csid(alias: Alias): Maker[Option[CSID]] =
    Rule(publish_aliases, scope / alias.toString) { aliases =>
      aliases.data.find(_._2 == alias).map(_._1)
    }

  /** ******
    */
  /* Tests */
  /** ******
    */

  lazy val override_test_names: Maker[SortedSet[String]] =
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

  def override_test(test_name: String): Maker[SignedPath[String]] =
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

  lazy val override_tests: Maker[SortedMap[String, SignedPath[String]]] =
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
      ) *: student_tests_by_csid *: publish_aliases *: override_tests *: test_extensions *: bad_tests *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) {
      case (
            dir,
            (
              tests_repo_info,
              student_tests_by_csid,
              aliases,
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
              val test_info = TestInfo(test_id, test_sp.copy(data=()))
              info.update(test_id, test_info)
              val src = test_sp.path / s"${csid.value}.$ext"
              val dest = dir / s"${alias}.$ext"
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
              val test_info = TestInfo(test_id, test_sp.copy(data=()))
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

  def csid_has_test(csid: CSID): Maker[Boolean] =
    Rule(csids_with_tests, scope / csid.value)(_.contains(csid))

  val tests_repo_name = s"${course.course_name}_${project_name}__tests"

}

object Project {
  given Ordering[Project] = Ordering.by(p => (p.course, p.project_name))

  private val run_locks = TrieMap[os.Path, ReentrantLock]()

  def run_lock[A](path: os.Path)(f: => A): A = {
    val lock = run_locks.getOrElseUpdate(path, new ReentrantLock())
    try {
      lock.lock()
      f
    } finally {
      lock.unlock()
    }
  }

  lazy val all_projects: Maker[Seq[Project]] = Rule(
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

  val automatic_override_names = Set("Makefile", "makefile")
}
