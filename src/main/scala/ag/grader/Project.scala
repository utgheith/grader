package ag.grader

import ag.common.{
  NoNulls,
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.r2.{
  Many,
  periodic,
  Scope,
  Target,
  ToRelPath,
  WithData,
  create_data,
  Result,
  run_if_needed,
  say,
  shout,
  update_data
}
import ag.rules.{lines, run}
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
import scala.util.control.NonFatal
import scala.util.matching.Regex

// TODO: introduce a better abstraction
val limit = Runtime.getRuntime.nn.availableProcessors() - 1
val governor = new Semaphore(limit)

// The commit a submission is graded at: the latest on master, or -- for
// results as of a phase, once its deadline has passed -- the last one on
// master before that phase's deadline, unless the phase's commit id file
// (commit_id_<n>) names another.
enum Phase derives ReadWriter:
  case Latest, One, Two

  // the file in a submission that can name the commit to grade, if any
  def commit_id_file: Option[String] = this match
    case Latest => None
    case One    => Some("commit_id_1")
    case Two    => Some("commit_id_2")

  // the phase's results, in the project's results repo
  def results_file: String = this match
    case Latest => "results.json"
    case One    => "results_phase1.json"
    case Two    => "results_phase2.json"

  // the commits graded are those before this, if anything
  def deadline(test_cutoff: LocalDateTime, code_cutoff: LocalDateTime) =
    this match
      case Latest => None
      case One    => Some(test_cutoff)
      case Two    => Some(code_cutoff)

object Phase:
  given ToRelPath[Phase] = {
    case Phase.Latest => os.RelPath("latest")
    case Phase.One    => os.RelPath("phase1")
    case Phase.Two    => os.RelPath("phase2")
  }

private val started_runs = AtomicLong(0)
private val finished_runs = AtomicLong(0)

@upickle.implicits.allowUnknownKeys(false)
case class Project(course: Course, project_name: String)
    extends GitoliteScope(course / project_name) derives ReadWriter {

  lazy val info: Target[RawProject] =
    Gitolite.raw_project(course.course_name, project_name)

  lazy val docker_file: Target[Option[String]] = target(
    (
      info = info
    )
  ) { (_, deps) =>
    deps.info.docker_file
  }

  lazy val docker_image: Target[Option[String]] =
    target((docker_file = docker_file)) { (_, deps) =>
      deps.docker_file.map(fn => Docker.of(os.pwd / fn))
    }

  lazy val active: Target[Boolean] =
    target(
      (
        active = course.active,
        info = info,
        repo_info = Gitolite.repo_info(project_repo_name)
      )
    ) {
      case (_, (course_is_active, info, RepoInfo(_, _, Some(_)))) =>
        course_is_active && info.active
      case (_, (_, _, RepoInfo(_, _, None))) =>
        false
    }

  lazy val phase1_tests: Target[SortedSet[String]] =
    target((p = info))((_, deps) => deps.p.phase1_tests.to(SortedSet))

  lazy val phase1_weight: Target[Int] =
    target((p = info))((_, deps) => deps.p.phase1_weight)

  lazy val phase2_tests: Target[SortedSet[String]] =
    target((p = info))((_, deps) => deps.p.phase2_tests.to(SortedSet))

  lazy val phase2_weight: Target[Int] =
    target((p = info))((_, deps) => deps.p.phase2_weight)

  lazy val test_weights: Target[Seq[Weight]] =
    target((p = info))((_, deps) => deps.p.test_weights)

  lazy val bad_tests: Target[SortedSet[String]] =
    target((p = info))((_, deps) => SortedSet(deps.p.bad_tests*))

  private lazy val cores: Target[Int] =
    target((p = info))((_, deps) => deps.p.cores)

  lazy val iterations: Target[Iterations] =
    target((p = info))((_, deps) => deps.p.iterations)

  lazy val test_cutoff: Target[LocalDateTime] =
    target((p = info))((_, deps) => deps.p.test_cutoff)
  lazy val code_cutoff: Target[LocalDateTime] =
    target((p = info))((_, deps) => deps.p.code_cutoff)
  lazy val extensions: Target[Extensions] =
    target((p = info))((_, deps) => deps.p.extensions)

  // `csid`'s deadline for grading their code in `phase`, with any extension
  // they have: phase 1's is the test cutoff, phase 2's the code cutoff; the
  // latest results are late from the code cutoff too. A student's test case is
  // due at the test cutoff, whatever their extension (see student_test).
  lazy val deadline: (CSID, Phase) => Target[LocalDateTime] = fun {
    (csid, phase) =>
      target(
        (first = test_cutoff, second = code_cutoff, extensions = extensions)
      ) { (_, deps) =>
        phase
          .deadline(deps.first, deps.second)
          .getOrElse(deps.second)
          .plusDays(deps.extensions.days(csid, phase))
          .nn
      }
  }
  lazy val test_extensions: Target[SortedSet[String]] =
    target((p = info))((_, deps) => SortedSet(deps.p.test_extensions*))

  private lazy val project_repo_name: String =
    s"${course.course_name}_$project_name"

  private lazy val project_repo: Target[WithData[Boolean]] =
    Gitolite.mirror(project_repo_name)

  lazy val staff: Target[SortedSet[CSID]] =
    target((p = info)) { (_, deps) =>
      SortedSet(deps.p.staff*)
    }

  ///////////////////
  // Override Repo //
  ///////////////////

  private lazy val override_repo_name = s"${project_repo_name}__override"

  lazy val publish_override_repo: Target[WithData[Unit]] =
    gitolite_target(
      repo = override_repo_name,
      msg = "update override",
      readers = Seq("@all"),
      writers = Seq(course.staff_group_name)
    )(
      (
        test_extensions = test_extensions,
        project_repo = project_repo
      )
    ) { (producer, deps, repo) =>
      val initialized_marker = repo.dir / ".initialized"

      if (!os.exists(initialized_marker)) {

        /* find the original tests */
        for {
          f <- os.list(deps.project_repo.get_data_path(producer))
          if !f.last.startsWith(".")
          if deps.test_extensions.contains(f.ext)
        } {
          val target =
            repo.dir / f.relativeTo(deps.project_repo.get_data_path(producer))
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
          .stream(
            path = deps.project_repo.get_data_path(producer),
            followLinks = false
          )
          .foreach { f =>
            if (
              os.isFile(f) && Project.automatic_override_names.contains(
                f.last
              )
            ) {
              val target =
                repo.dir / f.relativeTo(
                  deps.project_repo.get_data_path(producer)
                )
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

  //////////////////////////
  // Work repo (per csid) //
  //////////////////////////

  def work_repo_name(csid: CSID): String = s"${project_repo_name}_${csid.value}"

  def work_repo(csid: CSID): Target[WithData[Boolean]] =
    Gitolite.mirror(work_repo_name(csid))

  lazy val publish_work_repo: CSID => Target[WithData[Unit]] = fun {
    (csid: CSID) =>
      gitolite_target(
        repo = work_repo_name(csid),
        msg = "create",
        fork_from = Some(project_repo_name),
        readers = Seq(csid.value, course.staff_group_name),
        writers = Seq(csid.value)
      )(
        (
          n = course.notifications,
          can_send_mail = Config.can_send_mail
        )
      ) { (producer, deps, repo) =>
        if (repo.forked) {
          repo.after_push {
            shout(
              producer,
              s"sending notification to ${csid.toString}, can_send_mail is ${deps.can_send_mail}"
            )
            deps.n.send_repo_created(
              this,
              repo.info,
              csid,
              deps.can_send_mail,
              repo.dir
            )
          }
        }
      }
  }

  private lazy val submission: CSID => Target[Option[WithData[Unit]]] = fun {
    (csid: CSID) =>
      target((work_repo = work_repo(csid), project_repo = project_repo)) {
        (_, deps) =>
          if (
            (!deps.work_repo.value) || (deps.work_repo.data_signature == deps.project_repo.data_signature)
          ) {
            None
          } else {
            Some(deps.work_repo.copy(value = ()))
          }
      }
  }

  lazy val submissions: Target[SortedMap[CSID, WithData[Unit]]] =
    complex_target { tracker =>
      val csids: Seq[CSID] = tracker.track(course.enrolled_csids).toSeq
      val submissions: Seq[Option[WithData[Unit]]] =
        tracker.track_all(csids.map(submission))

      run_if_needed(tracker) { _ =>
        (for {
          (csid, submission) <- csids.zip(submissions)
          if submission.isDefined
        } yield (csid, submission.get)).to(SortedMap)
      }

    }

  lazy val students_with_submission: Target[SortedSet[CSID]] =
    target((s = submissions))((_, deps) => deps.s.keySet)

  private val report_name: String = "REPORT.txt"

  private val initial_report: Target[WithData[Unit]] = target(
    (
      project_repo = project_repo
    )
  ) { (producer, deps) =>
    create_data(producer)(skip = _ => false) { dir =>
      val src = deps.project_repo.get_data_path(producer) / report_name
      val dest = dir / report_name
      if (deps.project_repo.value && os.isFile(src)) {
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
  }

  private lazy val student_report: CSID => Target[WithData[Unit]] = fun {
    csid =>
      target(
        (
          submission = submission(csid)
        )
      ) { (producer, deps) =>
        val submission = deps.submission
        create_data(producer)(_ => false) { dir =>
          val dest = dir / report_name
          os.remove.all(dest)
          submission.foreach { submission =>
            val src = submission.get_data_path(producer) / report_name
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
      }
  }

  private lazy val has_student_report: CSID => Target[Boolean] = fun { csid =>
    target(
      (initial_report = initial_report, student_report = student_report(csid))
    ) { (_, deps) =>
      deps.initial_report.data_signature != deps.student_report.data_signature
    }
  }

  def copy_test(
      test_id: TestId,
      from: os.Path,
      to: os.Path,
      test_extensions: SortedSet[String]
  ): Unit = {
    val name = test_id.external_name
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

  // The commit graded for `phase`: the one its commit id file names, if it
  // names one; else the last on master before the phase's deadline (by commit
  // time; --first-parent skips commits merged in from other branches after
  // it), or master itself for Phase.Latest. None if there's no submission,
  // or no commit before the deadline. The commit id file is read as it is
  // now, not as of the deadline, on purpose: a student can change their mind
  // later and ask for a different commit to be graded.
  lazy val graded_commit: (CSID, Phase) => Target[Option[GradedCommit]] =
    fun { (csid, phase) =>
      target(
        (submission_repo = submission(csid), deadline = deadline(csid, phase))
      ) { (producer, deps) =>
        val submission_repo = deps.submission_repo
        submission_repo.flatMap { submission_repo =>
          val repo = submission_repo.get_data_path(producer)
          def git(args: String*): Option[String] = {
            val r = os
              .proc("git" +: args)
              .call(cwd = repo, check = false, stderr = os.Pipe)
            Option.when(r.exitCode == 0)(r.out.trim()).filter(_.nonEmpty)
          }
          def commit(rev: String): Option[String] =
            git("rev-parse", "-q", "--verify", s"$rev^{commit}")

          val named = for {
            name <- phase.commit_id_file
            file = repo / name
            if os.isFile(file)
            id <- os.read.lines(file).headOption.map(_.trim.nn)
            if id.nonEmpty
            sha <- {
              val sha = commit(id)
              if (sha.isEmpty)
                say(producer, s"$name names no commit ('$id'): ignored")
              sha
            }
          } yield (sha, CommitSource.FromFile(name, id))
          val chosen = named.orElse {
            val sha =
              if (phase == Phase.Latest) commit("master")
              else
                git(
                  "log",
                  "master",
                  "-n",
                  "1",
                  "--before",
                  ZonedDateTime
                    .of(deps.deadline, ZoneId.systemDefault)
                    .nn
                    .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                  "--first-parent",
                  "--pretty=format:%H"
                )
            sha.map(_ -> CommitSource.Latest)
          }
          chosen.map { (sha, source) =>
            GradedCommit(
              sha = sha,
              source = source,
              commit_time = ZonedDateTime.ofInstant(
                Instant.ofEpochSecond(
                  git("show", "-s", "--format=%ct", sha).get.toLong
                ),
                ZoneOffset.UTC
              ),
              // the server's push hook notes it, in refs/notes/push_time
              // (older repos: "PushTime: <secs>" lines in the default notes)
              push_time = PrepareInfo.push_time_from(
                (git("notes", "--ref", "push_time", "show", sha).toSeq ++
                  git("show", "-s", "--format=%N", sha).toSeq)
                  .flatMap(_.linesIterator)
              )
            )
          }
        }
      }
    }

  // A commit's code, ready to test: the course's overrides on top, its tests
  // and git history removed. Keyed by sha, so every phase grading the same
  // commit shares it (and its runs).
  lazy val prepare: (CSID, String) => Target[WithData[Unit]] =
    fun { (csid, sha) =>
      target(
        (
          submission_repo = submission(csid),
          override_repo = publish_override_repo,
          test_extensions = test_extensions
        )
      ) { (producer, deps) =>
        create_data(producer)(_ => false) { dir =>
          val repo = deps.submission_repo.getOrElse(
            throw IllegalStateException(s"${csid.value} has no submission")
          )
          // (1) the commit
          val _ = os
            .proc(
              "git",
              "clone",
              "--shared",
              "--template=",
              "--no-checkout",
              repo.get_data_path(producer),
              dir
            )
            .run()
          val _ = os.proc("git", "checkout", sha).run(cwd = dir)
          val commit_time =
            os.proc("git", "show", "-s", "--format=%cI")
              .call(cwd = dir)
              .out
              .trim()

          // (2) override
          os.copy(
            deps.override_repo.get_data_path(producer),
            dir,
            followLinks = false,
            replaceExisting = true,
            createFolders = true,
            mergeFolders = true
          )

          // (3) remove all tests
          for {
            p <- os.list(dir)
            if deps.test_extensions.contains(p.ext)
          } os.remove.all(p)

          // (4) remove .git
          os.remove.all(dir / ".git")

          // (5) which commit this is
          os.write.over(dir / "prepared_commit", s"$sha\n$commit_time\n")
        }
      }
    }

  // What's graded for `phase`, and how it was chosen. None: nothing to grade.
  lazy val prepare_info: (CSID, Phase) => Target[Option[PrepareInfo]] = fun {
    (csid, phase) =>
      target(
        (
          graded = graded_commit(csid, phase),
          has_report = has_student_report(csid)
        )
      ) { (_, deps) =>
        deps.graded.map(g =>
          PrepareInfo(
            commit_time = g.commit_time,
            sha = g.sha,
            has_report = deps.has_report,
            push_time = g.push_time,
            commit_source = g.source
          )
        )
      }
  }

  lazy val is_late: (CSID, Phase) => Target[Boolean] = fun { (csid, phase) =>
    target(
      (info = prepare_info(csid, phase), deadline = deadline(csid, phase))
    ) { (_, deps) =>
      deps.info.forall(
        _.commit_time.isAfter(
          ZonedDateTime.of(deps.deadline, ZoneId.systemDefault())
        )
      )
    }
  }

  // The commits on master after the student's (phase 2) deadline.
  lazy val late_commits: CSID => Target[WithData[Seq[LateCommit]]] = fun {
    csid =>
      target(
        (
          submission_repo = submission(csid),
          code_cutoff = deadline(csid, Phase.Two)
        )
      ) { (producer, deps) =>
        val submission_repo = deps.submission_repo
        create_data(producer)(_.lastOpt.contains(".git")) { dir =>
          submission_repo.toSeq.flatMap { submission_repo =>
            val _ = os
              .proc(
                "git",
                "clone",
                "--shared",
                "--template=",
                "--no-checkout",
                submission_repo.get_data_path(producer),
                dir
              )
              .run()

            val default_branch = "master"

            val cutoff_time =
              ZonedDateTime.of(deps.code_cutoff, ZoneId.systemDefault).nn
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
              val (hash, commit_timestamp, message) =
                line.split("\u0000") match
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
        }
      }
  }

  def copy_results(
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
    one("stdout")
    one("stderr")
  }

  // Run a submission/test combination once and report the outcome
  def run_one(
      keep_going: Boolean,
      n: Int
  ): (CSID, String, TestId) => Target[WithData[Outcome]] = fun {
    (csid, sha, test_id) =>
      complex_target { tracker =>
        val deps = tracker.track_all(
          (
            test_info = test_info(test_id),
            test_extensions = test_extensions,
            prepared = prepare(csid, sha),
            cores = cores,
            docker_image = docker_image
          )
        )

        tracker.run { (producer, old_state) =>
          val (cs, history) = old_state match {
            case cs @ Some(
                  Result(WithData(v, _, _), _)
                ) =>
              (Some(cs), v.outcomes)
            case _ =>
              (None, Seq())
          }
          cs match {
            case Some(cs)
                if history.size >= n || !(keep_going || history.forall(
                  _._1 == OutcomeStatus.pass
                )) =>
              cs
            case _ => { () =>
              val (test_info, test_extensions, prepared, cores, docker_image) =
                deps
              update_data(producer)(_ => false) { dir =>
                // only the files from the latest run are kept
                os.remove.all(dir)
                os.makeDir.all(dir)

                val res = doit(
                  project = this,
                  csid = csid,
                  cores = cores,
                  prepared_path = prepared.get_data_path(producer),
                  test_id = test_info.value.id,
                  test_path = test_info.get_data_path(producer),
                  test_extensions = test_extensions,
                  n = history.size + 1,
                  out_path = dir,
                  docker_image = docker_image
                )

                Outcome(
                  project = this,
                  test_id = test_id,
                  csid = csid,
                  commit_id = Some(sha),
                  outcomes = history :+ res
                )
              }
            }
          }
        }
      }
  }

  def student_results_repo_name(csid: CSID): String =
    s"${course.course_name}_${project_name}_${csid.value}_results"

  // A student's results for `phase`: their graded commit for the phase, run
  // on `tests`. None if there's nothing to grade.
  private def student_results(
      keep_going: Boolean,
      tests: TestSet
  ): (CSID, Phase, Int) => Target[Option[StudentResults]] = fun {
    (csid, phase, n) =>
      complex_target { tracker =>
        val info = tracker.track(prepare_info(csid, phase))
        val runs = for {
          info <- info.toSeq
          id <- tracker.track(test_ids_for(tests)).toSeq
        } yield run_one(keep_going, n)(csid, info.sha, id)
        val deps = tracker.track_all(
          (
            outcomes = Many(runs),
            alias = csid_to_alias(csid),
            has_test = csid_has_test(csid)
          )
        )
        run_if_needed(tracker) { _ =>
          info.map(info =>
            StudentResults(
              csid = csid,
              alias = deps.alias,
              has_test = deps.has_test,
              prepare_info = info,
              // keyed by alias: a student test's internal name is its
              // author's CSID
              outcomes = deps.outcomes
                .map(o => o.value.test_id.redacted -> o.value.redacted)
                .to(SortedMap)
            )
          )
        }
      }
  }

  // A student's latest results, with each test's output, in their own results
  // repo (which the result emails are made from).
  private def publish_student_results(
      keep_going: Boolean,
      tests: TestSet
  ): (CSID, Int) => Target[WithData[Option[StudentResults]]] = fun {
    (csid, n) =>
      complex_gitolite_target(
        repo = student_results_repo_name(csid),
        msg = "updated results",
        readers = Seq(csid.value, course.staff_group_name)
      ) { tracker =>
        val results =
          tracker.track(
            student_results(keep_going, tests)(csid, Phase.Latest, n)
          )
        // the same runs, for their output files
        val runs = for {
          r <- results.toSeq
          id <- tracker.track(test_ids_for(tests)).toSeq
        } yield run_one(keep_going, n)(csid, r.prepare_info.sha, id)
        val deps = tracker.track_all(
          (
            outcomes = Many(runs),
            // rewrite the_results.json if it stops being readable
            readable = student_results_readable(csid)
          )
        )

        (producer, repo) => {
          for {
            f <- os.list(repo.dir)
            if !f.last.startsWith(".")
          } os.remove.all(f)
          for (outcome <- deps.outcomes)
            copy_results(
              outcome.get_data_path(producer),
              repo.dir,
              outcome.value.test_id
            )
          for (r <- results)
            os.write.over(
              repo.dir / "the_results.json",
              upickle.default.write(r)
            )
          results
        }
      }
  }

  // ---- Results published to git repos (results.json, the_results.json)
  //
  // These live outside the workspace, so the cache's rule (a saved value that
  // can't be read back is recomputed) doesn't reach them by itself. Instead,
  // each writer depends on whether its published file is readable by this
  // version of the grader: when that changes (say, a file in an older format
  // turns up), the writer runs again and rewrites it. Readers fail loudly on
  // a file they can't read rather than pass on partial results.

  private def readable[A: upickle.default.Reader](file: os.Path): Boolean =
    os.isFile(file) && Try(
      NoNulls.check(upickle.default.read[A](os.read(file)), file.toString)
    ).isSuccess

  private def read_published[A: upickle.default.Reader](
      file: os.Path,
      rewritten_by: String
  ): A =
    try NoNulls.check(upickle.default.read[A](os.read(file)), file.toString)
    catch {
      case NonFatal(e) =>
        throw new IllegalStateException(
          s"${file.toString} isn't readable by this version of the grader; " +
            s"`$rewritten_by` rewrites it (${e.getMessage})",
          e
        )
    }

  private lazy val student_results_readable: CSID => Target[Boolean] = fun {
    csid =>
      target((repo = Gitolite.mirror(student_results_repo_name(csid)))) {
        (producer, deps) =>
          deps.repo.value && readable[StudentResults](
            deps.repo.get_data_path(producer) / "the_results.json"
          )
      }
  }

  private lazy val results_readable: Phase => Target[Boolean] = fun { phase =>
    target((repo = Gitolite.mirror(project_results_repo_name))) {
      (producer, deps) =>
        deps.repo.value && readable[SortedMap[Alias, RedactedStudentResults]](
          deps.repo.get_data_path(producer) / phase.results_file
        )
    }
  }

  lazy val get_student_results: CSID => Target[Option[StudentResults]] =
    fun { csid =>
      target((sp = Gitolite.mirror(student_results_repo_name(csid)))) {
        (producer, deps) =>
          if (deps.sp.value) {
            val f = deps.sp.get_data_path(producer) / "the_results.json"

            if (os.isFile(f)) {
              Some(
                read_published[StudentResults](
                  f,
                  s"run publish_results -c ${course.course_name} -p $project_name -s ${csid.value}"
                )
              )
            } else {
              None
            }
          } else {
            None
          }
      }
    }

  private lazy val get_student_failures
      : CSID => Target[Option[StudentFailures]] = fun { csid =>
    target(
      (
        rs = get_student_results(csid)
      )
    ) { (_, deps) =>
      deps.rs.map { rdr =>
        StudentFailures(rdr)
      }
    }
  }

  private lazy val has_spamon: CSID => Target[Boolean] = fun { csid =>
    target((work_repo = work_repo(csid))) { (producer, deps) =>
      os.exists(deps.work_repo.get_data_path(producer) / "spamon")
    }
  }

  // Where the project is relative to its deadlines. Recomputed when a
  // deadline changes, and checked again every minute as time passes; what
  // depends on it reruns only when the phase actually changes.
  lazy val state: Target[ProjectState] =
    target(
      (
        first = test_cutoff,
        second = code_cutoff,
        periodic = periodic(60 * 1000)
      )
    ) { (_, deps) =>
      ProjectState.at(LocalDateTime.now().nn, deps.first, deps.second)
    }

  // Whether `csid`'s deadline for `phase` has passed. Checked again every
  // minute as time passes; what depends on it reruns only when it flips.
  lazy val past_deadline: (CSID, Phase) => Target[Boolean] = fun {
    (csid, phase) =>
      target(
        (deadline = deadline(csid, phase), periodic = periodic(60 * 1000))
      ) { (_, deps) =>
        !LocalDateTime.now().nn.isBefore(deps.deadline)
      }
  }

  // No result emails after the student's second deadline.
  lazy val notify_student_results: CSID => Target[Unit] = fun { csid =>
    target(
      (
        get_student_failures = get_student_failures(
          csid
        ),
        notifications = course.notifications,
        gitolite = Config.gitolite,
        has_spamon = has_spamon(
          csid
        ),
        can_send_mail = Config.can_send_mail,
        past_deadline = past_deadline(csid, Phase.Two)
      )
    ) {
      case (producer, (_, _, _, _, _, true)) =>
        say(
          producer,
          s"no result emails after the second deadline: ${course.course_name}:$project_name:${csid.toString}"
        )
      case (producer, (Some(sr), n, g, has_spamon, can_send_mail, _)) =>
        if (has_spamon) {
          n.send_result_update(this, csid, g, sr, can_send_mail)
        } else {
          say(
            producer,
            s"no spamon for ${course.course_name}:$project_name:${csid.toString}"
          )
        }
      case (_, (None, _, _, _, _, _)) =>
    }
  }

  lazy val project_results_repo_name: String =
    s"${course.course_name}_${project_name}__results"

  // Whether publishing `phase`'s results with `n` tries has tests to run, as
  // far as the published results tell: a student's graded commit isn't in
  // them, they don't cover the phase's tests, or a test has tries to go (as
  // run_one decides). Missing or unreadable results are work too. The state
  // picks the tests and keep-going, as it does for publish_results.
  lazy val has_work: (Phase, Int) => Target[Boolean] = fun { (phase, n) =>
    complex_target { tracker =>
      val state_v = tracker.track(state)
      val keep_going = state_v.keeps_going(phase)
      val students = tracker.track(students_with_submission).toSeq
      val deps = tracker.track_all(
        (
          published = results(phase),
          ids = test_ids_for(state_v.tests(phase)),
          infos = Many(students.map(prepare_info(_, phase))),
          aliases = Many(students.map(csid_to_alias))
        )
      )
      run_if_needed(tracker) { _ =>
        val ids = deps.ids.map(_.redacted)
        deps.published.toOption.flatten match {
          case None            => true
          case Some(published) =>
            deps.infos
              .zip(deps.aliases)
              .exists {
                // publish_results leaves out students without an alias
                case (Some(info), Some(alias)) =>
                  published.get(alias) match {
                    case None    => true
                    case Some(r) =>
                      r.prepare_info.sha != info.sha ||
                      r.outcomes.keySet != ids ||
                      r.outcomes.values.exists { o =>
                        o.outcomes.size < n &&
                        (keep_going || o.outcomes
                          .forall(_._1 == OutcomeStatus.pass))
                      }
                  }
                case _ => false
              }
        }
      }
    }
  }

  // Every student's results for `phase`, by alias, in the project's results
  // repo (phase.results_file), for the report. Publishing the latest results
  // also updates each student's own results repo.
  def publish_results(
      keep_going: Boolean,
      tests: TestSet
  ): (Phase, Int) => Target[
    WithData[SortedMap[Alias, RedactedStudentResults]]
  ] =
    fun { (phase, n) =>
      complex_gitolite_target(
        repo = project_results_repo_name,
        msg = s"update ${phase.results_file}",
        readers = Seq("@all")
      ) { tracker =>
        val students = tracker.track(students_with_submission).toSeq
        val deps = tracker.track_all(
          (
            // rewrite the file if it stops being readable
            readable = results_readable(phase),
            results = Many(
              students.map(student_results(keep_going, tests)(_, phase, n))
            ),
            own_repos = Many(
              if (phase == Phase.Latest)
                students.map(publish_student_results(keep_going, tests)(_, n))
              else Seq()
            ),
            // lateness is from each student's own deadline for the phase
            deadlines = Many(students.map(deadline(_, phase))),
            aliases = publish_aliases
          )
        )

        (_, repo) => {
          val deadline_of = students
            .zip(deps.deadlines)
            .map((csid, d) =>
              csid -> ZonedDateTime.of(d, ZoneId.systemDefault()).nn
            )
            .toMap

          val data = (for {
            r <- deps.results.flatten
            alias <- deps.aliases.value.get(r.csid).toSeq
          } yield (alias, r.redacted(deadline_of(r.csid)))).to(SortedMap)
          os.write.over(
            repo.dir / phase.results_file,
            upickle.default.write(data, indent = 2)
          )
          data
        }
      }
    }

  // A phase's published results, for the report page: Right(None) if there
  // are none yet, Left(why) if its file isn't readable by this version of the
  // grader. The page shows a notice for that project instead of failing the
  // whole course site, until publish_results rewrites the file.
  lazy val results: Phase => Target[
    Either[String, Option[SortedMap[Alias, RedactedStudentResults]]]
  ] = fun { phase =>
    target(
      (
        results_repo = Gitolite.mirror(project_results_repo_name)
      )
    ) { (producer, deps) =>
      val f = deps.results_repo.get_data_path(producer) / phase.results_file
      if (!deps.results_repo.value || !os.exists(f)) Right(None)
      else
        Try(
          read_published[SortedMap[Alias, RedactedStudentResults]](
            f,
            s"run publish_results -c ${course.course_name} -p $project_name"
          )
        ).toEither.left
          .map(e => String.valueOf(e.getMessage))
          .map(Some(_))
    }
  }

  ///////////////////
  // student tests //
  ///////////////////

  private lazy val student_test: CSID => Target[WithData[Option[String]]] =
    fun { csid =>
      target(
        (
          submission = submission(csid),
          test_extensions = test_extensions,
          // the project's cutoff: phase 1 extensions are for code, not tests
          test_cutoff = test_cutoff
        )
      ) { (producer, deps) =>
        val submission = deps.submission
        create_data(producer)(_.lastOpt.contains(".git")) { dir =>
          submission.flatMap { submission =>
            // clone, no checkout
            val _ = os
              .proc(
                "git",
                "clone",
                submission.get_data_path(producer),
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
                deps.test_cutoff.toString,
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

            val looking_for =
              deps.test_extensions.map(e => s"${csid.toString}.$e")
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
          }
        }
      }
    }

  lazy val student_tests_by_csid: Target[SortedMap[CSID, WithData[String]]] =
    complex_target { tracker =>
      val csids = tracker.track(students_with_submission).toSeq
      val possible_tests = tracker.track_all(csids.map(student_test))

      run_if_needed(tracker) { _ =>
        (for {
          (csid, test) <- csids.zip(possible_tests)
          git_sha <- test.value.toSeq
        } yield (csid, test.copy(value = git_sha))).to(SortedMap)
      }
    }

  private lazy val csids_with_tests: Target[SortedSet[CSID]] =
    target((student_tests = student_tests_by_csid)) { (_, deps) =>
      deps.student_tests.keySet
    }

  //////////////////
  // Aliases repo //
  //////////////////

  private lazy val aliases_repo_name: String =
    s"${course.course_name}_${project_name}__aliases"

  private lazy val publish_aliases: Target[WithData[SortedMap[CSID, Alias]]] =
    gitolite_target(
      repo = aliases_repo_name,
      msg = "more aliases",
      readers = Seq(course.staff_group_name)
    )((students_with_submission = students_with_submission)) {
      (_, deps, repo) =>
        val file = repo.dir / "aliases.json"
        val old_aliases = if (os.exists(file)) {
          upickle.default.read[SortedMap[CSID, Alias]](os.read(file))
        } else {
          SortedMap[CSID, Alias]()
        }

        var next_alias = old_aliases.values.foldLeft(0)(_ max _.value) + 1
        val covered_csids = old_aliases.keySet

        var new_aliases = old_aliases.toMap

        for {
          csid <- deps.students_with_submission
          if !covered_csids.contains(csid)
        } {
          new_aliases = new_aliases.updated(csid, Alias(next_alias))
          next_alias += 1
        }

        val out = new_aliases.to(SortedMap)
        os.write.over(file, upickle.default.write(out, indent = 2))
        out
    }

  lazy val get_aliases: Target[SortedMap[CSID, Alias]] =
    target((aliases = Gitolite.mirror(aliases_repo_name))) { (producer, deps) =>
      val path = deps.aliases.get_data_path(producer) / "aliases.json"
      if (os.isFile(path)) {
        upickle.default.read[SortedMap[CSID, Alias]](os.read(path))
      } else {
        SortedMap[CSID, Alias]()
      }
    }

  lazy val anti_aliases: Target[SortedMap[Alias, CSID]] =
    target((aliases = publish_aliases)) { (_, deps) =>
      val pairs = for {
        (csid, alias) <- deps.aliases.value.toSeq
      } yield (alias, csid)
      pairs.to(SortedMap)
    }

  private lazy val csid_to_alias: CSID => Target[Option[Alias]] = fun { csid =>
    target((aliases = publish_aliases)) { (_, deps) =>
      deps.aliases.value.get(csid)
    }
  }

  ///////////
  // Tests //
  ///////////

  private lazy val override_test_names: Target[SortedSet[String]] =
    target(
      (
        override_repo = publish_override_repo,
        test_extensions = test_extensions
      )
    ) { (producer, deps) =>
      val possible_tests = (for {
        f <- os.list(deps.override_repo.get_data_path(producer))
        if deps.test_extensions.contains(f.ext)
        if !f.last.startsWith(".")
      } yield f).groupBy(f => f.baseName)

      val complete_tests =
        possible_tests.filter(_._2.size == deps.test_extensions.size)

      complete_tests.keySet.to(SortedSet)
    }

  private lazy val override_test: String => Target[WithData[String]] = fun {
    test_name =>
      target(
        (
          override_repo = publish_override_repo,
          test_extensions = test_extensions
        )
      ) { (producer, deps) =>
        create_data(producer)(_ => false) { dir =>
          for (ext <- deps.test_extensions) {
            val f = s"$test_name.$ext"
            os.copy(
              from = deps.override_repo.get_data_path(producer) / f,
              to = dir / f,
              createFolders = true,
              followLinks = false
            )
          }
          ""
        }
      }
  }

  private lazy val override_tests: Target[SortedMap[String, WithData[String]]] =
    complex_target { tracker =>
      val test_names = tracker.track(override_test_names).toSeq
      val tests = tracker.track_all(test_names.map(override_test))

      run_if_needed(tracker) { _ =>
        test_names.zip(tests).to(SortedMap)
      }
    }

  lazy val publish_tests: Target[WithData[SortedMap[TestId, TestInfo]]] =
    gitolite_target(
      repo = tests_repo_name,
      msg = "tests",
      readers = Seq("@all")
    )(
      (
        student_tests_by_csid = student_tests_by_csid,
        aliases = publish_aliases,
        publish_submitted_tests = publish_submitted_tests,
        override_tests = override_tests,
        test_extensions = test_extensions
      )
    ) { (producer, deps, repo) =>
      // delete all tests
      for {
        f <- os.list(repo.dir)
        if !f.last.startsWith(".")
      } {
        os.remove.all(f)
      }

      val info = mutable.Map[TestId, TestInfo]()

      // copy student tests, including ignored ones (projects before
      // their first deadline run those too; see TestSet)
      for {
        (csid, test_sp) <- deps.student_tests_by_csid.toSeq
        alias <- deps.aliases.value.get(csid).toSeq
      } {
        for (ext <- deps.test_extensions) {
          val test_id = TestId(
            external_name = alias.toString,
            internal_name = csid.toString
          )
          val test_info = TestInfo(test_id, test_sp.copy(value = ()))
          info.update(test_id, test_info)
          val src =
            test_sp.get_data_path(producer) / s"${csid.value}.$ext"
          val dest = repo.dir / s"${alias.toString}.$ext"
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
        (test_name, test_sp) <- deps.override_tests.toSeq
      } {
        for (ext <- deps.test_extensions) {
          val test_id = TestId(
            external_name = test_name,
            internal_name = test_name
          )
          val test_info = TestInfo(test_id, test_sp.copy(value = ()))
          info.update(test_id, test_info)
          os.copy.over(
            from = test_sp.get_data_path(producer) / s"$test_name.$ext",
            to = repo.dir / s"$test_name.$ext",
            createFolders = true,
            followLinks = false
          )
        }
      }

      info.to(SortedMap)
    }

  lazy val test_ids: Target[SortedSet[TestId]] =
    target((tests = publish_tests)) { (_, deps) =>
      deps.tests.value.keySet
    }

  lazy val phase1_test_ids: Target[SortedSet[TestId]] =
    target((test_ids = test_ids, phase1_tests = phase1_tests)) {
      case (_, (tests, chosen)) =>
        for {
          test <- tests
          if chosen.contains(test.external_name) || chosen.contains(
            test.internal_name
          )
        } yield test
    }

  lazy val phase2_test_ids: Target[SortedSet[TestId]] =
    target((test_ids = test_ids, phase2_tests = phase2_tests)) {
      case (_, (tests, chosen)) =>
        for {
          test <- tests
          if chosen.contains(test.external_name) || chosen.contains(
            test.internal_name
          )
        } yield test
    }

  // all but the tests marked ignored, by external or internal name
  lazy val good_test_ids: Target[SortedSet[TestId]] =
    target((tests = test_ids, bad = bad_tests)) { (_, deps) =>
      deps.tests.filterNot(t =>
        deps.bad.contains(t.external_name) || deps.bad.contains(t.internal_name)
      )
    }

  // the tests the correctness score counts: (phase 1 + phase 2) - ignored
  lazy val chosen_test_ids: Target[SortedSet[TestId]] =
    target(
      (phase1 = phase1_test_ids, phase2 = phase2_test_ids, good = good_test_ids)
    ) { (_, deps) =>
      (deps.phase1 ++ deps.phase2).intersect(deps.good)
    }

  // each phase's chosen tests, less the ignored ones
  private lazy val good_phase1_test_ids: Target[SortedSet[TestId]] =
    target((a = phase1_test_ids, b = good_test_ids))((_, deps) =>
      deps.a.intersect(deps.b)
    )
  private lazy val good_phase2_test_ids: Target[SortedSet[TestId]] =
    target((a = phase2_test_ids, b = good_test_ids))((_, deps) =>
      deps.a.intersect(deps.b)
    )

  def test_ids_for(tests: TestSet): Target[SortedSet[TestId]] = tests match {
    case "all"    => test_ids
    case "good"   => good_test_ids
    case "chosen" => chosen_test_ids
    case "phase1" => good_phase1_test_ids
    case "phase2" => good_phase2_test_ids
  }

  lazy val test_info: TestId => Target[WithData[TestInfo]] = fun { test_id =>
    target(
      (
        tests = publish_tests,
        test_extensions = test_extensions
      )
    ) { (producer, deps) =>
      create_data(producer)(_ => false) { dir =>
        deps.tests.value.get(test_id) match {
          case Some(info) =>
            copy_test(
              info.id,
              deps.tests.get_data_path(producer),
              dir,
              deps.test_extensions
            )
            info
          case None =>
            throw Exception(s"no info for ${test_id.toString}")
        }
      }
    }
  }

  private lazy val csid_has_test: CSID => Target[Boolean] = fun { csid =>
    target((ts = csids_with_tests))((_, deps) => deps.ts.contains(csid))
  }

  val tests_repo_name = s"${course.course_name}_${project_name}__tests"

  ///////////////////////////
  // Test Case Submissions //
  //////////////////////////

  private lazy val publish_submitted_tests
      : Target[WithData[SortedMap[TestId, TestInfo]]] =
    gitolite_target(
      repo = submitted_tests_repo_name,
      msg = "tests",
      readers = Seq(course.staff_group_name)
    )(
      (
        student_tests_by_csid = student_tests_by_csid,
        aliases = publish_aliases,
        test_extensions = test_extensions
      )
    ) { (producer, deps, repo) =>
      // delete all tests
      for {
        f <- os.list(repo.dir)
        if !f.last.startsWith(".")
      } {
        os.remove.all(f)
      }

      val info = mutable.Map[TestId, TestInfo]()

      // copy student tests
      for {
        (csid, test_sp) <- deps.student_tests_by_csid.toSeq
        alias <- deps.aliases.value.get(csid).toSeq
      } {
        for (ext <- deps.test_extensions) {
          val test_id = TestId(
            external_name = alias.toString,
            internal_name = csid.toString
          )
          val test_info = TestInfo(test_id, test_sp.copy(value = ()))
          info.update(test_id, test_info)
          val src =
            test_sp.get_data_path(producer) / s"${csid.value}.$ext"
          val dest = repo.dir / s"${alias.toString}.${ext.toString}"
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

  private lazy val submitted_tests_repo_name =
    s"${course.course_name}_${project_name}__submitted_tests"

  ////////////
  // Scores //
  ////////////

  private lazy val compute_student_scores
      : (CSID, Phase, Int) => Target[SortedSet[TestId]] = fun {
    (csid, phase, n) =>
      complex_target { tracker =>
        val runs = for {
          info <- tracker.track(prepare_info(csid, phase)).toSeq
          id <- tracker.track(phase2_test_ids).toSeq
        } yield run_one(true, n)(csid, info.sha, id)
        val outs_v = tracker.track_all(runs)
        run_if_needed(tracker) { _ =>
          (for {
            sp: WithData[Outcome] <- outs_v
            d: Outcome = sp.value
            if d.outcomes.map(_._1).contains(OutcomeStatus.pass)
          } yield d.test_id).to(SortedSet)
        }
      }
  }

  lazy val compute_scores: (Phase, Int) => Target[
    SortedMap[CSID, SortedSet[TestId]]
  ] = fun { (phase, n) =>
    complex_target { tracker =>
      val csids = tracker.track(students_with_submission)
      val scores = tracker.track_all(
        csids.toSeq.map(compute_student_scores(_, phase, n))
      )

      run_if_needed(tracker) { _ =>
        csids.toSeq.zip(scores).to(SortedMap)
      }
    }
  }
}

object Project extends Scope(".") {
  given Ordering[Project] = Ordering.by(p => (p.course, p.project_name))

  // The output of time's %E format, output by the Makefile build system for test runtimes
  val Seconds: Regex = """(\d+\.\d+)""".r
  val Minutes: Regex = """(\d+):(\d+.\d+)""".r
  val Hours: Regex = """(\d+):(\d+):(\d+.\d+)""".r

  def parseTime(s: Option[String]): Option[Double] = s match {
    case Some(Seconds(s))     => Some(s.nn.toDouble)
    case Some(Minutes(m, s))  => Some((m.nn.toDouble * 60) + s.nn.toDouble)
    case Some(Hours(h, m, s)) =>
      Some((h.nn.toDouble * 3600) + (m.nn.toDouble * 60) + s.nn.toDouble)
    case Some("timeout") => None
    case Some(s)         => throw new Exception(s"invalid time format: $s")
    case None            => None
  }

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

  private lazy val all_projects: Target[SortedSet[Project]] = complex_target {
    tracker =>
      val cs = tracker.track(Course.all)
      val pms = tracker.track_all(cs.map(_.projects))
      run_if_needed(tracker) { _ =>
        pms.flatMap(pm => pm.values).to(SortedSet)
      }
  }

  lazy val active_projects: Target[SortedSet[Project]] =
    complex_target { tracker =>
      val all_projects_v = tracker.track(all_projects).toSeq
      val active_v = tracker.track_all(all_projects_v.map(_.active))

      run_if_needed(tracker) { _ =>
        (for {
          (p, a) <- all_projects_v.zip(active_v)
          if a
        } yield p).to(SortedSet)
      }
    }

  private val automatic_override_names: Set[String] =
    Set("Makefile", "makefile")

  lazy val uid: String = os.proc("id", "-u").call().out.lines().head
  lazy val gid: String = os.proc("id", "-g").call().out.lines().head
}
