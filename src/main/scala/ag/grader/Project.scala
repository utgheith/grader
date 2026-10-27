package ag.grader

import ag.common.{
  NoNulls,
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.r2.{
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
  Tracker,
  update_data
}
import ag.rules.{lines, run}
import ag.task.Task
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
    extends Scope(course / project_name) derives ReadWriter {

  lazy val info: Target[RawProject] =
    Gitolite.raw_project(course.course_name, project_name)

  lazy val docker_file: Target[Option[String]] = target(
    info
  ) { info =>
    info.docker_file
  }

  lazy val docker_image: Target[Option[String]] = target(docker_file) {
    docker_file => docker_file.map(fn => Docker.of(os.pwd / fn))
  }

  lazy val active: Target[Boolean] =
    target(course.active, info, Gitolite.repo_info(project_repo_name)) {
      case (course_is_active, info, RepoInfo(_, _, Some(_))) =>
        course_is_active && info.active
      case (_, _, RepoInfo(_, _, None)) =>
        false
    }

  lazy val phase1_tests: Target[SortedSet[String]] =
    target(info)(p => p.phase1_tests.to(SortedSet))

  lazy val phase1_weight: Target[Int] =
    target(info)(p => p.phase1_weight)

  lazy val phase2_tests: Target[SortedSet[String]] =
    target(info)(p => p.phase2_tests.to(SortedSet))

  lazy val phase2_weight: Target[Int] =
    target(info)(p => p.phase2_weight)

  lazy val test_weights: Target[Seq[Weight]] =
    target(info)(p => p.test_weights)

  lazy val bad_tests: Target[SortedSet[String]] =
    target(info)(p => SortedSet(p.bad_tests*))

  private lazy val cores: Target[Int] =
    target(info)(p => p.cores)

  lazy val test_cutoff: Target[LocalDateTime] =
    target(info)(p => p.test_cutoff)
  lazy val code_cutoff: Target[LocalDateTime] =
    target(info)(p => p.code_cutoff)
  lazy val extensions: Target[Extensions] =
    target(info)(p => p.extensions)

  // `csid`'s deadline for `phase`, with any extension they have: phase 1's is
  // the test cutoff, phase 2's the code cutoff; the latest results are late
  // from the code cutoff too.
  lazy val deadline: (CSID, Phase) => Target[LocalDateTime] = fun {
    (csid, phase) =>
      target(test_cutoff, code_cutoff, extensions) {
        (first, second, extensions) =>
          phase
            .deadline(first, second)
            .getOrElse(second)
            .plusDays(extensions.days(csid, phase))
            .nn
      }
  }
  lazy val test_extensions: Target[SortedSet[String]] =
    target(info)(p => SortedSet(p.test_extensions*))

  private lazy val project_repo_name: String =
    s"${course.course_name}_$project_name"

  private lazy val project_repo: Target[WithData[Boolean]] =
    Gitolite.mirror(project_repo_name)

  lazy val staff: Target[SortedSet[CSID]] =
    target(info) { p =>
      SortedSet(p.staff*)
    }

  ///////////////////
  // Override Repo //
  ///////////////////

  private lazy val override_repo_name = s"${project_repo_name}__override"

  lazy val publish_override_repo: Target[WithData[Unit]] =
    target(
      Gitolite.repo_info(
        override_repo_name
      ),
      test_extensions,
      project_repo,
      Config.can_push_repo
    ) { (override_repo_info, test_extensions, project_repo, can_push_repo) =>
      update_data(_.lastOpt.contains(".git")) { dir =>
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
              f <- os.list(project_repo.get_data_path)
              if !f.last.startsWith(".")
              if test_extensions.contains(f.ext)
            } {
              val target = dir / f.relativeTo(project_repo.get_data_path)
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
              .stream(path = project_repo.get_data_path, followLinks = false)
              .foreach { f =>
                if (
                  os.isFile(f) && Project.automatic_override_names.contains(
                    f.last
                  )
                ) {
                  val target = dir / f.relativeTo(project_repo.get_data_path)
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
    }

  //////////////////////////
  // Work repo (per csid) //
  //////////////////////////

  def work_repo_name(csid: CSID): String = s"${project_repo_name}_${csid.value}"

  def work_repo(csid: CSID): Target[WithData[Boolean]] =
    Gitolite.mirror(work_repo_name(csid))

  lazy val publish_work_repo: CSID => Target[WithData[Unit]] = fun {
    (csid: CSID) =>
      target(
        Gitolite.repo_info(
          work_repo_name(csid)
        ),
        course.notifications,
        Config.can_send_mail,
        Config.can_push_repo
      ) { (repo_info, n, can_send_mail, can_push_repo) =>
        update_data(_.lastOpt.contains(".git")) { dir =>
          repo_info.update(
            path = dir,
            fork_from = Some(project_repo_name),
            readers = Seq(csid.value, course.staff_group_name),
            writers = Seq(csid.value),
            msg = s"create",
            can_push_repo
          ) { forked =>
            if (forked) {
              shout(
                s"sending notification to ${csid.toString}, can_send_mail is $can_send_mail"
              )
              n.send_repo_created(this, repo_info, csid, can_send_mail, dir)
            }
            ()
          }
        }
      }
  }

  private lazy val submission: CSID => Target[Option[WithData[Unit]]] = fun {
    (csid: CSID) =>
      target(work_repo(csid), project_repo) { (work_repo, project_repo) =>
        if (
          (!work_repo.value) || (work_repo.data_signature == project_repo.data_signature)
        ) {
          None
        } else {
          Some(work_repo.copy(value = ()))
        }
      }
  }

  lazy val submissions: Target[SortedMap[CSID, WithData[Unit]]] =
    complex_target {
      val csids: Seq[CSID] = course.enrolled_csids.guilty.toSeq
      val submissions: Seq[Option[WithData[Unit]]] = for {
        csid <- csids
      } yield {
        submission(csid).guilty
      }

      run_if_needed {
        (for {
          (csid, submission) <- csids.zip(submissions)
          if submission.isDefined
        } yield (csid, submission.get)).to(SortedMap)
      }

    }

  lazy val students_with_submission: Target[SortedSet[CSID]] =
    target(submissions)(_.keySet)

  private val report_name: String = "REPORT.txt"

  private val initial_report: Target[WithData[Unit]] = target(
    project_repo
  ) { project_repo =>
    create_data(skip = _ => false) { dir =>
      val src = project_repo.get_data_path / report_name
      val dest = dir / report_name
      if (project_repo.value && os.isFile(src)) {
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
        submission(csid)
      ) { submission =>
        create_data(_ => false) { dir =>
          val dest = dir / report_name
          os.remove.all(dest)
          submission.foreach { submission =>
            val src = submission.get_data_path / report_name
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
    target(initial_report, student_report(csid)) {
      (initial_report, student_report) =>
        initial_report.data_signature != student_report.data_signature
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
      target(submission(csid), deadline(csid, phase)) {
        (submission_repo, deadline) =>
          submission_repo.flatMap { submission_repo =>
            val repo = submission_repo.get_data_path
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
                if (sha.isEmpty) say(s"$name names no commit ('$id'): ignored")
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
                      .of(deadline, ZoneId.systemDefault)
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
      target(submission(csid), publish_override_repo, test_extensions) {
        (submission_repo, override_repo, test_extensions) =>
          create_data(_ => false) { dir =>
            val repo = submission_repo.getOrElse(
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
                repo.get_data_path,
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
              override_repo.get_data_path,
              dir,
              followLinks = false,
              replaceExisting = true,
              createFolders = true,
              mergeFolders = true
            )

            // (3) remove all tests
            for {
              p <- os.list(dir)
              if test_extensions.contains(p.ext)
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
      target(graded_commit(csid, phase), has_student_report(csid)) {
        (graded, has_report) =>
          graded.map(g =>
            PrepareInfo(
              commit_time = g.commit_time,
              sha = g.sha,
              has_report = has_report,
              push_time = g.push_time,
              commit_source = g.source
            )
          )
      }
  }

  lazy val is_late: (CSID, Phase) => Target[Boolean] = fun { (csid, phase) =>
    target(prepare_info(csid, phase), deadline(csid, phase)) {
      (info, deadline) =>
        info.forall(
          _.commit_time.isAfter(
            ZonedDateTime.of(deadline, ZoneId.systemDefault())
          )
        )
    }
  }

  // The commits on master after the student's (phase 2) deadline.
  lazy val late_commits: CSID => Target[WithData[Seq[LateCommit]]] = fun {
    csid =>
      target(submission(csid), deadline(csid, Phase.Two)) {
        (submission_repo, code_cutoff) =>
          create_data(_.lastOpt.contains(".git")) { dir =>
            submission_repo.toSeq.flatMap { submission_repo =>
              val _ = os
                .proc(
                  "git",
                  "clone",
                  "--shared",
                  "--template=",
                  "--no-checkout",
                  submission_repo.get_data_path,
                  dir
                )
                .run()

              val default_branch = "master"

              val cutoff_time =
                ZonedDateTime.of(code_cutoff, ZoneId.systemDefault).nn
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
      complex_target {
        val test_info_f = test_info(test_id).track
        val test_extensions_f = test_extensions.track
        val prepared_f = prepare(csid, sha).track
        val cores_f = cores.track
        val docker_image_f = docker_image.track

        summon[Tracker[WithData[Outcome]]].run { old_state =>
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
              val test_info = test_info_f.block
              val test_extensions = test_extensions_f.block
              val prepared = prepared_f.block
              val cores = cores_f.block
              val docker_image = docker_image_f.block
              Task.successful {
                update_data(_ => false) { dir =>
                  // only the files from the latest run are kept
                  os.remove.all(dir)
                  os.makeDir.all(dir)

                  val res = doit(
                    project = this,
                    csid = csid,
                    cores = cores,
                    prepared_path = prepared.get_data_path,
                    test_id = test_info.value.id,
                    test_path = test_info.get_data_path,
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
      complex_target {
        val info = prepare_info(csid, phase).guilty
        val outcomes = for {
          info <- info.toSeq
          id <- test_ids_for(tests).guilty.toSeq
        } yield run_one(keep_going, n)(csid, info.sha, id).track
        val alias = csid_to_alias(csid).track
        val has_test = csid_has_test(csid).track
        run_if_needed {
          info.map(info =>
            StudentResults(
              csid = csid,
              alias = alias.block,
              has_test = has_test.block,
              prepare_info = info,
              // keyed by alias: a student test's internal name is its
              // author's CSID
              outcomes = Task
                .sequence(outcomes)
                .block
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
      complex_target {
        val results =
          student_results(keep_going, tests)(csid, Phase.Latest, n).guilty
        // the same runs, for their output files
        val outcomes = for {
          r <- results.toSeq
          id <- test_ids_for(tests).guilty.toSeq
        } yield run_one(keep_going, n)(csid, r.prepare_info.sha, id).track
        val repo_info =
          Gitolite.repo_info(student_results_repo_name(csid)).track
        // rewrite the_results.json if it stops being readable
        val _ = student_results_readable(csid).track
        val can_push_repo = Config.can_push_repo.track

        run_if_needed {
          val student_results_repo = repo_info.block
          val outcomes_v = Task.sequence(outcomes).block
          update_data(_.lastOpt.contains(".git")) { dir =>
            student_results_repo.update(
              path = dir,
              fork_from = Some("empty"),
              msg = "updated results",
              readers = Seq(csid.value, course.staff_group_name),
              writers = Seq(),
              can_push_repo.block
            ) { _ =>
              for {
                f <- os.list(dir)
                if !f.last.startsWith(".")
              } os.remove.all(f)
              for (outcome <- outcomes_v)
                copy_results(outcome.get_data_path, dir, outcome.value.test_id)
              for (r <- results)
                os.write.over(
                  dir / "the_results.json",
                  upickle.default.write(r)
                )
              results
            }
          }
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
      target(Gitolite.mirror(student_results_repo_name(csid))) { repo =>
        repo.value && readable[StudentResults](
          repo.get_data_path / "the_results.json"
        )
      }
  }

  private lazy val results_readable: Phase => Target[Boolean] = fun { phase =>
    target(Gitolite.mirror(project_results_repo_name)) { repo =>
      repo.value && readable[SortedMap[Alias, RedactedStudentResults]](
        repo.get_data_path / phase.results_file
      )
    }
  }

  lazy val get_student_results: CSID => Target[Option[StudentResults]] =
    fun { csid =>
      target(Gitolite.mirror(student_results_repo_name(csid))) { sp =>
        if (sp.value) {
          val f = sp.get_data_path / "the_results.json"

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
      get_student_results(csid)
    ) {
      _.map { rdr =>
        StudentFailures(rdr)
      }
    }
  }

  private lazy val has_spamon: CSID => Target[Boolean] = fun { csid =>
    target(work_repo(csid)) { work_repo =>
      os.exists(work_repo.get_data_path / "spamon")
    }
  }

  // Where the project is relative to its deadlines. Recomputed when a
  // deadline changes, and checked again every minute as time passes; what
  // depends on it reruns only when the phase actually changes.
  lazy val state: Target[ProjectState] =
    target(test_cutoff, code_cutoff, periodic(60 * 1000)) {
      (first, second, _) =>
        ProjectState.at(LocalDateTime.now().nn, first, second)
    }

  // Whether `csid`'s deadline for `phase` has passed. Checked again every
  // minute as time passes; what depends on it reruns only when it flips.
  lazy val past_deadline: (CSID, Phase) => Target[Boolean] = fun {
    (csid, phase) =>
      target(deadline(csid, phase), periodic(60 * 1000)) { (deadline, _) =>
        !LocalDateTime.now().nn.isBefore(deadline)
      }
  }

  // No result emails after the student's second deadline.
  lazy val notify_student_results: CSID => Target[Unit] = fun { csid =>
    target(
      get_student_failures(
        csid
      ),
      course.notifications,
      Config.gitolite,
      has_spamon(
        csid
      ),
      Config.can_send_mail,
      past_deadline(csid, Phase.Two)
    ) {
      case (_, _, _, _, _, true) =>
        say(
          s"no result emails after the second deadline: ${course.course_name}:$project_name:${csid.toString}"
        )
      case (Some(sr), n, g, has_spamon, can_send_mail, _) =>
        if (has_spamon) {
          n.send_result_update(this, csid, g, sr, can_send_mail)
        } else {
          say(
            s"no spamon for ${course.course_name}:$project_name:${csid.toString}"
          )
        }
      case (None, _, _, _, _, _) =>
    }
  }

  lazy val project_results_repo_name: String =
    s"${course.course_name}_${project_name}__results"

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
      complex_target {
        val repo_info = Gitolite.repo_info(project_results_repo_name).track
        // rewrite the file if it stops being readable
        val _ = results_readable(phase).track
        val students = students_with_submission.guilty.toSeq
        val results = students.map(csid =>
          student_results(keep_going, tests)(csid, phase, n).track
        )
        val _ =
          if (phase == Phase.Latest)
            students.map(csid =>
              publish_student_results(keep_going, tests)(csid, n).track
            )
          else Nil
        // lateness is from each student's own deadline for the phase
        val deadlines = students.map(csid => deadline(csid, phase).track)
        val aliases = publish_aliases.track
        val can_push_repo = Config.can_push_repo.track

        run_if_needed {
          val repo_info_v = repo_info.block
          val deadline_of = students
            .zip(Task.sequence(deadlines).block)
            .map((csid, d) =>
              csid -> ZonedDateTime.of(d, ZoneId.systemDefault()).nn
            )
            .toMap
          update_data(_.lastOpt.contains(".git")) { dir =>
            repo_info_v.update(
              path = dir,
              fork_from = Some("empty"),
              msg = s"update ${phase.results_file}",
              readers = Seq("@all"),
              writers = Seq(),
              can_push_repo.block
            ) { _ =>
              val data = (for {
                r <- Task.sequence(results).block.flatten
                alias <- aliases.block.value.get(r.csid).toSeq
              } yield (alias, r.redacted(deadline_of(r.csid)))).to(SortedMap)
              os.write.over(
                dir / phase.results_file,
                upickle.default.write(data, indent = 2)
              )
              data
            }
          }
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
      Gitolite.mirror(project_results_repo_name)
    ) { results_repo =>
      val f = results_repo.get_data_path / phase.results_file
      if (!results_repo.value || !os.exists(f)) Right(None)
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
        submission(csid),
        test_extensions,
        deadline(csid, Phase.One)
      ) { (submission, test_extensions, test_cutoff) =>
        create_data(_.lastOpt.contains(".git")) { dir =>
          submission.flatMap { submission =>
            // clone, no checkout
            val _ = os
              .proc(
                "git",
                "clone",
                submission.get_data_path,
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

            val looking_for = test_extensions.map(e => s"${csid.toString}.$e")
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
    complex_target {

      val csids = students_with_submission.guilty.toSeq
      val possible_test_tasks = csids.map(csid => student_test(csid).track)

      run_if_needed {
        val possible_tests = Task.sequence(possible_test_tasks).block
        (for {
          (csid, test) <- csids.zip(possible_tests)
          git_sha <- test.value.toSeq
        } yield (csid, test.copy(value = git_sha))).to(SortedMap)
      }
    }

  private lazy val csids_with_tests: Target[SortedSet[CSID]] =
    target(student_tests_by_csid) { student_tests =>
      student_tests.keySet
    }

  //////////////////
  // Aliases repo //
  //////////////////

  private lazy val aliases_repo_name: String =
    s"${course.course_name}_${project_name}__aliases"

  private lazy val publish_aliases: Target[WithData[SortedMap[CSID, Alias]]] =
    target(
      Gitolite.repo_info(
        aliases_repo_name
      ),
      students_with_submission,
      Config.can_push_repo
    ) { (repo_info, students_with_submission, can_push_repo) =>
      update_data(skip = _.lastOpt.contains(".git")) { dir =>
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
    }

  lazy val get_aliases: Target[SortedMap[CSID, Alias]] =
    target(Gitolite.mirror(aliases_repo_name)) { aliases =>
      val path = aliases.get_data_path / "aliases.json"
      if (os.isFile(path)) {
        upickle.default.read[SortedMap[CSID, Alias]](os.read(path))
      } else {
        SortedMap[CSID, Alias]()
      }
    }

  lazy val anti_aliases: Target[SortedMap[Alias, CSID]] =
    target(publish_aliases) { aliases =>
      val pairs = for {
        (csid, alias) <- aliases.value.toSeq
      } yield (alias, csid)
      pairs.to(SortedMap)
    }

  private lazy val csid_to_alias: CSID => Target[Option[Alias]] = fun { csid =>
    target(publish_aliases) { aliases =>
      aliases.value.get(csid)
    }
  }

  ///////////
  // Tests //
  ///////////

  private lazy val override_test_names: Target[SortedSet[String]] =
    target(
      publish_override_repo,
      test_extensions
    ) { (override_repo, test_extensions) =>
      val possible_tests = (for {
        f <- os.list(override_repo.get_data_path)
        if test_extensions.contains(f.ext)
        if !f.last.startsWith(".")
      } yield f).groupBy(f => f.baseName)

      val complete_tests =
        possible_tests.filter(_._2.size == test_extensions.size)

      complete_tests.keySet.to(SortedSet)
    }

  private lazy val override_test: String => Target[WithData[String]] = fun {
    test_name =>
      target(
        publish_override_repo,
        test_extensions
      ) { (override_repo, test_extensions) =>
        create_data(_ => false) { dir =>
          for (ext <- test_extensions) {
            val f = s"$test_name.$ext"
            os.copy(
              from = override_repo.get_data_path / f,
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
    complex_target {
      val test_names = override_test_names.guilty.toSeq
      val test_tasks =
        test_names.map(test_name => override_test(test_name).track)

      run_if_needed {
        val tests = Task.sequence(test_tasks).block
        test_names.zip(tests).to(SortedMap)
      }
    }

  lazy val publish_tests: Target[WithData[SortedMap[TestId, TestInfo]]] =
    target(
      Gitolite.repo_info(tests_repo_name),
      student_tests_by_csid,
      publish_aliases,
      publish_submitted_tests,
      override_tests,
      test_extensions,
      Config.can_push_repo
    ) {
      (
          tests_repo_info,
          student_tests_by_csid,
          aliases,
          _,
          override_tests,
          test_extensions,
          can_push_repo
      ) =>
        update_data(_.lastOpt.contains(".git")) { dir =>
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

            // copy student tests, including ignored ones (projects before
            // their first deadline run those too; see TestSet)
            for {
              (csid, test_sp) <- student_tests_by_csid.toSeq
              alias <- aliases.value.get(csid).toSeq
            } {
              for (ext <- test_extensions) {
                val test_id = TestId(
                  external_name = alias.toString,
                  internal_name = csid.toString
                )
                val test_info = TestInfo(test_id, test_sp.copy(value = ()))
                info.update(test_id, test_info)
                val src = test_sp.get_data_path / s"${csid.value}.$ext"
                val dest = dir / s"${alias.toString}.$ext"
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
                val test_info = TestInfo(test_id, test_sp.copy(value = ()))
                info.update(test_id, test_info)
                os.copy.over(
                  from = test_sp.get_data_path / s"$test_name.$ext",
                  to = dir / s"$test_name.$ext",
                  createFolders = true,
                  followLinks = false
                )
              }
            }

            info.to(SortedMap)

          }

        }
    }

  lazy val test_ids: Target[SortedSet[TestId]] =
    target(publish_tests) { tests =>
      tests.value.keySet
    }

  lazy val phase1_test_ids: Target[SortedSet[TestId]] =
    target(test_ids, phase1_tests) { case (tests, chosen) =>
      for {
        test <- tests
        if chosen.contains(test.external_name) || chosen.contains(
          test.internal_name
        )
      } yield test
    }

  lazy val phase2_test_ids: Target[SortedSet[TestId]] =
    target(test_ids, phase2_tests) { case (tests, chosen) =>
      for {
        test <- tests
        if chosen.contains(test.external_name) || chosen.contains(
          test.internal_name
        )
      } yield test
    }

  // all but the tests marked ignored, by external or internal name
  lazy val good_test_ids: Target[SortedSet[TestId]] =
    target(test_ids, bad_tests) { (tests, bad) =>
      tests.filterNot(t =>
        bad.contains(t.external_name) || bad.contains(t.internal_name)
      )
    }

  // the tests the correctness score counts: (phase 1 + phase 2) - ignored
  lazy val chosen_test_ids: Target[SortedSet[TestId]] =
    target(phase1_test_ids, phase2_test_ids, good_test_ids) {
      (phase1, phase2, good) => (phase1 ++ phase2).intersect(good)
    }

  // each phase's chosen tests, less the ignored ones
  private lazy val good_phase1_test_ids: Target[SortedSet[TestId]] =
    target(phase1_test_ids, good_test_ids)(_.intersect(_))
  private lazy val good_phase2_test_ids: Target[SortedSet[TestId]] =
    target(phase2_test_ids, good_test_ids)(_.intersect(_))

  def test_ids_for(tests: TestSet): Target[SortedSet[TestId]] = tests match {
    case "all"    => test_ids
    case "good"   => good_test_ids
    case "chosen" => chosen_test_ids
    case "phase1" => good_phase1_test_ids
    case "phase2" => good_phase2_test_ids
  }

  lazy val test_info: TestId => Target[WithData[TestInfo]] = fun { test_id =>
    target(
      publish_tests,
      test_extensions
    ) { (tests, test_extensions) =>
      create_data(_ => false) { dir =>
        tests.value.get(test_id) match {
          case Some(info) =>
            copy_test(info.id, tests.get_data_path, dir, test_extensions)
            info
          case None =>
            throw Exception(s"no info for ${test_id.toString}")
        }
      }
    }
  }

  private lazy val csid_has_test: CSID => Target[Boolean] = fun { csid =>
    target(csids_with_tests)(_.contains(csid))
  }

  val tests_repo_name = s"${course.course_name}_${project_name}__tests"

  ///////////////////////////
  // Test Case Submissions //
  //////////////////////////

  private lazy val publish_submitted_tests
      : Target[WithData[SortedMap[TestId, TestInfo]]] =
    target(
      Gitolite.repo_info(
        submitted_tests_repo_name
      ),
      student_tests_by_csid,
      publish_aliases,
      test_extensions,
      Config.can_push_repo
    ) {
      (
          submitted_tests_repo_info,
          student_tests_by_csid,
          aliases,
          test_extensions,
          can_push_repo
      ) =>
        update_data(_.lastOpt.contains(".git")) { dir =>
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
              alias <- aliases.value.get(csid).toSeq
            } {
              for (ext <- test_extensions) {
                val test_id = TestId(
                  external_name = alias.toString,
                  internal_name = csid.toString
                )
                val test_info = TestInfo(test_id, test_sp.copy(value = ()))
                info.update(test_id, test_info)
                val src = test_sp.get_data_path / s"${csid.value}.$ext"
                val dest = dir / s"${alias.toString}.${ext.toString}"
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
    }

  private lazy val submitted_tests_repo_name =
    s"${course.course_name}_${project_name}__submitted_tests"

  ////////////
  // Scores //
  ////////////

  private lazy val compute_student_scores
      : (CSID, Phase, Int) => Target[SortedSet[TestId]] = fun {
    (csid, phase, n) =>
      complex_target {
        val outs = for {
          info <- prepare_info(csid, phase).guilty.toSeq
          id <- phase2_test_ids.guilty.toSeq
        } yield run_one(true, n)(csid, info.sha, id).track
        run_if_needed {
          val outs_v = Task.sequence(outs).block
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
    complex_target {
      val csids = students_with_submission.guilty
      val pairs = csids.toSeq.map { csid =>
        compute_student_scores(csid, phase, n).track
          .map(g => (csid, g))
      }

      run_if_needed {
        Task.sequence(pairs).block.to(SortedMap)
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
    val cs = Course.all.guilty
    val pms = cs.map(_.projects.track)
    run_if_needed {
      Task.sequence(pms).block.flatMap(pm => pm.values).to(SortedSet)
    }
  }

  lazy val active_projects: Target[SortedSet[Project]] =
    complex_target {
      val all_projects_v = all_projects.guilty.toSeq
      val active = all_projects_v.map(_.active.track)

      run_if_needed {
        val active_v = Task.sequence(active).block
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
