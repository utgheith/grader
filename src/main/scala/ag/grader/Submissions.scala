package ag.grader

import ag.common.{
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet,
  NoNulls
}
import ag.r3.{Commit, CommitInfo, Context, Scope, Target, WithData, create_data}

import java.time.{
  Duration,
  Instant,
  OffsetDateTime,
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
import scala.collection.{SortedMap, SortedSet}
import scala.util.Try
import scala.util.matching.Regex

// The students' submissions, and what's read from the other repos the
// grader keeps per course, project and student (enrollment, overrides,
// aliases, results), as r3 targets on the local mirror (`defs.source`). The
// r3 form of what ag.r2 works out from Gitolite.mirror in Course and Project.
//
// Every read is pinned: to the commit a ref snapshot names, or to a commit's
// id. Nothing here clones or keeps a git repo: a commit's files are read
// from the mirror, and copied out (prepare) only where a tool needs them.
final class Submissions(val defs: Definitions) extends Scope("submissions") {
  private val source = defs.source

  def project_repo(c: CourseName, p: ProjectName): String =
    defs.project_repo(c, p)
  def work_repo(c: CourseName, p: ProjectName, csid: CSID): String =
    s"${project_repo(c, p)}_${csid.value}"
  def override_repo(c: CourseName, p: ProjectName): String =
    s"${project_repo(c, p)}__override"
  def aliases_repo(c: CourseName, p: ProjectName): String =
    s"${project_repo(c, p)}__aliases"
  def results_repo(c: CourseName, p: ProjectName): String =
    s"${project_repo(c, p)}__results"
  def student_results_repo(c: CourseName, p: ProjectName, csid: CSID): String =
    repos.StudentResultsRepo.name_of(c, p, csid)
  def enrollment_repo(c: CourseName): String = s"${c}__enrollment"

  private val report_name = os.rel / "REPORT.txt"

  // the time git gives a commit (its committer's), as an instant
  private def time(ci: CommitInfo): Instant =
    OffsetDateTime.parse(ci.committed).nn.toInstant.nn

  private def local(t: java.time.LocalDateTime): ZonedDateTime =
    ZonedDateTime.of(t, ZoneId.systemDefault).nn

  // a file on master, if the repo and the file are there
  private def text_on(
      ctx: Context[?],
      m: Option[Commit],
      file: String
  ): Option[String] =
    m.flatMap(c => ctx.read_text(ctx.commit(c).tree, os.RelPath(file)))

  // ---- enrollment ------------------------------------------------------

  // csid -> their ssh key, from the course's enrollment repo
  lazy val enrollment: CourseName => Target[SortedMap[CSID, String]] = fun {
    (c: CourseName) =>
      target((m = source.master(enrollment_repo(c)))) { (producer, deps) =>
        text_on(producer, deps.m, "enrollment.json") match {
          case Some(json) => upickle.default.read[SortedMap[CSID, String]](json)
          case None       =>
            if (deps.m.isDefined)
              producer.log(s"${enrollment_repo(c)} has no enrollment.json\n")
            SortedMap[CSID, String]()
        }
      }
  }

  lazy val enrolled_csids: CourseName => Target[SortedSet[CSID]] = fun {
    (c: CourseName) => target((e = enrollment(c)))((_, deps) => deps.e.keySet)
  }

  // the course's enrolled students whose CSIDs match `re`
  lazy val enrolled: (CourseName, Regex) => Target[SortedSet[CSID]] = fun {
    (c: CourseName, re: Regex) =>
      target((e = enrolled_csids(c)))((_, deps) =>
        deps.e.filter(csid => re.matches(csid.value))
      )
  }

  // ---- submissions -----------------------------------------------------

  // The student's master, if they have a repo and it's more than the
  // project's starting code (its files differ from the project repo's).
  lazy val submission
      : (CourseName, ProjectName, CSID) => Target[Option[Commit]] = fun {
    (c: CourseName, p: ProjectName, csid: CSID) =>
      target(
        (
          w = source.master(work_repo(c, p, csid)),
          base = source.master(project_repo(c, p))
        )
      ) { (producer, deps) =>
        deps.w.filter(w =>
          deps.base.forall(b =>
            producer.commit(b).tree.id != producer.commit(w).tree.id
          )
        )
      }
  }

  // the enrolled students' submissions
  lazy val submissions
      : (CourseName, ProjectName) => Target[SortedMap[CSID, Commit]] =
    fun { (c: CourseName, p: ProjectName) =>
      complex_target[SortedMap[CSID, Commit]] { tracker =>
        val csids = tracker.track(enrolled_csids(c)).toSeq
        val subs = tracker.track_all(csids.map(csid => submission(c, p, csid)))
        tracker.run_if_needed(_ =>
          csids
            .zip(subs)
            .collect { case (csid, Some(s)) => (csid, s) }
            .to(SortedMap)
        )
      }
    }

  lazy val students_with_submission
      : (CourseName, ProjectName) => Target[SortedSet[CSID]] = fun {
    (c: CourseName, p: ProjectName) =>
      target((s = submissions(c, p)))((_, deps) => deps.s.keySet)
  }

  // Whether the submission's REPORT.txt differs from the project's (either
  // may be missing)
  lazy val has_student_report
      : (CourseName, ProjectName, CSID) => Target[Boolean] = fun {
    (c: CourseName, p: ProjectName, csid: CSID) =>
      target(
        (sub = submission(c, p, csid), base = source.master(project_repo(c, p)))
      ) { (producer, deps) =>
        def report(m: Option[Commit]) =
          m.flatMap(m => producer.id_at(producer.commit(m).tree, report_name))
        report(deps.sub) != report(deps.base)
      }
  }

  // whether the student's repo has a spamon file
  lazy val has_spamon: (CourseName, ProjectName, CSID) => Target[Boolean] =
    fun { (c: CourseName, p: ProjectName, csid: CSID) =>
      target((w = source.master(work_repo(c, p, csid)))) { (producer, deps) =>
        deps.w.exists(m =>
          producer.id_at(producer.commit(m).tree, os.rel / "spamon").isDefined
        )
      }
    }

  // ---- the commit graded -----------------------------------------------

  // The commit graded for `phase` (as Project.graded_commit): the one its
  // commit id file names, if it names one; else the last on master (along
  // first parents) committed by the phase's deadline, or master itself for
  // Phase.Latest. None if there's no submission, or nothing before the
  // deadline. The commit id file is read as it is now, on purpose.
  lazy val graded_commit
      : (CourseName, ProjectName, CSID, Phase) => Target[Option[GradedCommit]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, phase: Phase) =>
      target(
        (
          sub = submission(c, p, csid),
          refs = source.refs(work_repo(c, p, csid)),
          deadline = defs.deadline(c, p, csid, phase)
        )
      ) { (producer, deps) =>
        deps.sub.flatMap { m =>
          val tip = producer.commit(m)
          val named = for {
            name <- phase.commit_id_file
            text <- producer.read_text(tip.tree, os.RelPath(name))
            id <- text.linesIterator.nextOption().map(_.trim.nn)
            if id.nonEmpty
            sha <- {
              val sha = producer.resolve(m, id)
              if (sha.isEmpty)
                producer.log(s"$name names no commit ('$id'): ignored\n")
              sha
            }
          } yield (sha.id, CommitSource.FromFile(name, id))
          val chosen = named.orElse {
            val sha =
              if (phase == Phase.Latest) Some(m.id)
              else {
                val by = local(deps.deadline).toInstant.nn
                producer.history(m).find(ci => !time(ci).isAfter(by)).map(_.id)
              }
            sha.map(_ -> CommitSource.Latest)
          }
          chosen.map { (sha, how) =>
            val notes = for {
              ref <- Seq("refs/notes/push_time", "refs/notes/commits")
              n <- deps.refs.get(ref).toSeq
              text <- producer.note(Commit(m.at, n), sha).toSeq
              line <- text.linesIterator
            } yield line
            GradedCommit(
              sha = sha,
              source = how,
              commit_time = ZonedDateTime
                .ofInstant(
                  time(producer.commit(Commit(m.at, sha))),
                  ZoneOffset.UTC
                )
                .nn,
              push_time = PrepareInfo.push_time_from(notes)
            )
          }
        }
      }
    }

  // what's graded for `phase`, and how it was chosen; None: nothing to grade
  lazy val prepare_info
      : (CourseName, ProjectName, CSID, Phase) => Target[Option[PrepareInfo]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, phase: Phase) =>
      target(
        (
          graded = graded_commit(c, p, csid, phase),
          has_report = has_student_report(c, p, csid)
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

  lazy val is_late: (CourseName, ProjectName, CSID, Phase) => Target[Boolean] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, phase: Phase) =>
      target(
        (
          info = prepare_info(c, p, csid, phase),
          deadline = defs.deadline(c, p, csid, phase)
        )
      ) { (_, deps) =>
        deps.info.forall(_.commit_time.isAfter(local(deps.deadline)))
      }
    }

  // ---- a commit's code, ready to test ----------------------------------

  // Commit `sha` of the student's repo, with the course's overrides on top,
  // its tests removed, and prepared_commit saying which commit it is (as
  // Project.prepare). Depends on the commit by id, not on master: pushing
  // more doesn't redo it.
  lazy val prepare
      : (CourseName, ProjectName, CSID, String) => Target[WithData[Unit]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, sha: String) =>
      target(
        (
          ov = source.master(override_repo(c, p)),
          ext = defs.test_extensions(c, p)
        )
      ) { (producer, deps) =>
        create_data(producer) { dir =>
          val commit = producer.commit(
            Commit(source.dir(work_repo(c, p, csid)).toString, sha)
          )
          producer.copy_to(commit.tree, dir)
          deps.ov.foreach(o => producer.copy_to(producer.commit(o).tree, dir))
          for (f <- os.list(dir) if deps.ext.contains(f.ext)) os.remove.all(f)
          os.write.over(dir / "prepared_commit", s"$sha\n${commit.committed}\n")
        }
      }
    }

  // ---- late commits, and the student's own test ------------------------

  // git's %s: the message's first paragraph, each line's trailing blanks
  // dropped (its leading ones kept), joined by spaces
  private[grader] def subject(message: String): String =
    message.linesIterator
      .dropWhile(_.trim.isEmpty)
      .takeWhile(_.trim.nonEmpty)
      .map(_.stripTrailing.nn)
      .mkString(" ")

  // The commits on master (along first parents) after the student's phase 2
  // deadline, oldest first
  lazy val late_commits
      : (CourseName, ProjectName, CSID) => Target[Seq[LateCommit]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID) =>
      target(
        (
          sub = submission(c, p, csid),
          cutoff = defs.deadline(c, p, csid, Phase.Two)
        )
      ) { (producer, deps) =>
        val cutoff = local(deps.cutoff)
        (for {
          m <- deps.sub.toSeq
          ci <- producer.history(m)
          t = time(ci)
          if !t.isBefore(cutoff.toInstant)
        } yield {
          val at = ZonedDateTime.ofInstant(t, ZoneOffset.UTC).nn
          LateCommit(
            ci.id,
            at,
            Duration.between(cutoff, at).nn,
            subject(ci.message)
          )
        }).reverse
      }
    }

  // The student's own test: their <csid>.<ext> files (every one of the
  // project's test extensions) at the last commit on master, along first
  // parents, made by the project's test cutoff (phase 1 extensions are for
  // code, not tests). Its value is that commit.
  lazy val student_test
      : (CourseName, ProjectName, CSID) => Target[WithData[Option[String]]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID) =>
      target(
        (
          sub = submission(c, p, csid),
          ext = defs.test_extensions(c, p),
          cutoff = defs.test_cutoff(c, p)
        )
      ) { (producer, deps) =>
        val by = local(deps.cutoff).toInstant.nn
        create_data(producer) { dir =>
          for {
            m <- deps.sub
            ci <- producer.history(m).find(ci => !time(ci).isAfter(by))
            names = deps.ext.toSeq.map(e => s"${csid.value}.$e")
            if names.forall(n =>
              producer.id_at(ci.tree, os.RelPath(n)).isDefined
            )
          } yield {
            // a file (streamed: it can be bigger than memory) or a
            // directory (e.g. for a "dir" extension)
            for (n <- names) {
              val _ = producer.copy_path(ci.tree, os.RelPath(n), dir / n)
            }
            ci.id
          }
        }
      }
    }

  lazy val student_tests_by_csid
      : (CourseName, ProjectName) => Target[SortedMap[CSID, WithData[String]]] =
    fun { (c: CourseName, p: ProjectName) =>
      complex_target[SortedMap[CSID, WithData[String]]] { tracker =>
        val csids = tracker.track(students_with_submission(c, p)).toSeq
        val tests =
          tracker.track_all(csids.map(csid => student_test(c, p, csid)))
        tracker.run_if_needed(_ =>
          (for {
            (csid, t) <- csids.zip(tests)
            rev <- t.value.toSeq
          } yield (csid, WithData(rev, t.data))).to(SortedMap)
        )
      }
    }

  lazy val csids_with_tests
      : (CourseName, ProjectName) => Target[SortedSet[CSID]] = fun {
    (c: CourseName, p: ProjectName) =>
      target((t = student_tests_by_csid(c, p)))((_, deps) => deps.t.keySet)
  }

  // ---- what the grader published, read back ----------------------------

  // the project's aliases (none if the repo or file isn't there, or can't
  // be read)
  lazy val aliases
      : (CourseName, ProjectName) => Target[SortedMap[CSID, Alias]] = fun {
    (c: CourseName, p: ProjectName) =>
      target((m = source.master(aliases_repo(c, p)))) { (producer, deps) =>
        text_on(producer, deps.m, "aliases.json")
          .flatMap(json =>
            Try(upickle.default.read[SortedMap[CSID, Alias]](json)).toOption
          )
          .getOrElse(SortedMap[CSID, Alias]())
      }
  }

  private def readable[A: upickle.default.Reader](
      json: Option[String],
      name: String
  ): Boolean =
    json.exists(j =>
      Try(NoNulls.check(upickle.default.read[A](j), name)).isSuccess
    )

  // whether the student's published results are there and this grader can
  // read them
  lazy val student_results_readable
      : (CourseName, ProjectName, CSID) => Target[Boolean] =
    fun { (c: CourseName, p: ProjectName, csid: CSID) =>
      target((m = source.master(student_results_repo(c, p, csid)))) {
        (producer, deps) =>
          readable[StudentResults](
            text_on(producer, deps.m, "the_results.json"),
            "the_results.json"
          )
      }
    }

  lazy val results_readable
      : (CourseName, ProjectName, Phase) => Target[Boolean] = fun {
    (c: CourseName, p: ProjectName, phase: Phase) =>
      target((m = source.master(results_repo(c, p)))) { (producer, deps) =>
        readable[SortedMap[Alias, RedactedStudentResults]](
          text_on(producer, deps.m, phase.results_file),
          phase.results_file
        )
      }
  }

  // A phase's published results, by alias (as Project.results): Right(None)
  // if there are none yet, Left(why) if they can't be read by this version
  // of the grader
  lazy val results: (CourseName, ProjectName, Phase) => Target[
    Either[String, Option[SortedMap[Alias, RedactedStudentResults]]]
  ] = fun { (c: CourseName, p: ProjectName, phase: Phase) =>
    target((m = source.master(results_repo(c, p)))) { (producer, deps) =>
      val out: Either[
        String,
        Option[SortedMap[Alias, RedactedStudentResults]]
      ] = text_on(producer, deps.m, phase.results_file) match {
        case None       => Right(None)
        case Some(json) =>
          Try(
            NoNulls.check(
              upickle.default
                .read[SortedMap[Alias, RedactedStudentResults]](json),
              phase.results_file
            )
          ).toEither.left
            .map(e =>
              s"${results_repo(c, p)}'s ${phase.results_file} isn't readable by this version " +
                s"of the grader; `run grade -c $c -p $p` rewrites it (${String.valueOf(e.getMessage)})"
            )
            .map(Some(_))
      }
      out
    }
  }

  // The student's published results, if there are any. Fails if they can't
  // be read, saying what rewrites them (as Project.get_student_results).
  lazy val student_results
      : (CourseName, ProjectName, CSID) => Target[Option[StudentResults]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID) =>
      target((m = source.master(student_results_repo(c, p, csid)))) {
        (producer, deps) =>
          text_on(producer, deps.m, "the_results.json").map { json =>
            Try(
              NoNulls.check(
                upickle.default.read[StudentResults](json),
                "the_results.json"
              )
            )
              .fold(
                e =>
                  throw IllegalStateException(
                    s"${student_results_repo(c, p, csid)}'s the_results.json isn't readable by this " +
                      s"version of the grader; `run publish_results -c $c -p $p -s ${csid.value}` " +
                      s"rewrites it (${e.getMessage})",
                    e
                  ),
                identity
              )
          }
      }
    }

  // the student's published outcomes of the tests whose names match `re`
  // (none if there are no results)
  lazy val student_outcomes: (CourseName, ProjectName, CSID, Regex) => Target[
    SortedMap[RedactedTestId, RedactedOutcome]
  ] = fun { (c: CourseName, p: ProjectName, csid: CSID, re: Regex) =>
    target((r = student_results(c, p, csid)))((_, deps) =>
      deps.r.toSeq
        .flatMap(_.outcomes)
        .filter((t, _) => re.matches(t.external_name))
        .to(SortedMap)
    )
  }
}
