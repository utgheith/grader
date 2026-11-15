package ag.grader

import ag.common.{NoNulls, ToRelPath, given_ReadWriter_SortedMap}
import ag.grader.repos.Instance
import ag.rules.{Commit, CommitInfo, Source, Target, WithData, create_data}

import java.time.{Instant, OffsetDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}
import scala.util.Try
import scala.util.matching.Regex

// A student's targets for a project: their submission, the commit graded
// for each phase and its code ready to test, their own test, running the
// tests on it, their results, and what the grader published for them, read
// back from the local mirror.
//
// Every read is pinned: to the commit a ref snapshot names, or to a commit's
// id. Nothing here clones or keeps a git repo: a commit's files are read
// from the mirror, and copied out (prepare) only where a tool needs them.
final case class Submission(
    c: CourseName,
    p: ProjectName,
    csid: CSID,
    source: Source
) extends Instance[Submission] derives ToRelPath {
  private val report_name = os.rel / "REPORT.txt"

  // the time git gives a commit (its committer's), as an instant
  private def time(ci: CommitInfo): Instant =
    OffsetDateTime.parse(ci.committed).nn.toInstant.nn

  private def local(t: java.time.LocalDateTime): ZonedDateTime =
    ZonedDateTime.of(t, ZoneId.systemDefault).nn

  // ---- the submission, the commit graded, its code ready to test ------

  // The student's master, if they have a repo and it's more than the
  // project's starting code (its files differ from the project repo's).
  lazy val submission: Target[Option[Commit]] =
    target(
      (
        w = source.master(repos.WorkRepo.name_of(c, p, csid)),
        base = source.master(repos.ProjectRepo.name_of(c, p))
      )
    ) { (producer, deps) =>
      deps.w.filter(w =>
        deps.base.forall(b =>
          producer.commit(b).tree.id != producer.commit(w).tree.id
        )
      )
    }

  // Whether the submission's REPORT.txt differs from the project's (either
  // may be missing)
  lazy val has_student_report: Target[Boolean] =
    target(
      (
        sub = submission,
        base = source.master(repos.ProjectRepo.name_of(c, p))
      )
    ) { (producer, deps) =>
      def report(m: Option[Commit]) =
        m.flatMap(m => producer.id_at(producer.commit(m).tree, report_name))
      report(deps.sub) != report(deps.base)
    }

  // whether the student's repo has a spamon file
  lazy val has_spamon: Target[Boolean] =
    target((w = source.master(repos.WorkRepo.name_of(c, p, csid)))) {
      (producer, deps) =>
        deps.w.exists(m =>
          producer.id_at(producer.commit(m).tree, os.rel / "spamon").isDefined
        )
    }

  // The commit graded for `phase` (as Project.graded_commit): the one its
  // commit id file names, if it names one; else the last on master (along
  // first parents) committed by the phase's deadline, or master itself for
  // Phase.Latest. None if there's no submission, or nothing before the
  // deadline. The commit id file is read as it is now, on purpose.
  lazy val graded_commit: Phase => Target[Option[GradedCommit]] = fun {
    (phase: Phase) =>
      target(
        (
          sub = submission,
          refs = source.refs(repos.WorkRepo.name_of(c, p, csid)),
          deadline = Project(c, p, source).deadline(csid, phase)
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
                producer
                  .history(m)
                  .find(ci => !time(ci).isAfter(by))
                  .map(_.id)
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
  lazy val prepare_info: Phase => Target[Option[PrepareInfo]] = fun {
    (phase: Phase) =>
      target(
        (
          graded = graded_commit(phase),
          has_report = has_student_report
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

  lazy val is_late: Phase => Target[Boolean] = fun { (phase: Phase) =>
    target(
      (
        info = prepare_info(phase),
        deadline = Project(c, p, source).deadline(csid, phase)
      )
    ) { (_, deps) =>
      deps.info.forall(_.commit_time.isAfter(local(deps.deadline)))
    }
  }

  // Commit `sha` of the student's repo, with the course's overrides on top,
  // its tests removed, and prepared_commit saying which commit it is (as
  // Project.prepare). Depends on the commit by id, not on master: pushing
  // more doesn't redo it.
  lazy val prepare: String => Target[WithData[Unit]] = fun { (sha: String) =>
    target(
      (
        ov = source.master(repos.OverrideRepo.name_of(c, p)),
        ext = Project(c, p, source).test_extensions
      )
    ) { (producer, deps) =>
      create_data(producer) { dir =>
        val commit = producer.commit(
          Commit(
            source.dir(repos.WorkRepo.name_of(c, p, csid)).toString,
            sha
          )
        )
        producer.copy_to(commit.tree, dir)
        deps.ov.foreach(o => producer.copy_to(producer.commit(o).tree, dir))
        for (f <- os.list(dir) if deps.ext.contains(f.ext)) os.remove.all(f)
        os.write.over(dir / "prepared_commit", s"$sha\n${commit.committed}\n")
      }
    }
  }

  // The student's own test: their <csid>.<ext> files (every one of the
  // project's test extensions) at the last commit on master, along first
  // parents, made by the project's test cutoff (phase 1 extensions are for
  // code, not tests). Its value is that commit.
  lazy val test: Target[WithData[Option[String]]] =
    target(
      (
        sub = submission,
        ext = Project(c, p, source).test_extensions,
        cutoff = Project(c, p, source).test_cutoff
      )
    ) { (producer, deps) =>
      val by = local(deps.cutoff).toInstant.nn
      create_data(producer) { dir =>
        for {
          m <- deps.sub
          ci <- producer.history(m).find(ci => !time(ci).isAfter(by))
          names = deps.ext.toSeq.map(e => s"${csid.value}.$e")
          if names.forall(n => producer.id_at(ci.tree, os.RelPath(n)).isDefined)
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

  // whether the student's published results are there and this grader can
  // read them
  lazy val published_results_readable: Target[Boolean] =
    target(
      (m = source.master(repos.StudentResultsRepo.name_of(c, p, csid)))
    ) { (producer, deps) =>
      Project.readable[StudentResults](
        Project.text_on(producer, deps.m, "the_results.json"),
        "the_results.json"
      )
    }

  // The student's published results, if there are any. Fails if they can't
  // be read, saying what rewrites them (as Project.get_student_results).
  lazy val published_results: Target[Option[StudentResults]] =
    target(
      (m = source.master(repos.StudentResultsRepo.name_of(c, p, csid)))
    ) { (producer, deps) =>
      Project.text_on(producer, deps.m, "the_results.json").map { json =>
        Try(
          NoNulls.check(
            upickle.default.read[StudentResults](json),
            "the_results.json"
          )
        )
          .fold(
            e =>
              throw IllegalStateException(
                s"${repos.StudentResultsRepo.name_of(c, p, csid)}'s the_results.json isn't readable by this " +
                  s"version of the grader; `run grade -c $c -p $p` " +
                  s"rewrites it (${e.getMessage})",
                e
              ),
            identity
          )
      }
    }

  // the student's published outcomes of the tests whose names match `re`
  // (none if there are no results)
  lazy val published_outcomes
      : Regex => Target[SortedMap[RedactedTestId, RedactedOutcome]] = fun {
    (re: Regex) =>
      target((r = published_results))((_, deps) =>
        deps.r.toSeq
          .flatMap(_.outcomes)
          .filter((t, _) => re.matches(t.external_name))
          .to(SortedMap)
      )
  }

  // ---- grading -------------------------------------------------------

  lazy val alias: Target[Option[Alias]] =
    target((aliases = Project(c, p, source).aliases))((_, deps) =>
      deps.aliases.get(csid)
    )

  lazy val has_test: Target[Boolean] =
    target((with_tests = Project(c, p, source).csids_with_tests))((_, deps) =>
      deps.with_tests.contains(csid)
    )

  // A student's graded commit for `phase`, run on the tests the phase runs
  // (each test's runs), and the other tests it shows, as far as they've run
  // already (see run_one's n = 0; as Project.results_runs). A test
  // without runs is left out; none if there's nothing to grade.
  lazy val outcomes: (Phase, Int) => Target[Seq[WithData[Outcome]]] = fun {
    (phase: Phase, n: Int) =>
      complex_target[Seq[WithData[Outcome]]] { tracker =>
        val st = tracker.track(Project(c, p, source).state)
        val keep_going = st.keeps_going(phase)
        val run =
          tracker.track(Project(c, p, source).test_ids_for(st.tests(phase)))
        val others = tracker
          .track(Project(c, p, source).test_ids_for(st.shows(phase)))
          .filterNot(run.contains)
        val wanted = for {
          _ <- tracker.track(prepare_info(phase)).toSeq
          (id, tries) <- run.toSeq.map((_, n)) ++ others.toSeq.map((_, 0))
        } yield run_one(keep_going, tries)(phase, id)
        val done = tracker.track_all(wanted)
        tracker.run_if_needed(_ => done.filter(_.value.outcomes.nonEmpty))
      }
  }

  // A student's results for `phase`: their graded commit, run on the
  // phase's tests (as Project.student_results). None if there's nothing to
  // grade.
  lazy val results: (Phase, Int) => Target[Option[StudentResults]] = fun {
    (phase: Phase, n: Int) =>
      target(
        (
          info = prepare_info(phase),
          outcomes = outcomes(phase, n),
          alias = alias,
          has_test = has_test
        )
      ) { (_, deps) =>
        deps.info.map(info =>
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

  // The phase 2 tests a student's graded commit for `phase` passed at least
  // once in `n` tries, every test run to the end (as
  // Project.compute_student_scores)
  lazy val scores: (Phase, Int) => Target[SortedSet[TestId]] = fun {
    (phase: Phase, n: Int) =>
      complex_target[SortedSet[TestId]] { tracker =>
        val wanted = for {
          _ <- tracker.track(prepare_info(phase)).toSeq
          id <- tracker.track(Project(c, p, source).phase2_test_ids).toSeq
        } yield run_one(true, n)(phase, id)
        val outs = tracker.track_all(wanted)
        tracker.run_if_needed { _ =>
          (for {
            o <- outs
            if o.value.outcomes.exists(_._1 == OutcomeStatus.pass)
          } yield o.value.test_id).to(SortedSet)
        }
      }
  }

  // ---- running the tests -----------------------------------------------

  // A test's runs on a student's prepared submission for a phase (as
  // Project.run_one): what their prepared repo has for it (see
  // repos.PreparedRepo.prepared), so they run on what the student sees. The
  // outcome of each run so far, the latest run's output files as its data.
  // The runs are kept by what they ran on (see TestRun), so they stay
  // whatever this target is called, and phases graded at the same commit
  // share them; it runs again while it has fewer than `n` runs, but stops
  // at a failure unless `keep_going`. With n = 0, never runs: just the runs
  // it has, none if it has none. Fails if the phase has no commit to grade.
  def run_one(
      keep_going: Boolean,
      n: Int
  ): (Phase, TestId) => Target[WithData[Outcome]] =
    fun { (phase: Phase, id: TestId) =>
      complex_target[WithData[Outcome]] { tracker =>
        val deps = tracker.track_all(
          (
            test = Project(c, p, source).test_info(id),
            info = prepare_info(phase),
            prepared = repos.PreparedRepo(c, p, csid, source).prepared(phase),
            cores = Project(c, p, source).cores,
            docker_file = Project(c, p, source).docker_file,
            image = Project(c, p, source).docker_image
          )
        )
        val (sha, prepared) = (deps.info, deps.prepared) match {
          case (Some(info), Some(tree)) => (info.sha, tree)
          case _                        =>
            throw IllegalStateException(
              s"${c}_${p}_${csid.value} has no commit to grade for ${phase.toString}"
            )
        }
        val runs = tracker.track(
          TestRun(
            prepared = prepared,
            test = deps.test.data,
            test_name = id.external_name,
            cores = deps.cores,
            docker_file = deps.docker_file,
            image = deps.image
          )
            .runs(id, s"${c}_${p}_${csid.value}", keep_going, n)
        )
        tracker.run_if_needed { _ =>
          WithData(
            Outcome(
              course = c,
              project = p,
              csid = csid,
              test_id = id,
              outcomes = runs.value.outcomes,
              commit_id = Some(sha)
            ),
            runs.data
          )
        }
      }
    }
}
