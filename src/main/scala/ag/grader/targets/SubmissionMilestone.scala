package ag.grader.targets

import ag.grader.Milestone

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.json.{
  StudentResults,
  Outcome,
  OutcomeStatus,
  TestId,
  PrepareInfo,
  PublishedRuns,
  CommitSource,
  GradedCommit
}

import ag.common.{ToRelPath, given_ReadWriter_LocalDateTime}
import ag.rules.{Commit, DerivedScope, Source, Target, WithData, create_data}

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

// A student's targets for one of a project's milestones (see Milestone):
// their deadline, the commit graded, their own test as of the milestone,
// running the tests, and their results.
//
// Every read is pinned, as Submission's are.
final case class SubmissionMilestone(
    submission: Submission,
    milestone: Milestone
) extends DerivedScope[SubmissionMilestone] derives ToRelPath {
  val source: Source = submission.source
  private def project: Project = submission.project
  private def csid: CSID = submission.csid
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  private def project_milestone = project.milestone(milestone)

  // ---- the commit graded ---------------------------------------------

  // the student's deadline, with any extension they have (as
  // Project.deadline)
  lazy val deadline: Target[LocalDateTime] =
    target((rp = project.raw))((_, d) => d.rp.deadline(csid, milestone))

  // The commit graded (as Project.graded_commit): the one the milestone's
  // commit id file names, if it names one; else the last on master (along
  // first parents) committed by the deadline, or master itself for Live.
  // None if there's no submission, or nothing before the deadline. The
  // commit id file is read as it is now, on purpose.
  lazy val graded_commit: Target[Option[GradedCommit]] =
    target(
      (
        sub = submission.submission,
        refs = source.refs(ag.grader.repos.StudentRepo.name_of(c, csid)),
        deadline = deadline
      )
    ) { (producer, deps) =>
      deps.sub.flatMap { m =>
        val tip = producer.commit(m)
        val named = for {
          name <- milestone.commit_id_file
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
            if (milestone == Milestone.Live) Some(m.id)
            else {
              val by = Submission.local(deps.deadline).toInstant.nn
              producer
                .history(m)
                .find(ci => !Submission.time(ci).isAfter(by))
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
                Submission.time(producer.commit(Commit(m.at, sha))),
                ZoneOffset.UTC
              )
              .nn,
            push_time = PrepareInfo.push_time_from(notes)
          )
        }
      }
    }

  // what's graded, and how it was chosen; None: nothing to grade
  lazy val prepare_info: Target[Option[PrepareInfo]] =
    target(
      (graded = graded_commit, has_report = submission.has_student_report)
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

  lazy val is_late: Target[Boolean] =
    target((info = prepare_info, deadline = deadline)) { (_, deps) =>
      deps.info.forall(
        _.commit_time.isAfter(Submission.local(deps.deadline))
      )
    }

  // ---- the student's own test -----------------------------------------

  // Their test: their <csid>.<ext> files (every one of the project's test
  // extensions) at the last commit on master, along first parents, made by
  // the milestone's deadline (Live: the final one's; extensions are for
  // code, not tests). Its value is that commit.
  lazy val test: Target[WithData[Option[String]]] =
    target(
      (
        sub = submission.submission,
        ext = project.test_extensions,
        rp = project.raw
      )
    ) { (producer, deps) =>
      val by = Submission.local(deps.rp.deadline(milestone)).toInstant.nn
      create_data(producer) { dir =>
        for {
          m <- deps.sub
          ci <- producer
            .history(m)
            .find(ci => !Submission.time(ci).isAfter(by))
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

  lazy val has_test: Target[Boolean] =
    target((with_tests = project_milestone.csids_with_tests))((_, deps) =>
      deps.with_tests.contains(csid)
    )

  // ---- grading -------------------------------------------------------

  // The graded commit, run on the tests the milestone runs (each test's
  // runs), and the other tests it shows, as far as they've run already
  // (see run_one's n = 0; as Project.results_runs). A test without runs is
  // left out; none if there's nothing to grade.
  lazy val outcomes: Int => Target[Seq[WithData[Outcome]]] = fun { (n: Int) =>
    complex_target[Seq[WithData[Outcome]]] { tracker =>
      val st = tracker.track(project.state)
      val keep_going = st.keeps_going(milestone)
      val run = tracker.track(project_milestone.ids_for(st.tests(milestone)))
      val others = tracker
        .track(project_milestone.ids_for(st.shows(milestone)))
        .filterNot(run.contains)
      val wanted = for {
        _ <- tracker.track(prepare_info).toSeq
        (id, tries) <- run.toSeq.map((_, n)) ++ others.toSeq.map((_, 0))
      } yield run_one(keep_going, tries, id)
      val done = tracker.track_all(wanted)
      tracker.run_if_needed(_ => done.filter(_.value.outcomes.nonEmpty))
    }
  }

  // The student's results: their graded commit, run on the milestone's
  // tests (as Project.student_results). None if there's nothing to grade.
  lazy val results: Int => Target[Option[StudentResults]] = fun { (n: Int) =>
    target(
      (
        info = prepare_info,
        outcomes = outcomes(n),
        alias = submission.alias,
        has_test = has_test
      )
    ) { (_, deps) =>
      deps.info.map(info =>
        StudentResults(
          csid = csid,
          alias = deps.alias,
          has_test = deps.has_test,
          prepare_info = info,
          // keyed by alias: a student test's internal name is its author's
          // CSID
          outcomes = deps.outcomes
            .map(o => o.value.test_id.redacted -> o.value.redacted)
            .to(SortedMap)
        )
      )
    }
  }

  // The milestone's tests (Live: every milestone's) the graded commit
  // passed at least once in `n` tries, every test run to the end (as
  // Project.compute_student_scores)
  lazy val scores: Int => Target[SortedSet[TestId]] = fun { (n: Int) =>
    complex_target[SortedSet[TestId]] { tracker =>
      val wanted = for {
        _ <- tracker.track(prepare_info).toSeq
        id <- tracker.track(project_milestone.counted_test_ids).toSeq
      } yield run_one(true, n, id)
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

  // The student's runs as published for the milestone, by test (external
  // name), if they were of the commit graded now: what seeds a test's runs
  // when it has none saved (see TestRun.runs). None for a test the
  // published results don't have.
  lazy val seed
      : Target[SortedMap[String, Seq[(OutcomeStatus, Option[Double])]]] =
    target(
      (
        published = project_milestone.published_runs,
        alias = submission.alias,
        info = prepare_info
      )
    ) { (_, deps) =>
      (for {
        a <- deps.alias
        info <- deps.info
        r: PublishedRuns <- deps.published.get(a)
        if r.sha == info.sha
      } yield r.runs).getOrElse(SortedMap())
    }

  // A test's runs on the student's prepared submission (as
  // Project.run_one): what their prepared repo has for the milestone (see
  // ag.grader.repos.PreparedRepo.prepared), so they run on what the student sees. The
  // outcome of each run so far, the latest run's output files as its data.
  // The runs are kept by what they ran on (see TestRun), so they stay
  // whatever this target is called, and milestones graded at the same
  // commit share them; it runs again while it has fewer than `n` runs, but
  // stops at a failure unless `keep_going`. With n = 0, never runs: just the
  // runs it has, none if it has none. Fails if there's no commit to grade.
  // (keep_going and n are in its name: asked with others, it's another
  // target, not whichever was asked first)
  lazy val run_one: (Boolean, Int, TestId) => Target[WithData[Outcome]] =
    fun { (keep_going: Boolean, n: Int, id: TestId) =>
      complex_target[WithData[Outcome]] { tracker =>
        val deps = tracker.track_all(
          (
            test = project_milestone.test_info(id),
            info = prepare_info,
            prepared =
              ag.grader.repos.PreparedRepo(submission).prepared(milestone),
            cores = project.cores,
            docker_file = project.docker_file,
            image = project.docker_image,
            seed = seed
          )
        )
        val (sha, prepared) = (deps.info, deps.prepared) match {
          case (Some(info), Some(tree)) => (info.sha, tree)
          case _                        =>
            throw IllegalStateException(
              s"${c}_${p}_${csid.value} has no commit to grade for ${milestone.toString}"
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
            .runs(
              id,
              s"${c}_${p}_${csid.value} (${milestone.slug})",
              keep_going,
              n,
              deps.seed.getOrElse(id.external_name, Seq())
            )
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
