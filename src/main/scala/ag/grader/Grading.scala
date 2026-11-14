package ag.grader

import ag.common.{given_ReadWriter_SortedMap, given_ReadWriter_SortedSet}
import ag.r3.{Scope, Target, WithData, periodic}

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

// Grading: the students' results and scores, as r3 targets on the local
// mirror. The r3 form of student_results, has_work and compute_scores in
// Project. What's published from them is in Publishers.
//
// Which tests a phase runs, and whether a test keeps running after it fails,
// come from the project's state (as the publish_results command picks them
// in ag.r2); `n` is how many tries each test gets (see TestRuns.run_one).
final class Grading(val runs: TestRuns) extends Scope("grading") {
  private val subs = runs.subs
  private val defs = subs.defs

  // Where the project is relative to its deadlines (as Project.state):
  // worked out again every minute as time passes; what depends on it reruns
  // only when it changes.
  lazy val state: (CourseName, ProjectName) => Target[ProjectState] = fun {
    (c: CourseName, p: ProjectName) =>
      target(
        (
          first = defs.test_cutoff(c, p),
          second = defs.code_cutoff(c, p),
          clock = periodic(60 * 1000L)
        )
      ) { (_, deps) =>
        ProjectState.at(LocalDateTime.now().nn, deps.first, deps.second)
      }
  }

  lazy val alias: (CourseName, ProjectName, CSID) => Target[Option[Alias]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID) =>
      target((aliases = runs.aliases(c, p)))((_, deps) =>
        deps.aliases.get(csid)
      )
    }

  lazy val has_test: (CourseName, ProjectName, CSID) => Target[Boolean] = fun {
    (c: CourseName, p: ProjectName, csid: CSID) =>
      target((with_tests = subs.csids_with_tests(c, p)))((_, deps) =>
        deps.with_tests.contains(csid)
      )
  }

  // A student's graded commit for `phase`, run on the tests the phase runs
  // (each test's runs), and the other tests it shows, as far as they've run
  // already (see TestRuns.run_one's n = 0; as Project.results_runs). A test
  // without runs is left out; none if there's nothing to grade.
  lazy val outcomes: (CourseName, ProjectName, CSID, Phase, Int) => Target[
    Seq[WithData[Outcome]]
  ] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, phase: Phase, n: Int) =>
      complex_target[Seq[WithData[Outcome]]] { tracker =>
        val st = tracker.track(state(c, p))
        val keep_going = st.keeps_going(phase)
        val run = tracker.track(runs.test_ids_for(c, p, st.tests(phase)))
        val others = tracker
          .track(runs.test_ids_for(c, p, st.shows(phase)))
          .filterNot(run.contains)
        val wanted = for {
          _ <- tracker.track(subs.prepare_info(c, p, csid, phase)).toSeq
          (id, tries) <- run.toSeq.map((_, n)) ++ others.toSeq.map((_, 0))
        } yield runs.run_one(keep_going, tries)(c, p, csid, phase, id)
        val done = tracker.track_all(wanted)
        tracker.run_if_needed(_ => done.filter(_.value.outcomes.nonEmpty))
      }
    }

  // A student's results for `phase`: their graded commit, run on the
  // phase's tests (as Project.student_results). None if there's nothing to
  // grade.
  lazy val student_results
      : (CourseName, ProjectName, CSID, Phase, Int) => Target[
        Option[StudentResults]
      ] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, phase: Phase, n: Int) =>
      target(
        (
          info = subs.prepare_info(c, p, csid, phase),
          outcomes = outcomes(c, p, csid, phase, n),
          alias = alias(c, p, csid),
          has_test = has_test(c, p, csid)
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

  // Every student's results for `phase`, by alias, as published (as
  // Project.publish_results): late is from each student's own deadline for
  // the phase; students without an alias are left out.
  lazy val phase_results: (CourseName, ProjectName, Phase, Int) => Target[
    SortedMap[Alias, RedactedStudentResults]
  ] = fun { (c: CourseName, p: ProjectName, phase: Phase, n: Int) =>
    complex_target[SortedMap[Alias, RedactedStudentResults]] { tracker =>
      val students = tracker.track(subs.students_with_submission(c, p)).toSeq
      val results =
        tracker.track_all(students.map(student_results(c, p, _, phase, n)))
      val deadlines =
        tracker.track_all(students.map(defs.deadline(c, p, _, phase)))
      val aliases = tracker.track_all(students.map(alias(c, p, _)))
      tracker.run_if_needed { _ =>
        (for {
          ((r, d), a) <- results.zip(deadlines).zip(aliases)
          r <- r.toSeq
          a <- a.toSeq
        } yield a -> r.redacted(ZonedDateTime.of(d, ZoneId.systemDefault).nn))
          .to(SortedMap)
      }
    }
  }

  // The results files of the phases the project publishes now (see
  // ProjectState.publishes), by file name (Phase.results_file)
  lazy val results_files: (CourseName, ProjectName, Int) => Target[
    SortedMap[String, SortedMap[Alias, RedactedStudentResults]]
  ] = fun { (c: CourseName, p: ProjectName, n: Int) =>
    complex_target[
      SortedMap[String, SortedMap[Alias, RedactedStudentResults]]
    ] { tracker =>
      val st = tracker.track(state(c, p))
      val phases =
        Seq(Phase.Latest, Phase.One, Phase.Two).filter(st.publishes)
      val results = tracker.track_all(phases.map(phase_results(c, p, _, n)))
      tracker.run_if_needed(_ =>
        phases.map(_.results_file).zip(results).to(SortedMap)
      )
    }
  }

  // Whether grading `phase` to `n` tries has tests to run, as far as the
  // published results tell (as Project.has_work): a student's graded commit
  // isn't in them, they don't cover the tests the phase runs (or have ones
  // it doesn't show), or a test it runs has tries to go. Missing or
  // unreadable results are work too.
  lazy val has_work: (CourseName, ProjectName, Phase, Int) => Target[Boolean] =
    fun { (c: CourseName, p: ProjectName, phase: Phase, n: Int) =>
      complex_target[Boolean] { tracker =>
        val st = tracker.track(state(c, p))
        val keep_going = st.keeps_going(phase)
        val students = tracker.track(subs.students_with_submission(c, p)).toSeq
        val published = tracker.track(subs.results(c, p, phase))
        val ids =
          tracker
            .track(runs.test_ids_for(c, p, st.tests(phase)))
            .map(_.redacted)
        // the tests that may be shown besides, with the runs they have:
        // never work
        val shown =
          tracker
            .track(runs.test_ids_for(c, p, st.shows(phase)))
            .map(_.redacted)
        val infos =
          tracker.track_all(students.map(subs.prepare_info(c, p, _, phase)))
        val aliases = tracker.track_all(students.map(alias(c, p, _)))
        tracker.run_if_needed { _ =>
          published.toOption.flatten match {
            case None            => true
            case Some(published) =>
              infos.zip(aliases).exists {
                // the results leave out students without an alias
                case (Some(info), Some(a)) =>
                  published.get(a) match {
                    case None    => true
                    case Some(r) =>
                      r.prepare_info.sha != info.sha ||
                      !ids.subsetOf(r.outcomes.keySet) ||
                      !r.outcomes.keySet.subsetOf(shown) ||
                      r.outcomes.view
                        .filterKeys(ids.contains)
                        .values
                        .exists { o =>
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

  // ---- report pages ----------------------------------------------------

  // What a project's report pages show (as HtmlGen.page): its published
  // results, from the mirror, and its settings
  lazy val report_inputs: (CourseName, ProjectName) => Target[ReportInputs] =
    fun { (c: CourseName, p: ProjectName) =>
      target(
        (
          latest = subs.results(c, p, Phase.Latest),
          one = subs.results(c, p, Phase.One),
          two = subs.results(c, p, Phase.Two),
          state = state(c, p),
          phase1_tests = defs.phase1_tests(c, p),
          phase1_weight = defs.phase1_weight(c, p),
          phase2_tests = defs.phase2_tests(c, p),
          phase2_weight = defs.phase2_weight(c, p),
          weights = defs.test_weights(c, p),
          bad_tests = defs.bad_tests(c, p),
          test_cutoff = defs.test_cutoff(c, p),
          code_cutoff = defs.code_cutoff(c, p),
          enrollment = subs.enrollment(c),
          aliases = runs.aliases(c, p),
          course_staff = defs.course_staff(c),
          project_staff = defs.project_staff(c, p),
          projects = defs.active_projects(c, Definitions.any)
        )
      ) { (_, deps) =>
        ReportInputs(
          title = s"${c}_$p",
          project_name = p.value,
          test_cutoff = deps.test_cutoff,
          code_cutoff = deps.code_cutoff,
          enrollments = deps.enrollment.size,
          results = Seq(
            Phase.Latest -> deps.latest,
            Phase.One -> deps.one,
            Phase.Two -> deps.two
          ).map((phase, r) => (phase, r, deps.state.publishes(phase))),
          phase1_tests = deps.phase1_tests,
          phase1_weight = deps.phase1_weight,
          phase2_tests = deps.phase2_tests,
          phase2_weight = deps.phase2_weight,
          weights = deps.weights,
          bad_tests = deps.bad_tests,
          staff_aliases = HtmlGen.staff_aliases(
            deps.aliases,
            deps.course_staff ++ deps.project_staff
          ),
          tabs = HtmlGen.tabs(c.value, p.value, deps.projects.map(_.value))
        )
      }
    }

  // every active project's
  lazy val site_inputs: CourseName => Target[Seq[ReportInputs]] = fun {
    (c: CourseName) =>
      complex_target[Seq[ReportInputs]] { tracker =>
        val projects =
          tracker.track(defs.active_projects(c, Definitions.any)).toSeq
        val inputs = tracker.track_all(projects.map(report_inputs(c, _)))
        tracker.run_if_needed(_ => inputs)
      }
  }

  // ---- scores -----------------------------------------------------------

  // The phase 2 tests a student's graded commit for `phase` passed at least
  // once in `n` tries, every test run to the end (as
  // Project.compute_student_scores)
  lazy val student_scores
      : (CourseName, ProjectName, CSID, Phase, Int) => Target[
        SortedSet[TestId]
      ] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, phase: Phase, n: Int) =>
      complex_target[SortedSet[TestId]] { tracker =>
        val wanted = for {
          _ <- tracker.track(subs.prepare_info(c, p, csid, phase)).toSeq
          id <- tracker.track(runs.phase2_test_ids(c, p)).toSeq
        } yield runs.run_one(true, n)(c, p, csid, phase, id)
        val outs = tracker.track_all(wanted)
        tracker.run_if_needed { _ =>
          (for {
            o <- outs
            if o.value.outcomes.exists(_._1 == OutcomeStatus.pass)
          } yield o.value.test_id).to(SortedSet)
        }
      }
    }

  lazy val scores: (CourseName, ProjectName, Phase, Int) => Target[
    SortedMap[CSID, SortedSet[TestId]]
  ] =
    fun { (c: CourseName, p: ProjectName, phase: Phase, n: Int) =>
      complex_target[SortedMap[CSID, SortedSet[TestId]]] { tracker =>
        val csids = tracker.track(subs.students_with_submission(c, p)).toSeq
        val passed =
          tracker.track_all(csids.map(student_scores(c, p, _, phase, n)))
        tracker.run_if_needed(_ => csids.zip(passed).to(SortedMap))
      }
    }
}
