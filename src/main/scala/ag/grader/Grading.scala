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
  lazy val state: (String, String) => Target[ProjectState] = fun {
    (c: String, p: String) =>
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

  lazy val alias: (String, String, CSID) => Target[Option[Alias]] = fun {
    (c: String, p: String, csid: CSID) =>
      target((aliases = runs.aliases(c, p)))((_, deps) =>
        deps.aliases.get(csid)
      )
  }

  lazy val has_test: (String, String, CSID) => Target[Boolean] = fun {
    (c: String, p: String, csid: CSID) =>
      target((with_tests = subs.csids_with_tests(c, p)))((_, deps) =>
        deps.with_tests.contains(csid)
      )
  }

  // A student's graded commit for `phase`, run on the phase's tests (each
  // test's runs); none if there's nothing to grade
  lazy val outcomes
      : (String, String, CSID, Phase, Int) => Target[Seq[WithData[Outcome]]] =
    fun { (c: String, p: String, csid: CSID, phase: Phase, n: Int) =>
      complex_target[Seq[WithData[Outcome]]] { tracker =>
        val st = tracker.track(state(c, p))
        val wanted = for {
          info <- tracker.track(subs.prepare_info(c, p, csid, phase)).toSeq
          id <- tracker.track(runs.test_ids_for(c, p, st.tests(phase))).toSeq
        } yield runs.run_one(st.keeps_going(phase), n)(c, p, csid, info.sha, id)
        val done = tracker.track_all(wanted)
        tracker.run_if_needed(_ => done)
      }
    }

  // A student's results for `phase`: their graded commit, run on the
  // phase's tests (as Project.student_results). None if there's nothing to
  // grade.
  lazy val student_results
      : (String, String, CSID, Phase, Int) => Target[Option[StudentResults]] =
    fun { (c: String, p: String, csid: CSID, phase: Phase, n: Int) =>
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
  lazy val phase_results: (String, String, Phase, Int) => Target[
    SortedMap[Alias, RedactedStudentResults]
  ] = fun { (c: String, p: String, phase: Phase, n: Int) =>
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
  lazy val results_files: (String, String, Int) => Target[
    SortedMap[String, SortedMap[Alias, RedactedStudentResults]]
  ] = fun { (c: String, p: String, n: Int) =>
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
  // isn't in them, they don't cover the phase's tests, or a test has tries
  // to go. Missing or unreadable results are work too.
  lazy val has_work: (String, String, Phase, Int) => Target[Boolean] = fun {
    (c: String, p: String, phase: Phase, n: Int) =>
      complex_target[Boolean] { tracker =>
        val st = tracker.track(state(c, p))
        val keep_going = st.keeps_going(phase)
        val students = tracker.track(subs.students_with_submission(c, p)).toSeq
        val published = tracker.track(subs.results(c, p, phase))
        val ids =
          tracker
            .track(runs.test_ids_for(c, p, st.tests(phase)))
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

  // ---- report pages ----------------------------------------------------

  // What a project's report pages show (as HtmlGen.page): its published
  // results, from the mirror, and its settings
  lazy val report_inputs: (String, String) => Target[ReportInputs] = fun {
    (c: String, p: String) =>
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
          projects = defs.active_projects(c)
        )
      ) { (_, deps) =>
        ReportInputs(
          title = s"${c}_$p",
          project_name = p,
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
          tabs = HtmlGen.tabs(c, p, deps.projects)
        )
      }
  }

  // every active project's
  lazy val site_inputs: String => Target[Seq[ReportInputs]] = fun {
    (c: String) =>
      complex_target[Seq[ReportInputs]] { tracker =>
        val projects = tracker.track(defs.active_projects(c)).toSeq
        val inputs = tracker.track_all(projects.map(report_inputs(c, _)))
        tracker.run_if_needed(_ => inputs)
      }
  }

  // ---- scores -----------------------------------------------------------

  // The phase 2 tests a student's graded commit for `phase` passed at least
  // once in `n` tries, every test run to the end (as
  // Project.compute_student_scores)
  lazy val student_scores
      : (String, String, CSID, Phase, Int) => Target[SortedSet[TestId]] =
    fun { (c: String, p: String, csid: CSID, phase: Phase, n: Int) =>
      complex_target[SortedSet[TestId]] { tracker =>
        val wanted = for {
          info <- tracker.track(subs.prepare_info(c, p, csid, phase)).toSeq
          id <- tracker.track(runs.phase2_test_ids(c, p)).toSeq
        } yield runs.run_one(true, n)(c, p, csid, info.sha, id)
        val outs = tracker.track_all(wanted)
        tracker.run_if_needed { _ =>
          (for {
            o <- outs
            if o.value.outcomes.exists(_._1 == OutcomeStatus.pass)
          } yield o.value.test_id).to(SortedSet)
        }
      }
    }

  lazy val scores: (String, String, Phase, Int) => Target[
    SortedMap[CSID, SortedSet[TestId]]
  ] =
    fun { (c: String, p: String, phase: Phase, n: Int) =>
      complex_target[SortedMap[CSID, SortedSet[TestId]]] { tracker =>
        val csids = tracker.track(subs.students_with_submission(c, p)).toSeq
        val passed =
          tracker.track_all(csids.map(student_scores(c, p, _, phase, n)))
        tracker.run_if_needed(_ => csids.zip(passed).to(SortedMap))
      }
    }
}
