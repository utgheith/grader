package ag.grader

import ag.rules.{Scope, Target, WithData}

import scala.collection.{SortedMap, SortedSet}

// Grading: the students' results and scores, as r3 targets on the local
// mirror. The r3 form of student_results, has_work and compute_scores in
// r2's Project. What's published from them is in repos (see repos.Grade).
//
// Which tests a phase runs, and whether a test keeps running after it fails,
// come from the project's state (as r2's publish_results picked
// them); `n` is how many tries each test gets (see TestRuns.run_one).
final class Grading(val runs: TestRuns) extends Scope("grading") {
  private val subs = runs.subs
  private val env = subs.env

  lazy val alias: (CourseName, ProjectName, CSID) => Target[Option[Alias]] =
    fun { (c: CourseName, p: ProjectName, csid: CSID) =>
      target((aliases = Project(c, p, env).aliases))((_, deps) =>
        deps.aliases.get(csid)
      )
    }

  lazy val has_test: (CourseName, ProjectName, CSID) => Target[Boolean] = fun {
    (c: CourseName, p: ProjectName, csid: CSID) =>
      target((with_tests = Project(c, p, env).csids_with_tests))((_, deps) =>
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
        val st = tracker.track(Project(c, p, env).state)
        val keep_going = st.keeps_going(phase)
        val run =
          tracker.track(Project(c, p, env).test_ids_for(st.tests(phase)))
        val others = tracker
          .track(Project(c, p, env).test_ids_for(st.shows(phase)))
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

  // ---- report pages ----------------------------------------------------

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
          id <- tracker.track(Project(c, p, env).phase2_test_ids).toSeq
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

}
