package ag.grader

import ag.rules.{Scope, Target, WithData}

// The project's tests, and running them on the students' submissions, as r3
// targets on the local mirror: the r3 form of the tests and run_one in
// Project. Nothing is published from here: the tests and aliases are worked
// out from what's on the mirror (the outbox is to publish them).
//
// A test runs in a scratch directory of its own, a fresh copy of the
// prepared submission with the test's files added, so runs never share a
// directory.
final class TestRuns(val subs: Submissions) extends Scope("test_runs") {
  private val env = subs.env

  // ---- running them -----------------------------------------------------

  // no dependencies: it runs every time
  lazy val docker_image_of: String => Target[String] = fun { (f: String) =>
    target(EmptyTuple)((_, _) => Docker.image_name(os.pwd / os.RelPath(f)))
  }

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
  ): (CourseName, ProjectName, CSID, Phase, TestId) => Target[
    WithData[Outcome]
  ] =
    fun {
      (c: CourseName, p: ProjectName, csid: CSID, phase: Phase, id: TestId) =>
        complex_target[WithData[Outcome]] { tracker =>
          val deps = tracker.track_all(
            (
              test = Project(c, p, env).test_info(id),
              info = subs.prepare_info(c, p, csid, phase),
              prepared = repos.PreparedRepo(c, p, csid, env).prepared(phase),
              cores = Project(c, p, env).cores,
              docker_file = Project(c, p, env).docker_file,
              image = Project(c, p, env).docker_image
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
