package ag.grader.targets

import ag.grader.{Milestone, TestSet}

import ag.grader.name.{Alias, CSID, CourseName, ProjectName}

import ag.grader.json.{RedactedStudentResults, OutcomeStatus, TestId}

import ag.common.{NoNulls, ToRelPath, given_ReadWriter_SortedMap}
import ag.grader.repos.{OverrideRepo, ResultsRepo}
import ag.rules.{DerivedScope, Source, Target, Tree, WithData, create_data}

import java.time.{ZoneId, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}
import scala.util.Try

// A project's targets for one of its milestones (see Milestone): its tests
// (the course's, and each student's as of the milestone) and weight, every
// student's results for it, and what the grader published for it, read
// back.
final case class ProjectMilestone(project: Project, milestone: Milestone)
    extends DerivedScope[ProjectMilestone] derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p

  private def submission(csid: CSID): SubmissionMilestone =
    project.submission(csid).milestone(milestone)

  // ---- its settings and tests ----------------------------------------

  // its tests (by name) and weight; Live's are every milestone's (see
  // RawProject.tests)
  lazy val test_names: Target[SortedSet[String]] =
    target((rp = project.raw))((_, d) => d.rp.tests(milestone).to(SortedSet))
  lazy val weight: Target[Int] =
    target((rp = project.raw))((_, d) => d.rp.weight(milestone))

  // The students' own tests, as of the milestone (see
  // SubmissionMilestone.test): each one's commit, and its files
  lazy val student_tests: Target[SortedMap[CSID, WithData[String]]] =
    complex_target[SortedMap[CSID, WithData[String]]] { tracker =>
      val csids = tracker.track(project.students_with_submission).toSeq
      val tests = tracker.track_all(csids.map(submission(_).test))
      tracker.run_if_needed(_ =>
        (for {
          (csid, t) <- csids.zip(tests)
          rev <- t.value.toSeq
        } yield (csid, WithData(rev, t.data))).to(SortedMap)
      )
    }

  lazy val csids_with_tests: Target[SortedSet[CSID]] =
    target((t = student_tests))((_, deps) => deps.t.keySet)

  // Every test, and the tree with its files, named <internal name>.<ext>:
  // each student's own (their alias outside, their CSID inside) and the
  // override repo's (as Project.publish_tests: one of those wins over a
  // student's with the same id)
  lazy val tests: Target[SortedMap[TestId, Tree]] =
    target(
      (
        students = student_tests,
        aliases = project.aliases,
        names = project.override_test_names,
        ov = source.master(OverrideRepo.name_of(c, p))
      )
    ) { (producer, deps) =>
      val theirs = for {
        (csid, t) <- deps.students.toSeq
        alias <- deps.aliases.get(csid).toSeq
      } yield (TestId(alias.toString, csid.value), t.data)
      val ours = for {
        o <- deps.ov.toSeq
        tree = producer.commit(o).tree
        n <- deps.names.toSeq
      } yield (TestId(n, n), tree)
      (theirs ++ ours).to(SortedMap)
    }

  lazy val test_ids: Target[SortedSet[TestId]] =
    target((tests = tests))((_, deps) => deps.tests.keySet)

  // all but the tests marked ignored, by external or internal name
  lazy val good_test_ids: Target[SortedSet[TestId]] =
    target((ids = test_ids, bad = project.bad_tests)) { (_, deps) =>
      deps.ids.filterNot(Project.named(deps.ids, deps.bad).contains)
    }

  // the ones its score counts: its own (Live: every milestone's)
  lazy val counted_test_ids: Target[SortedSet[TestId]] =
    target((ids = test_ids, names = test_names)) { (_, deps) =>
      Project.named(deps.ids, deps.names)
    }

  // `m`'s tests (Live: every milestone's) among these, less the ignored ones
  private lazy val good_of: Milestone => Target[SortedSet[TestId]] = fun {
    (m: Milestone) =>
      target((ids = good_test_ids, rp = project.raw)) { (_, deps) =>
        Project.named(deps.ids, deps.rp.tests(m).to(SortedSet))
      }
  }

  // the tests in `set` (see ProjectState.tests), as of the milestone
  def ids_for(set: TestSet): Target[SortedSet[TestId]] = set match {
    case TestSet.All      => test_ids
    case TestSet.Good     => good_test_ids
    case TestSet.Chosen   => good_of(Milestone.Live)
    case TestSet.Of(name) => good_of(Milestone.Named(name))
  }

  // A test's files, named <external name>.<ext>, as the Makefile runs them,
  // links to other files in the tests followed. Fails if there's no such
  // test.
  lazy val test_info: TestId => Target[WithData[Unit]] = fun { (id: TestId) =>
    target((tests = tests, ext = project.test_extensions)) { (producer, deps) =>
      val tree = deps.tests.getOrElse(
        id,
        throw IllegalArgumentException(
          s"no test ${id.external_name} in ${c}_$p"
        )
      )
      create_data(producer) { dir =>
        for {
          e <- deps.ext
          at <- Project.follow(
            producer,
            tree,
            os.RelPath(s"${id.internal_name}.$e")
          )
        } {
          val _ =
            producer.copy_path(tree, at, dir / s"${id.external_name}.$e")
        }
      }
    }
  }

  // ---- published, read back -----------------------------------------

  lazy val results_readable: Target[Boolean] =
    target((m = source.master(ResultsRepo.name_of(c, p)))) { (producer, deps) =>
      Project.readable[SortedMap[Alias, RedactedStudentResults]](
        Project.text_on(producer, deps.m, milestone.results_file),
        milestone.results_file
      )
    }

  // Its published results, by alias (as Project.results): Right(None) if
  // there are none yet, Left(why) if they can't be read by this version of
  // the grader
  lazy val published_results: Target[
    Either[String, Option[SortedMap[Alias, RedactedStudentResults]]]
  ] =
    target((m = source.master(ResultsRepo.name_of(c, p)))) { (producer, deps) =>
      val out
          : Either[String, Option[SortedMap[Alias, RedactedStudentResults]]] =
        Project.text_on(producer, deps.m, milestone.results_file) match {
          case None       => Right(None)
          case Some(json) =>
            Try(
              NoNulls.check(
                upickle.default
                  .read[SortedMap[Alias, RedactedStudentResults]](json),
                milestone.results_file
              )
            ).toEither.left
              .map(e =>
                s"${ResultsRepo.name_of(c, p)}'s ${milestone.results_file} isn't readable by this version " +
                  s"of the grader; `run grade -c $c -p $p` rewrites it (${String.valueOf(e.getMessage)})"
              )
              .map(Some(_))
        }
      out
    }

  // ---- grading -------------------------------------------------------

  // Every student's results, by alias, as published (as
  // Project.publish_results): late is from each student's own deadline;
  // students without an alias are left out.
  lazy val results: Int => Target[SortedMap[Alias, RedactedStudentResults]] =
    fun { (n: Int) =>
      complex_target[SortedMap[Alias, RedactedStudentResults]] { tracker =>
        val students = tracker.track(project.students_with_submission).toSeq
        val results =
          tracker.track_all(students.map(submission(_).results(n)))
        val deadlines = tracker.track_all(students.map(submission(_).deadline))
        val aliases =
          tracker.track_all(students.map(project.submission(_).alias))
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

  // Whether grading it to `n` tries has tests to run, as far as the
  // published results tell (as Project.has_work): a student's graded commit
  // isn't in them, they don't cover the tests it runs (or have ones it
  // doesn't show), or a test it runs has tries to go. Missing or unreadable
  // results are work too.
  lazy val has_work: Int => Target[Boolean] = fun { (n: Int) =>
    complex_target[Boolean] { tracker =>
      val st = tracker.track(project.state)
      val keep_going = st.keeps_going(milestone)
      val students = tracker.track(project.students_with_submission).toSeq
      val published = tracker.track(published_results)
      val ids =
        tracker.track(ids_for(st.tests(milestone))).map(_.redacted)
      // the tests that may be shown besides, with the runs they have:
      // never work
      val shown =
        tracker.track(ids_for(st.shows(milestone))).map(_.redacted)
      val infos = tracker.track_all(students.map(submission(_).prepare_info))
      val aliases =
        tracker.track_all(students.map(project.submission(_).alias))
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

  // each student's tests passed (see SubmissionMilestone.scores)
  lazy val scores: Int => Target[SortedMap[CSID, SortedSet[TestId]]] =
    fun { (n: Int) =>
      complex_target[SortedMap[CSID, SortedSet[TestId]]] { tracker =>
        val csids = tracker.track(project.students_with_submission).toSeq
        val passed = tracker.track_all(csids.map(submission(_).scores(n)))
        tracker.run_if_needed(_ => csids.zip(passed).to(SortedMap))
      }
    }
}
