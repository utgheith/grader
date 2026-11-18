package ag.grader.targets

import ag.grader.Milestone

import ag.grader.name.{Alias, CSID, CourseName, ProjectName}

import ag.grader.json.{StudentResults, RedactedOutcome, RedactedTestId}

import ag.common.{NoNulls, ToRelPath, given_ReadWriter_SortedMap}
import ag.rules.{
  Commit,
  CommitInfo,
  DerivedScope,
  Source,
  Target,
  WithData,
  create_data
}

import java.time.{Instant, OffsetDateTime, ZoneId, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}
import scala.util.Try
import scala.util.matching.Regex

// A student's targets for a project: their submission, its code ready to
// test, and what the grader published for them, read back from the local
// mirror. What depends on a milestone too (the commit graded, their own
// test, running the tests, their results) is SubmissionMilestone's.
//
// Every read is pinned: to the commit a ref snapshot names, or to a commit's
// id. Nothing here clones or keeps a git repo: a commit's files are read
// from the mirror, and copied out (prepare) only where a tool needs them.
final case class Submission(project: Project, csid: CSID)
    extends DerivedScope[Submission] derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  private val report_name = os.rel / "REPORT.txt"

  // its targets for a milestone
  def milestone(m: Milestone): SubmissionMilestone =
    SubmissionMilestone(this, m)

  // ---- the submission, its code ready to test --------------------------

  // where the student's work is, (repo, branch), and its branch: see
  // repos.StudentRepo
  private val (work_repo, work_branch) =
    ag.grader.repos.WorkRepo.place_of(c, p, csid)
  private def work: Target[Option[Commit]] =
    source.branch(work_repo, work_branch)
  private def own_results: Target[Option[Commit]] = {
    val (r, b) = ag.grader.repos.StudentResultsRepo.place_of(c, p, csid)
    source.branch(r, b)
  }

  // The student's branch for the project, if it's there and more than the
  // project's starting code (its files differ from the project repo's).
  lazy val submission: Target[Option[Commit]] =
    target(
      (
        w = work,
        base = source.master(ag.grader.repos.ProjectRepo.name_of(c, p))
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
        base = source.master(ag.grader.repos.ProjectRepo.name_of(c, p))
      )
    ) { (producer, deps) =>
      def report(m: Option[Commit]) =
        m.flatMap(m => producer.id_at(producer.commit(m).tree, report_name))
      report(deps.sub) != report(deps.base)
    }

  // whether the student's branch has a spamon file
  lazy val has_spamon: Target[Boolean] =
    target((w = work)) { (producer, deps) =>
      deps.w.exists(m =>
        producer.id_at(producer.commit(m).tree, os.rel / "spamon").isDefined
      )
    }

  // Commit `sha` of the student's repo, with the course's overrides on top,
  // its tests removed, and prepared_commit saying which commit it is (as
  // Project.prepare). Depends on the commit by id, not on master: pushing
  // more doesn't redo it.
  lazy val prepare: String => Target[WithData[Unit]] = fun { (sha: String) =>
    target(
      (
        ov = ag.grader.repos.OverrideRepo(project).inspect,
        ext = project.test_extensions
      )
    ) { (producer, deps) =>
      create_data(producer) { dir =>
        val commit = producer.commit(
          Commit(
            source.dir(work_repo).toString,
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

  // whether the student's published results are there and this grader can
  // read them
  lazy val published_results_readable: Target[Boolean] =
    target((m = own_results)) { (producer, deps) =>
      Project.readable[StudentResults](
        Project.text_on(producer, deps.m, "the_results.json"),
        "the_results.json"
      )
    }

  // The student's published results, if there are any. Fails if they can't
  // be read, saying what rewrites them (as Project.get_student_results).
  lazy val published_results: Target[Option[StudentResults]] =
    target((m = own_results)) { (producer, deps) =>
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
                s"${work_repo}'s ${ag.grader.repos.StudentRepo.results_branch(p)}'s the_results.json isn't readable by this " +
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
    target((aliases = project.aliases))((_, deps) => deps.aliases.get(csid))
}

object Submission {
  // the time git gives a commit (its committer's), as an instant
  private[grader] def time(ci: CommitInfo): Instant =
    OffsetDateTime.parse(ci.committed).nn.toInstant.nn

  private[grader] def local(t: java.time.LocalDateTime): ZonedDateTime =
    ZonedDateTime.of(t, ZoneId.systemDefault).nn
}
