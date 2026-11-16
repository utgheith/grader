package ag.grader.repos

import ag.grader.targets.{Project, Submission}

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.gitolite.{Groups, OwnedRepo, Create, Publish}

import ag.common.ToRelPath

import ag.rules.{Source, Target, WithData}

// A student's latest results for a project, with each test's output, in
// their own results repo (as Project.publish_student_results): the
// student's and the staff's to read.
final case class StudentResultsRepo(submission: Submission)
    extends OwnedRepo[StudentResultsRepo] derives ToRelPath {
  val source: Source = submission.source
  private def project: Project = submission.project
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  private def csid: CSID = submission.csid
  def repo_name: String = StudentResultsRepo.name_of(c, p, csid)
  def create: Option[Create] =
    Some(Create("empty", Seq(csid.value, Groups.staff(c)), Seq()))
  val message = "updated results"

  // for the milestone the students are working toward (see
  // ProjectState.current), graded to `n` tries
  lazy val latest: Int => Target[WithData[Publish]] = fun { (n: Int) =>
    owned { t =>
      val milestone = submission.milestone(t.track(project.state).current)
      t.track_all(
        (
          results = milestone.results(n),
          outcomes = milestone.outcomes(n)
        )
      )
    } { (in, producer, dir) =>
      for {
        o <- in.outcomes
        ext <- StudentResultsRepo.result_files
      } {
        val name = s"${o.value.test_id.external_name}.$ext"
        val _ = producer.copy_file(o.data, os.RelPath(name), dir / name)
      }
      for (r <- in.results)
        os.write.over(dir / "the_results.json", upickle.default.write(r))
      Seq()
    }
  }
}

object StudentResultsRepo {
  def name_of(c: CourseName, p: ProjectName, csid: CSID): String =
    s"${c.value}_${p.value}_${csid.value}_results"

  // the files a run leaves that are published with the results, by
  // extension (as Project.copy_results)
  private val result_files =
    Seq("ok", "out", "err", "result", "time", "stdout", "stderr")
}
