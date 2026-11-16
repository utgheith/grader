package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{
  ProjectMilestone,
  Course,
  CourseName,
  Create,
  Project,
  ProjectName,
  Publish
}
import ag.rules.{Source, Target, WithData}

// The students' own tests, by alias, for the staff (as
// Project.publish_submitted_tests)
final case class SubmittedTestsRepo(
    c: CourseName,
    p: ProjectName,
    source: Source
) extends OwnedRepo[SubmittedTestsRepo] derives ToRelPath {
  private def project = Project(Course(c, source), p)
  def repo_name: String = SubmittedTestsRepo.name_of(c, p)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.staff(c)), Seq()))
  val message = "tests"

  lazy val latest: Target[WithData[Publish]] =
    owned { t =>
      // as of the milestone the students are working toward
      val milestone =
        ProjectMilestone(project, t.track(project.state).current)
      t.track_all(
        (
          tests = milestone.student_tests,
          aliases = project.aliases,
          ext = project.test_extensions
        )
      )
    } { (in, producer, dir) =>
      TestsRepo.copy_student_tests(producer, in.tests, in.aliases, in.ext, dir)
      Seq()
    }
}

object SubmittedTestsRepo {
  def name_of(c: CourseName, p: ProjectName): String =
    s"${ProjectRepo.name_of(c, p)}__submitted_tests"
}
