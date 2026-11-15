package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CourseName, Create, Project, ProjectName, Publish}
import ag.rules.{Source, Target, WithData}

// The students' own tests, by alias, for the staff (as
// Project.publish_submitted_tests)
final case class SubmittedTestsRepo(
    c: CourseName,
    p: ProjectName,
    source: Source
) extends OwnedRepo[SubmittedTestsRepo] derives ToRelPath {
  def repo_name: String = SubmittedTestsRepo.name_of(c, p)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.staff(c)), Seq()))
  val message = "tests"

  lazy val latest: Target[WithData[Publish]] =
    owned(
      _.track_all(
        (
          tests = Project(c, p, source).student_tests_by_csid,
          aliases = Project(c, p, source).aliases,
          ext = Project(c, p, source).test_extensions
        )
      )
    ) { (in, producer, dir) =>
      TestsRepo.copy_student_tests(producer, in.tests, in.aliases, in.ext, dir)
      Seq()
    }
}

object SubmittedTestsRepo {
  def name_of(c: CourseName, p: ProjectName): String =
    s"${ProjectRepo.name_of(c, p)}__submitted_tests"
}
