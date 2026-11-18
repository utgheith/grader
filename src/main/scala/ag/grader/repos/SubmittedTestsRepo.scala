package ag.grader.repos

import ag.grader.targets.Project

import ag.grader.name.{CourseName, ProjectName}

import ag.grader.gitolite.{OwnedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.rules.{Source, Target, WithData}

// The students' own tests, by alias, for the staff (as
// Project.publish_submitted_tests), on the grader's submitted_tests branch
// of the course's repo (see ClassRepo)
final case class SubmittedTestsRepo(project: Project)
    extends OwnedRepo[SubmittedTestsRepo] derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  def repo_name: String = ClassRepo.name_of(c)
  override def branch: String = ClassRepo.submitted_tests_branch(p)
  def create: Option[Create] = Some(ClassRepo.create(c))
  val message = "tests"

  lazy val latest: Target[WithData[Publish]] =
    owned { t =>
      // as of the milestone the students are working toward
      val milestone = project.milestone(t.track(project.state).current)
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
