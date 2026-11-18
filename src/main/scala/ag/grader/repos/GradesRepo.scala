package ag.grader.repos

import ag.grader.targets.Course

import ag.grader.name.CourseName

import ag.grader.gitolite.{HandedOverRepo, Create, Publish}

import ag.common.ToRelPath
import ag.rules.{Source, Target, WithData}

// A course's grades: the staff's workspace for grading data (as r2's
// Course.create_grades_repo), on the staff's grades branch of the course's
// repo (see ClassRepo). The grader makes it, with a README, and otherwise
// leaves it to them.
final case class GradesRepo(course: Course) extends HandedOverRepo[GradesRepo]
    derives ToRelPath {
  val source: Source = course.source
  private def c: CourseName = course.c
  def repo_name: String = ClassRepo.name_of(c)
  override def branch: String = ClassRepo.grades_branch
  def create: Option[Create] = Some(ClassRepo.create(c))
  val message = "creating grades"

  lazy val latest: Target[WithData[Publish]] =
    handed_over(_ => ()) { (_, _, dir) =>
      os.write.over(
        dir / "README.md",
        s"""
           |# Grading data for `$c`
           |
           |This is a per-course workspace for synchronizing grading data
           |used by the grading scripts between TAs. The grader itself
           |automatically creates this repo, but does not otherwise use
           |it; that may change in the future.
           |""".stripMargin
      )
      Seq()
    }
}
