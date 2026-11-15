package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CourseName, Create, Publish}
import ag.rules.{Source, Target, WithData}

// A course's grades repo: the staff's workspace for grading data (as
// r2's Course.create_grades_repo). The grader makes it, with a README, and
// otherwise leaves it to them; only the staff read and write it.
final case class GradesRepo(c: CourseName, source: Source)
    extends HandedOverRepo[GradesRepo] derives ToRelPath {
  def repo_name: String = GradesRepo.name_of(c)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.staff(c)), Seq(Groups.staff(c))))
  val message = "creating grades repo"

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

object GradesRepo {
  def name_of(c: CourseName): String = s"${c.value}__grades"
}
