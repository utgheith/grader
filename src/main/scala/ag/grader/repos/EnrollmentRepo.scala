package ag.grader.repos

import ag.grader.targets.{Course, Dropbox}

import ag.grader.name.CourseName

import ag.grader.gitolite.{Groups, OwnedRepo, Create, Publish}

import ag.common.{ToRelPath, given_ReadWriter_SortedMap}

import ag.rules.{Source, Target, WithData}

// A course's enrollment: its students, as the dropbox has their keys (as
// Course.publish_enrollment), for the staff to read. Left as it is if the
// dropbox has no directory for the course: that would unenroll everyone.
final case class EnrollmentRepo(course: Course, dropbox: Dropbox)
    extends OwnedRepo[EnrollmentRepo] derives ToRelPath {
  val source: Source = course.source
  private def c: CourseName = course.c
  def repo_name: String = EnrollmentRepo.name_of(c)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.staff(c)), Seq()))
  val message = "enrolling students"
  override protected def replaces_all: Boolean = false

  lazy val latest: Target[WithData[Publish]] =
    owned(_.track(dropbox.course_keys(c))) { (keys, producer, dir) =>
      keys match {
        case Some(keys) =>
          os.write.over(
            dir / "enrollment.json",
            upickle.default.write(keys, indent = 2)
          )
        case None =>
          producer.log(s"no dropbox directory for $c: enrollment left alone\n")
      }
      Seq()
    }
}

object EnrollmentRepo {
  def name_of(c: CourseName): String = s"${c.value}__enrollment"
}
