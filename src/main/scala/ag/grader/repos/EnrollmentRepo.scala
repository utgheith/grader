package ag.grader.repos

import ag.grader.targets.{Course, Dropbox}

import ag.grader.name.CourseName

import ag.grader.gitolite.{OwnedRepo, Create, Publish}

import ag.common.{ToRelPath, given_ReadWriter_SortedMap}

import ag.rules.{Source, Target, WithData}

// A course's enrollment: its students, as the dropbox has their keys (as
// Course.publish_enrollment), on the grader's enrollment branch of the
// course's repo (see ClassRepo), for the staff. Left as it is if the
// dropbox has no directory for the course: that would unenroll everyone.
final case class EnrollmentRepo(course: Course, dropbox: Dropbox)
    extends OwnedRepo[EnrollmentRepo] derives ToRelPath {
  val source: Source = course.source
  private def c: CourseName = course.c
  def repo_name: String = ClassRepo.name_of(c)
  override def branch: String = ClassRepo.enrollment_branch
  def create: Option[Create] = Some(ClassRepo.create(c))
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
