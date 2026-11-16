package ag.grader.repos

import ag.grader.targets.{Project, Submission}

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.gitolite.{Groups, CreatedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.grader.Notice
import ag.rules.{Source, Target, WithData}

// A student's repo for a project (as Project.publish_work_repo): forked
// from the project's repo, then theirs, never touched. The student and the
// staff read it, the student writes it. When it's made, they're told, with
// the project's README, as the course says.
final case class WorkRepo(submission: Submission) extends CreatedRepo[WorkRepo]
    derives ToRelPath {
  val source: Source = submission.source
  private def project: Project = submission.project
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  private def csid: CSID = submission.csid
  def repo_name: String = WorkRepo.name_of(c, p, csid)
  def create: Option[Create] =
    Some(
      Create(
        ProjectRepo.name_of(c, p),
        Seq(csid.value, Groups.staff(c)),
        Seq(csid.value)
      )
    )
  val message = "create"

  lazy val latest: Target[WithData[Publish]] =
    created(_.track(project.course.notifications)) { (n, producer, base) =>
      if (!n.repo_create) Seq()
      else {
        val readme = base
          .flatMap(b =>
            producer.read_text(producer.commit(b).tree, os.rel / "README")
          )
          .getOrElse("")
        Notice.routed(n, csid, Notice.What.RepoCreated(repo_name, readme))
      }
    }
}

object WorkRepo {
  def name_of(c: CourseName, p: ProjectName, csid: CSID): String =
    s"${ProjectRepo.name_of(c, p)}_${csid.value}"
}
