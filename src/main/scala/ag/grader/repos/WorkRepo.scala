package ag.grader.repos

import ag.grader.targets.{Project, Submission}

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.gitolite.{CreatedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.grader.Notice
import ag.rules.{Source, Target, WithData}

// A student's branch for a project, in their own repo (see StudentRepo):
// it starts as the project's repo, then it's theirs, never touched. When
// it's made, they're told, with the project's README, as the course says.
final case class WorkRepo(submission: Submission) extends CreatedRepo[WorkRepo]
    derives ToRelPath {
  val source: Source = submission.source
  private def project: Project = submission.project
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  private def csid: CSID = submission.csid
  def repo_name: String = StudentRepo.name_of(c, csid)
  override def branch: String = StudentRepo.work_branch(p)
  def create: Option[Create] = Some(StudentRepo.create(c, csid))
  override def start_from: Option[(String, String)] =
    Some((ProjectRepo.name_of(c, p), ag.grader.gitolite.Repo.master))
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
        Notice.routed(
          n,
          csid,
          Notice.What.RepoCreated(repo_name, branch, readme)
        )
      }
    }
}

object WorkRepo {
  // where a student's work for a project is: (repo, branch)
  def place_of(c: CourseName, p: ProjectName, csid: CSID): (String, String) =
    (StudentRepo.name_of(c, csid), StudentRepo.work_branch(p))
}
