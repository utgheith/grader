package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{
  CSID,
  Course,
  CourseName,
  Create,
  Notice,
  ProjectName,
  Publish
}
import ag.rules.{Target, WithData}

// A student's repo for a project (as Project.publish_work_repo): forked
// from the project's repo, then theirs, never touched. The student and the
// staff read it, the student writes it. When it's made, they're told, with
// the project's README, as the course says.
final case class WorkRepo(c: CourseName, p: ProjectName, csid: CSID, env: Env)
    extends CreatedRepo[WorkRepo] derives ToRelPath {
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
    created(_.track(Course(c, env).notifications)) { (n, producer, base) =>
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
