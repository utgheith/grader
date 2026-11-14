package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CourseName, ProjectName}
import ag.r3.Target

// A project's repo, which the staff keep: the starting point of the
// students' work repos. The grader only reads it.
final case class ProjectRepo(c: CourseName, p: ProjectName, env: Env)
    extends ReadOnlyRepo[ProjectRepo] derives ToRelPath {
  def repo_name: String = ProjectRepo.name_of(c, p)

  // whether it's on the mirror, with a master
  lazy val has_master: Target[Boolean] =
    target((m = inspect))((_, deps) => deps.m.isDefined)
}

object ProjectRepo {
  def name_of(c: CourseName, p: ProjectName): String = s"${c.value}_${p.value}"
}
