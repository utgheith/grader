package ag.grader.repos

import ag.grader.targets.Project

import ag.grader.name.{CourseName, ProjectName}

import ag.grader.gitolite.ReadOnlyRepo

import ag.common.ToRelPath
import ag.rules.{Source, Target}

// A project's repo, which the staff keep: the starting point of the
// students' work repos. The grader only reads it.
final case class ProjectRepo(project: Project) extends ReadOnlyRepo[ProjectRepo]
    derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  def repo_name: String = ProjectRepo.name_of(c, p)

  // whether it's on the mirror, with a master
  lazy val has_master: Target[Boolean] =
    target((m = inspect))((_, deps) => deps.m.isDefined)
}

object ProjectRepo {
  def name_of(c: CourseName, p: ProjectName): String = s"${c.value}_${p.value}"
}
