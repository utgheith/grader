package ag.grader.repos

import ag.grader.targets.Project

import ag.grader.name.{Alias, CSID, CourseName, ProjectName}

import ag.grader.gitolite.{Groups, AdditiveRepo, Create, Publish}

import ag.common.{ToRelPath, given_ReadWriter_SortedMap}
import ag.rules.{Source, Target, WithData}

import scala.collection.SortedMap

// A project's aliases, which name the students in what's published (as
// Project.publish_aliases): the staff's to read. Added to as students
// submit; an alias, once published, never changes or goes, since what's
// published is keyed by it.
final case class AliasesRepo(project: Project) extends AdditiveRepo[AliasesRepo]
    derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  def repo_name: String = AliasesRepo.name_of(c, p)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.staff(c)), Seq()))
  val message = "more aliases"

  lazy val latest: Target[WithData[Publish]] =
    additive(_.track(project.aliases)) { (aliases, _, dir) =>
      val f = dir / "aliases.json"
      val before =
        if (os.isFile(f))
          upickle.default.read[SortedMap[CSID, Alias]](os.read(f))
        else SortedMap[CSID, Alias]()
      val changed = before.filter((csid, a) => !aliases.get(csid).contains(a))
      if (changed.nonEmpty)
        throw IllegalStateException(
          s"$repo_name: ${changed.size.toString} published aliases would change or go"
        )
      os.write.over(f, upickle.default.write(aliases, indent = 2))
      Seq()
    }
}

object AliasesRepo {
  def name_of(c: CourseName, p: ProjectName): String =
    s"${ProjectRepo.name_of(c, p)}__aliases"
}
