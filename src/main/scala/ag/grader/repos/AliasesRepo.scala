package ag.grader.repos

import ag.common.{ToRelPath, given_ReadWriter_SortedMap}
import ag.grader.{Alias, CSID, CourseName, Create, ProjectName, Publish}
import ag.r3.{Target, WithData}

import scala.collection.SortedMap

// A project's aliases, which name the students in what's published (as
// Project.publish_aliases): the staff's to read. Added to as students
// submit; an alias, once published, never changes or goes, since what's
// published is keyed by it.
final case class AliasesRepo(c: CourseName, p: ProjectName, env: Env)
    extends AdditiveRepo[AliasesRepo] derives ToRelPath {
  def repo_name: String = AliasesRepo.name_of(c, p)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.staff(c)), Seq()))
  val message = "more aliases"

  lazy val latest: Target[WithData[Publish]] =
    additive(_.track(env.runs.aliases(c, p))) { (aliases, _, dir) =>
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
