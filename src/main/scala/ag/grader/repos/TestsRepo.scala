package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{
  Alias,
  CSID,
  CourseName,
  Create,
  Project,
  ProjectName,
  Publish
}
import ag.rules.{Producer, Source, Target, WithData}

import scala.collection.SortedMap

// Every test of a project, for everyone to read (as Project.publish_tests):
// the students' (by alias, at <alias>.<ext>) and the override repo's.
final case class TestsRepo(c: CourseName, p: ProjectName, source: Source)
    extends OwnedRepo[TestsRepo] derives ToRelPath {
  def repo_name: String = s"${ProjectRepo.name_of(c, p)}__tests"
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq()))
  val message = "tests"

  lazy val latest: Target[WithData[Publish]] =
    owned(
      _.track_all(
        (
          tests = Project(c, p, source).student_tests_by_csid,
          aliases = Project(c, p, source).aliases,
          ext = Project(c, p, source).test_extensions,
          names = Project(c, p, source).override_test_names,
          ov = OverrideRepo(c, p, source).inspect
        )
      )
    ) { (in, producer, dir) =>
      TestsRepo.copy_student_tests(producer, in.tests, in.aliases, in.ext, dir)
      for {
        o <- in.ov.toSeq
        tree = producer.commit(o).tree
        n <- in.names.toSeq
        e <- in.ext
      } {
        val _ = producer.copy_path(tree, os.RelPath(s"$n.$e"), dir / s"$n.$e")
      }
      Seq()
    }
}

object TestsRepo {
  // the students' tests, by alias, at <alias>.<ext>
  def copy_student_tests(
      producer: Producer[?],
      tests: SortedMap[CSID, WithData[String]],
      aliases: SortedMap[CSID, Alias],
      ext: Iterable[String],
      dir: os.Path
  ): Unit =
    for {
      (csid, t) <- tests.toSeq
      alias <- aliases.get(csid).toSeq
      e <- ext
    } {
      val _ = producer.copy_path(
        t.data,
        os.RelPath(s"${csid.value}.$e"),
        dir / s"${alias.toString}.$e"
      )
    }
}
