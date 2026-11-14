package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{Alias, CSID, CourseName, Create, ProjectName, Publish}
import ag.r3.{Producer, Target, WithData}

import scala.collection.SortedMap

// Every test of a project, for everyone to read (as Project.publish_tests):
// the students' (by alias, at <alias>.<ext>) and the override repo's.
final case class TestsRepo(c: CourseName, p: ProjectName, env: Env)
    extends OwnedRepo[TestsRepo] derives ToRelPath {
  def repo_name: String = s"${ProjectRepo.name_of(c, p)}__tests"
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq()))
  val message = "tests"

  lazy val latest: Target[WithData[Publish]] =
    owned(
      _.track_all(
        (
          tests = env.subs.student_tests_by_csid(c, p),
          aliases = env.runs.aliases(c, p),
          ext = ProjectConfig(c, p, env).test_extensions,
          names = env.runs.override_test_names(c, p),
          ov = OverrideRepo(c, p, env).inspect
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
