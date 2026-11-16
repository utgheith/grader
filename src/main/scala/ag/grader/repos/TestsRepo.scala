package ag.grader.repos

import ag.grader.targets.Project

import ag.grader.name.{Alias, CSID, CourseName, ProjectName}

import ag.grader.gitolite.{Groups, OwnedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.rules.{Producer, Source, Target, WithData}

import scala.collection.SortedMap

// Every test of a project, for everyone to read (as Project.publish_tests):
// the students' (by alias, at <alias>.<ext>) and the override repo's.
final case class TestsRepo(project: Project) extends OwnedRepo[TestsRepo]
    derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  def repo_name: String = s"${ProjectRepo.name_of(c, p)}__tests"
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq()))
  val message = "tests"

  lazy val latest: Target[WithData[Publish]] =
    owned { t =>
      // as of the milestone the students are working toward
      val milestone = project.milestone(t.track(project.state).current)
      t.track_all(
        (
          tests = milestone.student_tests,
          aliases = project.aliases,
          ext = project.test_extensions,
          names = project.override_test_names,
          ov = OverrideRepo(project).inspect
        )
      )
    } { (in, producer, dir) =>
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
