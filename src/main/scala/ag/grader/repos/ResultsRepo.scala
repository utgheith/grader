package ag.grader.repos

import ag.grader.targets.Project

import ag.grader.name.{CourseName, ProjectName}

import ag.grader.gitolite.{Groups, OwnedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.rules.{Source, Target, WithData}

// Every student's results, by alias, for each milestone the project publishes
// (Milestone.results_file), for the report (as Project.publish_results): for
// everyone to read. A milestone's file stays once it's no longer published.
final case class ResultsRepo(project: Project) extends OwnedRepo[ResultsRepo]
    derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  def repo_name: String = ResultsRepo.name_of(c, p)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq()))
  val message = "update results"
  override protected def replaces_all: Boolean = false

  // graded to `n` tries (0: as far as each student has run; see
  // Submission.run_one)
  lazy val latest: Int => Target[WithData[Publish]] = fun { (n: Int) =>
    owned(_.track(project.results_files(n))) { (files, _, dir) =>
      for ((name, data) <- files)
        os.write.over(dir / name, upickle.default.write(data, indent = 2))
      Seq()
    }
  }
}

object ResultsRepo {
  def name_of(c: CourseName, p: ProjectName): String =
    s"${ProjectRepo.name_of(c, p)}__results"
}
