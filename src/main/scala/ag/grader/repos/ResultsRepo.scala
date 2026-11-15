package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CourseName, Create, Project, ProjectName, Publish}
import ag.rules.{Target, WithData}

// Every student's results, by alias, for each phase the project publishes
// (Phase.results_file), for the report (as Project.publish_results): for
// everyone to read. A phase's file stays once it's no longer published.
final case class ResultsRepo(c: CourseName, p: ProjectName, env: Env)
    extends OwnedRepo[ResultsRepo] derives ToRelPath {
  def repo_name: String = ResultsRepo.name_of(c, p)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq()))
  val message = "update results"
  override protected def replaces_all: Boolean = false

  // graded to `n` tries (0: as far as each student has run; see
  // TestRuns.run_one)
  lazy val latest: Int => Target[WithData[Publish]] = fun { (n: Int) =>
    owned(_.track(Project(c, p, env).results_files(n))) { (files, _, dir) =>
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
