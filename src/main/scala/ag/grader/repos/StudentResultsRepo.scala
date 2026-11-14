package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CSID, CourseName, Create, Phase, ProjectName, Publish}
import ag.r3.{Target, WithData}

// A student's latest results for a project, with each test's output, in
// their own results repo (as Project.publish_student_results): the
// student's and the staff's to read.
final case class StudentResultsRepo(
    c: CourseName,
    p: ProjectName,
    csid: CSID,
    env: Env
) extends OwnedRepo[StudentResultsRepo] derives ToRelPath {
  def repo_name: String = StudentResultsRepo.name_of(c, p, csid)
  def create: Option[Create] =
    Some(Create("empty", Seq(csid.value, Groups.staff(c)), Seq()))
  val message = "updated results"

  // graded to `n` tries
  lazy val latest: Int => Target[WithData[Publish]] = fun { (n: Int) =>
    owned(
      _.track_all(
        (
          results = env.grading.student_results(c, p, csid, Phase.Latest, n),
          outcomes = env.grading.outcomes(c, p, csid, Phase.Latest, n)
        )
      )
    ) { (in, producer, dir) =>
      for {
        o <- in.outcomes
        ext <- StudentResultsRepo.result_files
      } {
        val name = s"${o.value.test_id.external_name}.$ext"
        val _ = producer.copy_file(o.data, os.RelPath(name), dir / name)
      }
      for (r <- in.results)
        os.write.over(dir / "the_results.json", upickle.default.write(r))
      Seq()
    }
  }
}

object StudentResultsRepo {
  def name_of(c: CourseName, p: ProjectName, csid: CSID): String =
    s"${c.value}_${p.value}_${csid.value}_results"

  // the files a run leaves that are published with the results, by
  // extension (as Project.copy_results)
  private val result_files =
    Seq("ok", "out", "err", "result", "time", "stdout", "stderr")
}
