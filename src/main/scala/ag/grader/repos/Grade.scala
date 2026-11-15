package ag.grader.repos

import ag.grader.{CSID, CourseName, ProjectName, Publish}
import ag.rules.{Target, WithData}

// What publishing a project means, in the order to sync it (a repo's
// contents can depend on another's being there first)
object Grade {
  // what a project publishes, whether or not it's graded: the override
  // repo first, since the tests come from it, then the aliases, which name
  // the students in the rest
  def project(
      c: CourseName,
      p: ProjectName,
      env: Env
  ): Seq[Target[WithData[Publish]]] =
    Seq(
      OverrideRepo(c, p, env).latest,
      AliasesRepo(c, p, env).latest,
      SubmittedTestsRepo(c, p, env).latest,
      TestsRepo(c, p, env).latest
    )

  // grading a project to `n` tries, for `students` (those with a
  // submission): what it publishes (so a new student's alias and tests go
  // out with their first results), its results repo, and each student's
  // own results and prepared code (what the tests ran on)
  def all(
      c: CourseName,
      p: ProjectName,
      students: Iterable[CSID],
      n: Int,
      env: Env
  ): Seq[Target[WithData[Publish]]] =
    project(c, p, env) ++
      (ResultsRepo(c, p, env).latest(n) +: own(c, p, students, n, env))

  // the same, as far as everyone has run, with `students` (some of those
  // with a submission) for the own repos: no test runs again (n = 0; see
  // Submission.run_one), so students and tests run to different counts can
  // share the results repo
  def so_far(
      c: CourseName,
      p: ProjectName,
      students: Iterable[CSID],
      env: Env
  ): Seq[Target[WithData[Publish]]] =
    all(c, p, students, 0, env)

  // each student's own results and prepared code
  private def own(
      c: CourseName,
      p: ProjectName,
      students: Iterable[CSID],
      n: Int,
      env: Env
  ): Seq[Target[WithData[Publish]]] =
    students.toSeq.flatMap(csid =>
      Seq(
        StudentResultsRepo(c, p, csid, env).latest(n),
        PreparedRepo(c, p, csid, env).latest
      )
    )
}
