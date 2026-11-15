package ag.grader.repos

import ag.grader.{CSID, CourseName, ProjectName, Publish}
import ag.rules.{Source, Target, WithData}

// What publishing a project means, in the order to sync it (a repo's
// contents can depend on another's being there first)
object Grade {
  // what a project publishes, whether or not it's graded: the override
  // repo first, since the tests come from it, then the aliases, which name
  // the students in the rest
  def project(
      c: CourseName,
      p: ProjectName,
      source: Source
  ): Seq[Target[WithData[Publish]]] =
    Seq(
      OverrideRepo(c, p, source).latest,
      AliasesRepo(c, p, source).latest,
      SubmittedTestsRepo(c, p, source).latest,
      TestsRepo(c, p, source).latest
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
      source: Source
  ): Seq[Target[WithData[Publish]]] =
    project(c, p, source) ++
      (ResultsRepo(c, p, source).latest(n) +: own(c, p, students, n, source))

  // the same, as far as everyone has run, with `students` (some of those
  // with a submission) for the own repos: no test runs again (n = 0; see
  // Submission.run_one), so students and tests run to different counts can
  // share the results repo
  def so_far(
      c: CourseName,
      p: ProjectName,
      students: Iterable[CSID],
      source: Source
  ): Seq[Target[WithData[Publish]]] =
    all(c, p, students, 0, source)

  // each student's own results and prepared code
  private def own(
      c: CourseName,
      p: ProjectName,
      students: Iterable[CSID],
      n: Int,
      source: Source
  ): Seq[Target[WithData[Publish]]] =
    students.toSeq.flatMap(csid =>
      Seq(
        StudentResultsRepo(c, p, csid, source).latest(n),
        PreparedRepo(c, p, csid, source).latest
      )
    )
}
