package ag.grader.repos

import ag.grader.targets.Project

import ag.grader.name.CSID

import ag.grader.gitolite.Publish

import ag.rules.{Target, WithData}

// What publishing a project means, in the order to sync it (a repo's
// contents can depend on another's being there first)
object Grade {
  // what a project publishes, whether or not it's graded: the override
  // repo first, since the tests come from it, then the aliases, which name
  // the students in the rest
  def project(project: Project): Seq[Target[WithData[Publish]]] =
    Seq(
      OverrideRepo(project).latest,
      AliasesRepo(project).latest,
      SubmittedTestsRepo(project).latest,
      TestsRepo(project).latest
    )

  // grading a project to `n` tries, for `students` (those with a
  // submission): what it publishes (so a new student's alias and tests go
  // out with their first results), its course's site (with its results, as
  // far as everyone has run), and each student's own results and prepared
  // code (what the tests ran on)
  def all(
      project: Project,
      students: Iterable[CSID],
      n: Int
  ): Seq[Target[WithData[Publish]]] =
    Grade.project(project) ++
      (SiteRepo(project.course).latest +: own(project, students, n))

  // the same, as far as everyone has run, with `students` (some of those
  // with a submission) for the own repos: no test runs again (n = 0; see
  // SubmissionMilestone.run_one), so students and tests run to different
  // counts can share the site
  def so_far(
      project: Project,
      students: Iterable[CSID]
  ): Seq[Target[WithData[Publish]]] =
    all(project, students, 0)

  // each student's own results and prepared code
  private def own(
      project: Project,
      students: Iterable[CSID],
      n: Int
  ): Seq[Target[WithData[Publish]]] =
    students.toSeq
      .map(project.submission)
      .flatMap(submission =>
        Seq(
          StudentResultsRepo(submission).latest(n),
          PreparedRepo(submission).latest
        )
      )
}
