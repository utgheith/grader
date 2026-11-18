package ag.grader.repos

import ag.grader.Milestone

import ag.grader.name.{CourseName, ProjectName}

import ag.grader.gitolite.{Create, Groups}

// A course's own repo, <course>: one for everything the grader and the
// staff keep for the course, each on a branch of it (the project repos and
// the students' repos are apart: see ProjectRepo and StudentRepo). Only
// the staff read it; what students see of it is the site, served on the
// web, and the tests and Makefiles in their prepared code.
//
// The grader's branches are under grader/, and the server's rules keep
// the staff from pushing them: the site (SiteRepo, with each project's
// results), the enrollment (EnrollmentRepo), and each project's tests
// (TestsRepo), aliases (AliasesRepo) and submitted tests
// (SubmittedTestsRepo). The staff's are the rest: the grades (GradesRepo)
// and each project's override (OverrideRepo), which the grader starts.
// Made (forked from `empty`) for whichever branch is published first.
object ClassRepo {
  def name_of(c: CourseName): String = c.value

  def create(c: CourseName): Create =
    Create("empty", Seq(Groups.staff(c)), Seq(Groups.staff(c)))

  // the grader's
  val site_branch = "grader/site"
  val enrollment_branch = "grader/enrollment"
  private def grader_branch(p: ProjectName, what: String): String =
    s"grader/${p.value}/$what"
  def tests_branch(p: ProjectName): String = grader_branch(p, "tests")
  def aliases_branch(p: ProjectName): String = grader_branch(p, "aliases")
  def submitted_tests_branch(p: ProjectName): String =
    grader_branch(p, "submitted_tests")

  // the staff's
  val grades_branch = "grades"
  def override_branch(p: ProjectName): String = s"${p.value}/override"

  // where a milestone's results are on the site branch
  def results_path(p: ProjectName, milestone: Milestone): String =
    s"results/${p.value}/${milestone.results_file}"
}
