package ag.grader.repos

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.gitolite.{Create, Groups}

// A student's own repo in a course, <course>_<csid>: one for all their
// projects, each on branches of it. A project's branch (its name) is the
// student's to push: their work (WorkRepo). The grader's are under
// grader/<project>/: the results (StudentResultsRepo) and the prepared
// code (PreparedRepo); the student reads them, and the server's rules keep
// them from pushing them. Other branches are the student's own, and the
// grader ignores them. Made (forked from `empty`) for whichever of its
// branches is published first; the student writes it, the staff read it.
object StudentRepo {
  def name_of(c: CourseName, csid: CSID): String = s"${c.value}_${csid.value}"

  def create(c: CourseName, csid: CSID): Create =
    Create("empty", Seq(csid.value, Groups.staff(c)), Seq(csid.value))

  // the student's branch for project `p`
  def work_branch(p: ProjectName): String = p.value

  // the grader's branches for project `p`
  private def grader_branch(p: ProjectName, what: String): String =
    s"grader/${p.value}/$what"
  def results_branch(p: ProjectName): String = grader_branch(p, "results")
  def prepared_branch(p: ProjectName): String = grader_branch(p, "prepared")
}
