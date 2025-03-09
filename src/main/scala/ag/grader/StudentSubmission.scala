package ag.grader

import language.experimental.namedTuples

import ag.r2.Scope

case class StudentSubmission(
    project: Project,
    csid: CSID,
    cutoffTime: CutoffTime
) extends Scope(project / csid / cutoffTime) {}
