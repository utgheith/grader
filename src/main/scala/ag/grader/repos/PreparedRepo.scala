package ag.grader.repos

import ag.grader.targets.{Project, Submission}

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.gitolite.{OwnedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.grader.Milestone
import ag.rules.{Source, Target, Tree, WithData}

// A student's prepared submission for a project, as their tests run on it
// (see Submission.prepare): a directory for each milestone that has a commit to
// grade (latest, phase1, phase2), on the grader's prepared branch of their
// own repo (see StudentRepo), for them and the staff to read. The runs run
// on what's here (prepared), so what the student sees is what was tested.
// It's the grader's: it's worked out again over anything pushed to it.
final case class PreparedRepo(submission: Submission)
    extends OwnedRepo[PreparedRepo] derives ToRelPath {
  val source: Source = submission.source
  private def project: Project = submission.project
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  private def csid: CSID = submission.csid
  def repo_name: String = StudentRepo.name_of(c, csid)
  override def branch: String = StudentRepo.prepared_branch(p)
  def create: Option[Create] = Some(StudentRepo.create(c, csid))
  val message = "updated prepared code"

  lazy val latest: Target[WithData[Publish]] = owned { t =>
    val all_milestones = t.track(project.state).all_milestones
    val infos = t.track_all(
      all_milestones.map(submission.milestone(_).prepare_info)
    )
    val graded =
      all_milestones.zip(infos).collect { case (milestone, Some(info)) =>
        (milestone, info.sha)
      }
    val trees =
      t.track_all(
        graded.map((_, sha) => submission.prepare(sha))
      )
    graded.map(_._1).zip(trees.map(_.data))
  } { (in, producer, dir) =>
    for ((milestone, tree) <- in)
      producer.copy_to(tree, dir / ToRelPath(milestone))
    Seq()
  }

  // What a milestone's runs run on: its directory in what's published; None if
  // the milestone has no commit to grade. The same files as the tree it was
  // copied from, so the same id (and the same runs, see ag.grader.targets.TestRun).
  lazy val prepared: Milestone => Target[Option[Tree]] = fun {
    (milestone: Milestone) =>
      target((published = latest)) { (producer, deps) =>
        val data = deps.published.data
        producer.id_at(data, ToRelPath(milestone)).map(Tree(_, data.at))
      }
  }
}
