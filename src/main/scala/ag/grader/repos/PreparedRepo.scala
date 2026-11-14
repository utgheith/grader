package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CSID, CourseName, Create, Phase, ProjectName, Publish}
import ag.r3.{Target, Tree, WithData}

// A student's prepared submission for a project, as their tests run on it
// (see Submissions.prepare): a directory for each phase that has a commit to
// grade (latest, phase1, phase2), in their own repo, for them and the staff
// to read. The runs run on what's here (prepared), so what the student sees
// is what was tested. Staff can push to it, but it's the grader's: it's
// worked out again over what they push.
final case class PreparedRepo(
    c: CourseName,
    p: ProjectName,
    csid: CSID,
    env: Env
) extends OwnedRepo[PreparedRepo] derives ToRelPath {
  def repo_name: String = PreparedRepo.name_of(c, p, csid)
  def create: Option[Create] =
    Some(
      Create(
        "empty",
        readers = Seq(csid.value, Groups.staff(c)),
        writers = Seq(Groups.staff(c))
      )
    )
  val message = "updated prepared code"

  lazy val latest: Target[WithData[Publish]] = owned { t =>
    val infos = t.track_all(
      PreparedRepo.phases.map(env.subs.prepare_info(c, p, csid, _))
    )
    val graded = PreparedRepo.phases.zip(infos).collect {
      case (phase, Some(info)) => (phase, info.sha)
    }
    val trees =
      t.track_all(graded.map((_, sha) => env.subs.prepare(c, p, csid, sha)))
    graded.map(_._1).zip(trees.map(_.data))
  } { (in, producer, dir) =>
    for ((phase, tree) <- in) producer.copy_to(tree, dir / ToRelPath(phase))
    Seq()
  }

  // What a phase's runs run on: its directory in what's published; None if
  // the phase has no commit to grade. The same files as the tree it was
  // copied from, so the same id (and the same runs, see TestRun).
  lazy val prepared: Phase => Target[Option[Tree]] = fun { (phase: Phase) =>
    target((published = latest)) { (producer, deps) =>
      val data = deps.published.data
      producer.id_at(data, ToRelPath(phase)).map(Tree(_, data.at))
    }
  }
}

object PreparedRepo {
  def name_of(c: CourseName, p: ProjectName, csid: CSID): String =
    s"${c.value}_${p.value}_${csid.value}__prepared"

  val phases: Seq[Phase] = Seq(Phase.Latest, Phase.One, Phase.Two)
}
