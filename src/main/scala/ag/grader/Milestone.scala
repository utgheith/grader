package ag.grader

import ag.common.ToRelPath
import upickle.default.ReadWriter

// What a submission is graded for: one of the project's milestones, Named
// (see RawMilestone), at the last commit on master by its deadline (with the
// student's extension), unless its commit id file (commit_id_<name>) names
// another; or Live, which is the work pushed after the final one: the head
// of master, late from the final deadline. While a named milestone is open
// its commit is the head of master too, so it's the students' feedback
// until its deadline.
enum Milestone derives ReadWriter:
  case Named(name: String)
  case Live

  // in file names, paths and URLs: the milestone's name, or "live"
  def slug: String = this match
    case Named(name) => name
    case Live        => Milestone.live

  // the file in a submission that can name the commit to grade, if any
  def commit_id_file: Option[String] = this match
    case Named(name) => Some(s"commit_id_$name")
    case Live        => None

  // the milestone's results, in the project's results repo
  def results_file: String = s"results_$slug.json"

object Milestone:
  // Live's slug, which no named milestone may have
  val live = "live"

  // by its slug
  def of(slug: String): Milestone = if (slug == live) Live else Named(slug)

  given ToRelPath[Milestone] = milestone => os.RelPath(milestone.slug)
