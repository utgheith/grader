package ag.grader

import ag.common.ToRelPath
import upickle.default.ReadWriter

import java.time.LocalDateTime

// The commit a submission is graded at: the latest on master, or -- for
// results as of a phase, once its deadline has passed -- the last one on
// master before that phase's deadline, unless the phase's commit id file
// (commit_id_<n>) names another.
enum Phase derives ReadWriter:
  case Latest, One, Two

  // the file in a submission that can name the commit to grade, if any
  def commit_id_file: Option[String] = this match
    case Latest => None
    case One    => Some("commit_id_1")
    case Two    => Some("commit_id_2")

  // the phase's results, in the project's results repo
  def results_file: String = this match
    case Latest => "results.json"
    case One    => "results_phase1.json"
    case Two    => "results_phase2.json"

  // the commits graded are those before this, if anything
  def deadline(test_cutoff: LocalDateTime, code_cutoff: LocalDateTime) =
    this match
      case Latest => None
      case One    => Some(test_cutoff)
      case Two    => Some(code_cutoff)

object Phase:
  given ToRelPath[Phase] = {
    case Phase.Latest => os.RelPath("latest")
    case Phase.One    => os.RelPath("phase1")
    case Phase.Two    => os.RelPath("phase2")
  }
