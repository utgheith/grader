package ag.grader

import upickle.default.ReadWriter

import java.time.{Instant, ZonedDateTime}
import ag.common.{given_ReadWriter_Instant, given_ReadWriter_ZonedDateTime}

// How `prepare` chose the commit it graded.
enum CommitSource derives ReadWriter:
  // The latest commit: last on master before the cutoff, or master itself.
  // Also used when the commit named in a commit id file couldn't be checked
  // out and prepare fell back to master.
  case Latest
  // Named in the submission's commit id file; `sha` is the file's contents.
  case FromFile(file_name: String, sha: String)

final case class PrepareInfo(
    commit_time: ZonedDateTime,
    sha: String,
    has_report: Boolean,
    push_time: Option[Instant] = None,
    // Defaulted so results saved before this was tracked still load.
    commit_source: CommitSource = CommitSource.Latest
) derives ReadWriter
