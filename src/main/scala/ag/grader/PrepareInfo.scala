package ag.grader

import upickle.default.ReadWriter

import java.time.{Duration, Instant, ZonedDateTime}
import ag.common.{given_ReadWriter_Instant, given_ReadWriter_ZonedDateTime}

// How `prepare` chose the commit it graded.
enum CommitSource derives ReadWriter:
  // The latest commit: last on master before the cutoff, or master itself.
  // Also used when the commit named in a commit id file couldn't be checked
  // out and prepare fell back to master.
  case Latest
  // Named in the submission's commit id file; `sha` is the file's contents.
  case FromFile(file_name: String, sha: String)

// Private: the commit's sha and times identify the student to anyone who can
// see their repo. Only staff and the student themselves may see this; what
// may be published is `redacted`.
@upickle.implicits.allowUnknownKeys(false)
final case class PrepareInfo(
    commit_time: ZonedDateTime,
    sha: String,
    has_report: Boolean,
    push_time: Option[Instant],
    commit_source: CommitSource
) derives ReadWriter {
  def redacted(code_cutoff: ZonedDateTime): RedactedPrepareInfo =
    RedactedPrepareInfo(
      has_report = has_report,
      days_late = PrepareInfo.days_late(commit_time, code_cutoff),
      from_commit_file = commit_source != CommitSource.Latest,
      push_gap = PrepareInfo.push_gap(this).isDefined
    )
}

object PrepareInfo {
  // When a commit first reached the server, from its push-time notes: bare
  // "<secs>" lines (the hook appends one per push that leaves HEAD on it), or
  // "PushTime: <secs>" lines (older repos). The earliest counts: that's when
  // the commit arrived; later ones are re-pushes.
  def push_time_from(note_lines: Seq[String]): Option[Instant] =
    note_lines
      .map(_.trim)
      .flatMap { line =>
        line.split(' ') match {
          case Array(secs)              => secs.toLongOption
          case Array("PushTime:", secs) => secs.toLongOption
          case _                        => None
        }
      }
      .minOption
      .map(Instant.ofEpochSecond(_, 0).nn)

  // Fractional days past the cutoff (6 hours late is 0.25); 0 if on time.
  def days_late(commit: ZonedDateTime, cutoff: ZonedDateTime): Double =
    if (!commit.isAfter(cutoff)) 0.0
    else Duration.between(cutoff, commit).nn.getSeconds / 86400.0

  // Commit times come from the student's machine, push times from the
  // server; a gap of over 30 minutes (either way) is worth a look.
  def push_gap(info: PrepareInfo): Option[Duration] =
    info.push_time
      .map(p => Duration.between(info.commit_time.toInstant, p).nn)
      .filter(_.abs.nn.toMinutes > 30)
}

// What may be published about how a submission was prepared: facts the
// report needs, and nothing that identifies the student (no sha, no times).
@upickle.implicits.allowUnknownKeys(false)
final case class RedactedPrepareInfo(
    has_report: Boolean,
    // past the code cutoff, in days; 0 if on time (needed for the score)
    days_late: Double,
    // graded the commit named in a commit id file
    from_commit_file: Boolean,
    // commit and push times over 30 minutes apart
    push_gap: Boolean
) derives ReadWriter
