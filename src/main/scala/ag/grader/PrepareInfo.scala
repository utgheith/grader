package ag.grader

import upickle.default.ReadWriter

import java.time.{Instant, ZonedDateTime}
import ag.common.{given_ReadWriter_Instant, given_ReadWriter_ZonedDateTime}

final case class PrepareInfo(
    commit_time: ZonedDateTime,
    sha: String,
    has_report: Boolean,
    push_time: Option[Instant] = None
) derives ReadWriter
