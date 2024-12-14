package ag.grader

import upickle.default.ReadWriter
import java.time.{ZonedDateTime, Duration}
import ag.rules.{given_ReadWriter_ZonedDateTime, given_ReadWriter_Duration}

final case class LateCommit(
    hash: String,
    time: ZonedDateTime,
    delay: Duration,
    message: String
) derives ReadWriter
