package ag.gitolite

import upickle.default.ReadWriter
import java.time.ZonedDateTime
import ag.rules.given_ReadWriter_ZonedDateTime

final case class PrepareInfo(commit_time: ZonedDateTime, sha: String, has_report: Boolean) derives ReadWriter
