package ag.gitolite

import upickle.default.ReadWriter

case class RedactedTestId(external_name: String, git_sha: String) derives ReadWriter
object RedactedTestId {
  given Ordering[RedactedTestId] = Ordering.by(t => t.external_name)
}

case class TestId(external_name: String, internal_name: String, git_sha: String) derives ReadWriter {
  lazy val redacted: RedactedTestId = RedactedTestId(external_name = external_name, git_sha=git_sha)
}

object TestId {
  given Ordering[TestId] = Ordering.by(t => (t.external_name, t.internal_name))
}