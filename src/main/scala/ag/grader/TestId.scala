package ag.grader

import ag.r2.ToRelPath
import upickle.default.ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class RedactedTestId(external_name: String) derives ReadWriter
object RedactedTestId {
  given Ordering[RedactedTestId] = Ordering.by(t => t.external_name)
}

@upickle.implicits.allowUnknownKeys(false)
case class TestId(external_name: String, internal_name: String)
    derives ReadWriter {
  lazy val redacted: RedactedTestId =
    RedactedTestId(external_name = external_name)
}

object TestId {
  given Ordering[TestId] = Ordering.by(t => (t.external_name, t.internal_name))
  given ToRelPath[TestId] = { tid =>
    os.RelPath(tid.external_name) / tid.internal_name
  }
}
