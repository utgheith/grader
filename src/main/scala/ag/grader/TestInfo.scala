package ag.grader

import ag.rules.SignedPath
import upickle.default.ReadWriter

case class TestInfo(id: TestId, sp: SignedPath[Unit]) derives ReadWriter
