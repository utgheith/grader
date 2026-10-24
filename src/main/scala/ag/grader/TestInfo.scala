package ag.grader

import ag.r2.WithData
import upickle.default.ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class TestInfo(id: TestId, sp: WithData[Unit]) derives ReadWriter
