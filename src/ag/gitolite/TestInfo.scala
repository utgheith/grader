package ag.gitolite

import ag.rules.SignedPath
import upickle.default.ReadWriter

case class TestInfo(id: TestId, sp: SignedPath[String]) derives ReadWriter