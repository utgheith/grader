package ag.grader

import ag.r2.ToRelPath
import upickle.default.{readwriter, ReadWriter}
import scala.math.Ordering

case class CSID(value: String) {
  override def toString: String = value
}

object CSID {
  given ReadWriter[CSID] = readwriter[String].bimap(
    csid => csid.value,
    value => CSID(value)
  )
  given Ordering[CSID] = Ordering.by(_.value)
  given ToRelPath[CSID] = { csid => os.RelPath(csid.value) }
}
