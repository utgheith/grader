package ag.grader

import scala.util.matching.Regex

// The output of time's %E format, which the Makefiles report test run
// times in
object TimeFormat {
  private val Seconds: Regex = """(\d+\.\d+)""".r
  private val Minutes: Regex = """(\d+):(\d+.\d+)""".r
  private val Hours: Regex = """(\d+):(\d+):(\d+.\d+)""".r

  // in seconds; None for none, or a timeout
  def parse(s: Option[String]): Option[Double] = s match {
    case Some(Seconds(s))     => Some(s.nn.toDouble)
    case Some(Minutes(m, s))  => Some((m.nn.toDouble * 60) + s.nn.toDouble)
    case Some(Hours(h, m, s)) =>
      Some((h.nn.toDouble * 3600) + (m.nn.toDouble * 60) + s.nn.toDouble)
    case Some("timeout") => None
    case Some(s)         => throw new Exception(s"invalid time format: $s")
    case None            => None
  }
}
