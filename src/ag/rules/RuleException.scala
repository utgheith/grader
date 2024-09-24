package ag.rules

import scala.util.control.NonFatal

case class RuleException private (
    thread_name: String,
    chain: Seq[RuleBase],
    cause: Throwable
) extends Exception(cause) {
  override def getMessage: String = s"[$thread_name] [${chain.map(_.path)}]"
}

object RuleException {
  def apply(r: RuleBase, cause: Throwable): RuleException = cause match {
    case RuleException(thread_name, chain, cause) =>
      new RuleException(thread_name, r +: chain, cause)
    case NonFatal(e) =>
      new RuleException(Thread.currentThread.getName, Seq(r), e)
  }
}
