package ag.rules

import language.experimental.namedTuples

object Periodic {
  def apply(ms: Long): Maker[Long] = Rule(os.RelPath(ms.toString)) {
    (System.currentTimeMillis() / ms) * ms
  }
}
