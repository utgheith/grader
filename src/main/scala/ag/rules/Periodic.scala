package ag.rules

object Periodic {
  def apply(ms: Long): Maker[Long] = Rule(os.RelPath(ms.toString)) {
    (System.currentTimeMillis() / ms) * ms
  }
}
