package ag.common

trait Logging {
  def log(msg: => Any): Unit
}

def log(msg: => Any)(using logger: Logging): Unit = {
  logger.log(msg)
}
