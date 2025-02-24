package ag.tracing

import ag.cfg.load
import ag.common.given_ReadWriter_RelPath
import upickle.default.Reader

import java.time.LocalDateTime
import java.util.concurrent.locks.ReentrantLock

case class Config(
    file: Option[os.RelPath],
    thread_name: Boolean,
    timestamp: Boolean
) derives Reader
lazy val config: Config =
  load[Config]

object Config {
  val trace_lock = new ReentrantLock()
}

def trace[A](msg: => A): Unit = {
  config.file.foreach { file =>
    val path = os.pwd / file
    val thread_part = if (config.thread_name) {
      s"[${Thread.currentThread.getName}] "
    } else ""
    val time_part = if (config.timestamp) {
      s"[${LocalDateTime.now()}] "
    } else ""

    val line = s"$thread_part$time_part$msg\n"

    Config.trace_lock.lockInterruptibly()
    try {
      os.write.append(path, line)
    } finally {
      Config.trace_lock.unlock()
    }
  }
}
