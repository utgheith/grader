package ag.tracing

import ag.cfg.load
import ag.common.{given_ReadWriter_RelPath, Scoped}
import upickle.default.ReadWriter

import java.time.LocalDateTime
import java.util.concurrent.locks.ReentrantLock

case class Config(
    file: Option[os.RelPath],
    thread_name: Boolean,
    timestamp: Boolean
) derives ReadWriter {
  val lock = new ReentrantLock()

  def trace[A](msg: => A): Unit = {
    file.foreach { file =>
      val path = os.pwd / file
      val thread_part = if (thread_name) {
        s"[${Thread.currentThread.getName}] "
      } else ""
      val time_part = if (timestamp) {
        s"[${LocalDateTime.now().toString}] "
      } else ""

      val line = s"$thread_part$time_part${msg.toString}\n"

      lock.lockInterruptibly()
      try {
        os.write.append(path, line)
      } finally {
        lock.unlock()
      }
    }
  }

}

val tracingConfig = Scoped(load[Config])

inline def trace(msg: => Any): Unit = {
  tracingConfig.get.trace(msg)
}
