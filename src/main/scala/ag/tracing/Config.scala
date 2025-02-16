package ag.tracing

import ag.cfg.load
import ag.common.{given_ReadWriter_LocalDateTime, given_ReadWriter_RelPath}
import upickle.default.{Reader, Writer, write, writeJs}

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

def trace[A: Writer](msg: => A): Unit = {
  config.file.foreach { file =>
    val out = ujson.Obj()
    val path = os.pwd / file
    out("message") = writeJs(msg)
    if (config.thread_name) {
      out("thread_name") = Thread.currentThread.getName
    }
    Config.trace_lock.lockInterruptibly()
    try {
      if (config.timestamp) {
        out("timestamp") = writeJs(LocalDateTime.now())
      }
      os.write.append(path, write(out, indent = 2))
    } finally {
      Config.trace_lock.unlock()
    }
  }
}
