package ag.cmd

import ag.cfg.load
import upickle.default.{Reader, Writer}

private case class Config(enable_tracing: Boolean) derives Reader

private lazy val config = load[Config]

def trace[A: Writer](msg: => A): Unit = {
  if (config.enable_tracing) {
    ag.tracing.trace(msg)
  }
}
