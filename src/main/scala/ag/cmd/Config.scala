package ag.cmd

import ag.cfg.load
import upickle.default.Reader

private case class Config(enable_tracing: Boolean, remote_limit: Int)
    derives Reader

private lazy val config = load[Config]

def trace[A](msg: => A): Unit = {
  if (config.enable_tracing) {
    ag.tracing.trace(msg)
  }
}
