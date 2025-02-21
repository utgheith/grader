package ag.r2

import ag.cfg.load
import ag.tracing.trace
import upickle.default.ReadWriter

case class Config(trace_miss: Boolean, trace_read_error: Boolean, trace_make: Boolean) derives ReadWriter {
  def on_miss(target: Target[?])(using ctx: Context[?]): Unit = {
    if (trace_miss) {
      trace(s"[MISS] ${ctx.target.map(_.path)} looking for ${target.path}")
    }
  }
  def on_read_error(target: Target[?], e: Throwable)(using ctx: Context[?]): Unit = {
    if (trace_read_error) {
      trace(s"[READ ERROR] ${ctx.target.map(_.path)} looking for ${target.path}: ${e.getMessage}")
    }
  }
}

val config = load[Config]
