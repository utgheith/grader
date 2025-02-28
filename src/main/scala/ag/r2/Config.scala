package ag.r2

import ag.cfg.load
import ag.common.given_ReadWriter_RelPath
import ag.tracing.trace
import upickle.default.ReadWriter

case class TraceFlags(
    miss: Boolean,
    read_error: Boolean,
    make: Boolean,
    dirty: Boolean
) derives ReadWriter

case class Config(
    trace_flags: TraceFlags,
    test_dir: Option[os.RelPath]
) derives ReadWriter {
  def trace_dirty(target: Target[?]): Unit = {
    if (trace_flags.dirty) {
      trace(s"[DIRTY] ${target.path}")
    }
  }
  def trace_miss(ctx: Context[?], target: TargetBase): Unit = {
    if (trace_flags.miss) {
      trace(s"[MISS] ${ctx.producing_opt.map(_.path)} looking for ${target.path}")
    }
  }
  def trace_read_error(
      ctx: Context[?],
      target: TargetBase,
      e: Throwable
  ): Unit = {
    if (trace_flags.read_error) {
      trace(
        s"[READ ERROR] ${ctx.producing_opt.map(_.path)} looking for ${target.path}: ${e.getMessage}"
      )
    }
  }
  def trace_make(ctx: Context[?], target: TargetBase): Unit = {
    if (trace_flags.make) {
      trace(
        s"[MAKE] ${target.path}"
      )
    }
  }
  lazy val get_test_dir: os.Path = test_dir match {
    case Some(rp) =>
      val d = os.pwd / rp / "workspace"
      os.remove.all(d)
      d
    case None =>
      os.temp.dir(deleteOnExit = true)
  }
}

val config = load[Config]
