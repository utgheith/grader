package ag.cmd

import scala.compiletime.summonFrom
import scala.util.control.NonFatal

trait Command {
  def my_part: os.Shellable
  def cmd: os.Shellable = my_part

  def opt[A](o: String, a: A | Null)(using
      ev: A => os.Shellable
  ): os.Shellable = {
    if (a == null) Seq() else Seq[os.Shellable](o, ev(a))
  }
}

trait CallableCommand[+Out] extends Command {
  lazy val proc: os.proc = os.proc(cmd)
  def translate(res: os.CommandResult): Out

  def call(cwd: os.Path = os.pwd, check: Boolean = true): Out = {
    trace(os.Shellable.unapply(proc.command).value)
    try {
      val out = translate(proc.call(cwd = cwd, check = check))
      trace("ok")
      out
    } catch {
      case NonFatal(e) =>
        trace(s"failed with ${e.getCause.getMessage}")
        throw e
    }
  }
}

trait SubCommand extends Command {
  def parent: Command
  override lazy val cmd: os.Shellable = Seq[os.Shellable](parent.cmd, my_part)
}
