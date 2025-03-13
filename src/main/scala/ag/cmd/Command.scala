package ag.cmd

import scala.compiletime.summonFrom
import java.util.concurrent.Semaphore
import ag.common.down

object Command {
  val remote_lock = new Semaphore(config.remote_limit)
}

trait Command {
  def parent: Option[Command]
  def my_part: os.Shellable
  def my_remote: Boolean
  def cmd: os.Shellable = parent.map(_.cmd) match {
    case Some(ps) => Seq(ps, my_part)
    case None     => my_part
  }
  def remote: Boolean = my_remote || parent.exists(_.remote)

  def opt[A](o: String, a: A | Null)(using
      ev: A => os.Shellable
  ): os.Shellable = {
    if (a == null) Seq() else Seq[os.Shellable](o, ev(a))
  }
}

trait CallableCommand[+Out] extends Command {
  lazy val proc: os.proc = os.proc(cmd)
  def translate(res: os.CommandResult): Out

  def call(cwd: os.Path): Either[os.CommandResult, Out] = {
    trace(os.Shellable.unapply(proc.command).value)
    val res = if (remote) {
      proc.call(cwd = cwd, check = false)
    } else {
      Command.remote_lock.down {
        proc.call(cwd = cwd, check = false)
      }
    }
    if (res.exitCode != 0) {
      trace(s"failed with ${res.exitCode}")
      Left(res)
    } else {
      trace("ok")
      val out = translate(res)
      Right(out)
    }
  }

  def check(cwd: os.Path = os.pwd): Out = {
    call(cwd = cwd) match {
      case Left(res) =>
        throw os.SubprocessException(res)
      case Right(out) =>
        out
    }
  }
}
