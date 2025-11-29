package ag.r2

import ag.common.down

import java.util.concurrent.Semaphore
import scala.annotation.implicitNotFound

//
// Context for a running computations
//
// A context is created by the environment as a computation is about
// to start and is implicitly passed around (as a context argument)
//
// It has 2 jobs:
//    - carries information about the target being computed (state, target, ...)
//    - collects the dependencies as they're discovered
//
// It is declared as a capability (extends Capability) because we don't want
// it to accidentally leak into closures and member variable.
//
// Why do we want to prevent it from leaking? Because calling "track" only
// makes sense during the dependency collection phase of a computation. We
// don't want to context created for a particular computation to outlive
// the dependency collection phase.

object Context {
  private val printLock: Semaphore = new Semaphore(1)

  private lazy val say_path = {
    val p = os.pwd / "say.txt"
    os.remove.all(p)
    p
  }

  def say(ctx: Option[Context[?, ?]], msg: => Any): Unit = {
    val t = if (msg == null) {
      "<null>"
    } else {
      msg.toString
    }

    val dots = "." * ctx.map(_.depth).getOrElse(0)
    val thread_name = Thread.currentThread().getName

    val producing = for {
      ctx <- ctx
      prod <- ctx.producing_opt
    } yield prod.path

    val out =
      s"$dots[$thread_name]${producing.map(p => s" [${p.toString}]").getOrElse("")} $t\n"

    /*
    ctx.foreach {
      case log: Logging =>

        log.log(out)
      case _ => /* not a producer */

    }*/

    printLock.down(1) {
      if (Noise()) {
        print(out)
      }
      os.write.append(say_path, out)
    }

  }
}

@implicitNotFound("no given Context")
trait Context[-E <: Exception, +A] {
  val route: Seq[Target[?, ?]]
  val depth: Int
  val state: State
  def producing_opt: Option[Target[E, A]]
}
