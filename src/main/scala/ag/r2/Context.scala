package ag.r2

import ag.common.down

import java.util.concurrent.Semaphore
import scala.concurrent.ExecutionContext
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

  def say(ctx: Option[Context[?]], msg: => Any): Unit = if (Noise()) {
    val t = if (msg == null) {
      "<null>"
    } else {
      msg.toString
    }
    printLock.down(1) {
      (1 to ctx.map(_.depth).getOrElse(0)).foreach(_ => System.err.print("."))
      System.err.print(s"[${Thread.currentThread().getName}] ")
      for {
        ctx <- ctx
        prod <- ctx.producing_opt
      } System.err.print(s"[${prod.path}] ")
      System.err.print(t)
      System.err.println()
    }
  }
}

@implicitNotFound("no given Context")
trait Context[A] extends ExecutionContext {
  val depth: Int
  val state: State
  def producing_opt: Option[Target[A]]
}
