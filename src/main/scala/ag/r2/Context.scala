package ag.r2

import language.experimental.captureChecking

//
// Context for a running computations
//
// A context is created by the environment as a computation is about
// to start, and is passed around as an explicit argument.
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
  def say(ctx: Option[Context[?]]^, msg: => Any): Unit = {
    val dots = ctx match {
      case Some(c) => "." * c.depth
      case None    => ""
    }
    val producing = ctx match {
      case Some(c) => c.producing_opt.map(_.path.toString)
      case None    => None
    }
    ag.common.Say.line(dots, producing, msg)
  }
}

trait Context[A] extends caps.SharedCapability {
  val route: Seq[Target[?]]
  val depth: Int
  val state: State
  def producing_opt: Option[Target[A]]
}
