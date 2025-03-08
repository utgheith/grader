package ag.r2

import ag.common.{block, down, Signature}

import java.util.concurrent.Semaphore
import scala.collection.SortedMap
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}
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

@implicitNotFound("no given Tracker")
trait Tracker[A] extends Context[A] {

  private val added_dependencies = TrieMap[os.RelPath, Future[Result[?]]]()

  def add_dependency(d: TargetBase, fr: Future[Result[?]]): Unit = {
    if (d.is_peek) {
      Context.say(Some(this), s"does not depend on ${d.path}")
    } else {
      Context.say(Some(this), s"depends on ${d.path}")
      added_dependencies.update(d.path, fr)
    }
  }

  // Returns the known dependencies
  lazy val dependencies: SortedMap[os.RelPath, Signature] = {
    (for {
      (p, result) <- added_dependencies.toSeq
    } yield (p, result.block.signature)).to(SortedMap)
  }
}

@implicitNotFound("no given Producer")
trait Producer[A] extends Context[A] {
  def producing: Target[A]
  def old_state: OldState[A]

  lazy val target_path: os.Path = state.target_path(producing)
  lazy val saved_path: os.Path = state.saved_path(producing)
  lazy val dirty_path: os.Path = state.dirty_path(producing)
  lazy val data_path: os.Path = state.data_path(producing)
}
