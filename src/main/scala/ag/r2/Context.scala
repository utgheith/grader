package ag.r2

import ag.common.block
import ag.rules.Signature

import java.util.concurrent.atomic.AtomicInteger
import scala.caps.Capability
import scala.collection.SortedMap
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

import upickle.default.ReadWriter

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

case class Context[+A](state: State, target: Option[Target[A]], parent: Option[Context[?]]) extends Capability {
  
  private val phase = new AtomicInteger(1)
  
  // Called by "track" to add a discovered dependency
  private val added_dependencies = TrieMap[os.RelPath, Future[Result[?]]]()
  def add_dependency(d: Target[?], fr: Future[Result[?]]): Unit = {
    if (phase.get() != 1) {
      throw IllegalStateException(s"phase = ${phase.get()}")
    }
    println(s"${target.map(_.path)} depends on ${d.path}")
    added_dependencies.update(d.path, fr)
  }

  // Returns the known dependencies
  lazy val dependencies: SortedMap[os.RelPath, Signature] = {
    val old = phase.getAndAdd(1)
    if (old != 1) {
      throw IllegalStateException(s"phase = ${phase.get()}")
    }
    (for {
      (p,result) <- added_dependencies.toSeq
    } yield (p,result.block.signature)).to(SortedMap)
  }
}

object Context {
  def apply(using Context[?]): Context[?] = summon[Context[?]]
}

def run_if_needed[A: ReadWriter](f: => Future[A])(using ctx: Context[A]): Future[Result[A]] = {
  ctx.state.run_if_needed { () => f }
} 
