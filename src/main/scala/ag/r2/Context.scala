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
    Context.say(Some(this), s"depends on ${d.path}")
    added_dependencies.update(d.path, fr)
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

  // override lazy val producing_opt: Option[Target[A]] = Some(producing)

  // var skip_filter: (os.RelPath => Boolean) | Null = null

  lazy val target_path: os.Path = state.target_path(producing)
  lazy val saved_path: os.Path = state.saved_path(producing)
  lazy val dirty_path: os.Path = state.dirty_path(producing)
  lazy val data_path: os.Path = state.data_path(producing)
}
/*
@implicitNotFound("Can't find given Context")
trait Context extends ExecutionContext {
  val state: State
  val target_opt: Option[TargetBase]
  val parent_opt: Option[Context]

  def add_dependency(target: TargetBase, fr: Future[Result[?]]): Unit
}

class Consumer extends Context {
    override lazy val target_opt: Option[TargetBase] = None
    override lazy val parent_opt: Option[Consumer] = None

    override def add_dependency(target: TargetBase, fr: Future[Result[?]]): Unit = {}
}

@implicitNotFound("Can't fund given Consumer")
class SimpleConsumer(val state: State) extends Consumer {

}

@implicitNotFound("Can't find given Producer")
class Producer[A](override val state: State,
    val target: Target[A],
    val parent: Context
) extends Context {

  var skip_filter: (os.RelPath => Boolean) | Null = null

  lazy val target_path: os.Path = state.target_path(target)
  lazy val saved_path: os.Path = state.saved_path(target)
  lazy val dirty_path: os.Path = state.dirty_path(target)
  lazy val data_path: os.Path = state.data_path(target)

  private val phase = new AtomicInteger(1)

  // Called by "track" to add a discovered dependency
  private val added_dependencies = TrieMap[os.RelPath, Future[Result[?]]]()

  override def add_dependency(d: TargetBase, fr: Future[Result[?]]): Unit = {
    if (phase.get() != 1) {
      throw IllegalStateException(s"phase = ${phase.get()}")
    }
    println(s"${target.path} depends on ${d.path}")
    added_dependencies.update(d.path, fr)
  }

  // Returns the known dependencies
  lazy val dependencies: SortedMap[os.RelPath, Signature] = {
    val old = phase.getAndAdd(1)
    if (old != 1) {
      throw IllegalStateException(s"phase = ${phase.get()}")
    }
    (for {
      (p, result) <- added_dependencies.toSeq
    } yield (p, result.block.signature)).to(SortedMap)
  }

  def run_if_needed(
      f: => Future[A]
  )(using ReadWriter[A]): Future[Result[A]] = {
    state.run_if_needed(this) { () => f }
  }

  override lazy val target_opt: Option[Target[A]] = Some(target)
  override lazy val parent_opt: Option[Context] = Some(parent)
}
 */
