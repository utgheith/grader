package ag.r2

import ag.common.{Signature, block}

import java.util.concurrent.atomic.AtomicInteger
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

class Context[A](
    val state: State,
    val target: Option[Target[A]],
    val parent: Option[Context[?]]
) {

  var skip_filter: (os.RelPath => Boolean) | Null = null

  private val phase = new AtomicInteger(1)

  lazy val target_path: os.Path = state.targets / target.get.path
  lazy val data_path: os.Path = target_path / "data"
  lazy val dirty_path: os.Path = target_path / "dirty"
  lazy val saved_path: os.Path = target_path / "saved.json"

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
      (p, result) <- added_dependencies.toSeq
    } yield (p, result.block.signature)).to(SortedMap)
  }

  def run_if_needed(
      f: => Future[A]
  )(using ReadWriter[A]): Future[Result[A]] = {
    state.run_if_needed(this) { () => f }
  }
}
