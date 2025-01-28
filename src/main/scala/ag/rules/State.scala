package ag.rules

import upickle.default.{ReadWriter, read, write}

import java.security.MessageDigest
import java.util.concurrent.atomic.AtomicLong
import scala.collection.{SortedMap}
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.Success
import scala.util.Try
import scala.util.Failure

object State {

  def apply()(using State): State = summon[State]

  def of(base: os.Path, monitor: StateMonitor = NopStateMonitor()): State =
    new State(base, monitor)

}

trait StateMonitor {
  def onLookup(s: State, rule: RuleBase): Unit

  def onMiss(s: State, rule: RuleBase): Unit

  def onStart(s: State, rule: RuleBase): Unit

  def onSuccess[A](s: State, rule: RuleBase, result: Result[A]): Result[A]

  def onFailure(s: State, rule: RuleBase, e: RuleException): Unit

  def onCompute(s: State, rule: RuleBase, in: Any): Unit

  def onSave(s: State, rule: RuleBase, in: Any, saved: Saved[Any]): Unit
}

class NopStateMonitor extends StateMonitor {
  override def onLookup(s: State, rule: RuleBase): Unit = ()

  override def onMiss(s: State, rule: RuleBase): Unit = ()

  override def onStart(s: State, rule: RuleBase): Unit = ()

  override def onSuccess[A](
      s: State,
      rule: RuleBase,
      result: Result[A]
  ): Result[A] =
    result

  override def onFailure(s: State, rule: RuleBase, e: RuleException): Unit =
    ()

  override def onCompute(s: State, rule: RuleBase, in: Any): Unit = ()

  override def onSave(
      s: State,
      rule: RuleBase,
      in: Any,
      saved: Saved[Any]
  ): Unit = ()
}

class CountingStateMonitor extends StateMonitor {
  val lookups: AtomicLong = AtomicLong(0)
  val starts: AtomicLong = AtomicLong(0)
  val misses: AtomicLong = AtomicLong(0)
  val computes: AtomicLong = AtomicLong(0)
  val saves: AtomicLong = AtomicLong(0)
  val successes: AtomicLong = AtomicLong(0)
  val failures: AtomicLong = AtomicLong(0)

  override def onLookup(s: State, rule: RuleBase): Unit = {
    val _ = lookups.incrementAndGet()
  }

  override def onStart(s: State, rule: RuleBase): Unit = {
    val _ = starts.incrementAndGet()
  }

  override def onMiss(s: State, rule: RuleBase): Unit = {
    val _ = misses.incrementAndGet()
  }

  override def onCompute(s: State, Rule: RuleBase, in: Any): Unit = {
    val _ = computes.incrementAndGet()
  }

  override def onSave(
      s: State,
      Rule: RuleBase,
      in: Any,
      saved: Saved[Any]
  ): Unit = {
    val _ = saves.incrementAndGet()
  }

  override def onSuccess[A](
      s: State,
      rule: RuleBase,
      result: Result[A]
  ): Result[A] =
    successes.incrementAndGet()
    result

  override def onFailure(s: State, rule: RuleBase, e: RuleException): Unit =
    val _ = failures.incrementAndGet()
}

class State private (
    val base_dir: os.Path,
    monitor: StateMonitor = NopStateMonitor()
) {

  val targets_dir: os.Path = {
    val d = base_dir / "targets"
    os.makeDir.all(d)
    d
  }

  val signed_path_dir: os.Path = {
    val d = base_dir / "signed_paths"
    os.makeDir.all(d)
    d
  }

  private val cache = new mutable.HashMap[os.RelPath, Task[Result[Any]]]()

  def get[In, Out: ReadWriter](r: Rule[In, Out]): Task[Result[Out]] = {
    monitor.onLookup(this, r)
    val out: Task[Result[?]] = cache.synchronized {
      cache.getOrElseUpdate(
        r.path, {
          monitor.onMiss(this, r)
          Task {
            try {
              monitor.onSuccess(this, r, handle_miss(r))
            } catch {
              case NonFatal(e) =>
                val re = RuleException(r, e)
                monitor.onFailure(this, r, re)
                throw re
            }
          }
        }
      )
    }

    out.asInstanceOf[Task[Result[Out]]]
  }

  private def handle_miss[In, Out: ReadWriter](
      r: Rule[In, Out]
  ): Result[Out] = {

    // It's ok to wait because:
    //    (1) we're requiring virtual threads above
    //    (2) there is no more parallelism to extract
    //    (3) we're running in a task already
    // Why not use map, flatMap, sequence, ...? Because average programmers have a hard
    // time reading and writing such abstract code.

    monitor.onStart(this, r)

    val new_dependencies_task: Task[Seq[RuleBase]] =
      r.needs.parts(using this).map(_.toSeq)

    val new_results_task: Task[Seq[Result[?]]] = for {
      new_dependencies <- new_dependencies_task
      t <- Task.sequence(new_dependencies.map(_.get_result(using this)))
    } yield t.toSeq

    // get the signatures, blocks until all dependencies are evaluated ... or any of them fail
    val new_results: Seq[Result[?]] = new_results_task.block
    val new_dependencies: Seq[RuleBase] = new_dependencies_task.block

    val new_signatures: Seq[Signature] = new_results.map(_.signature)

    val file = targets_dir / r.path

    def evaluate(old: Option[Out]): Result[Out] = {
      trace(s"evaluating ${r.path}")
      trace("depends_on")
      new_dependencies.zip(new_signatures).foreach { case (d, s) =>
        trace(s"    $s ${d.path}")
      }
      // blocks ... but we forced parallel evaluation above
      val in = r.needs.make(using this).block

      monitor.onCompute(this, r, in)
      val out = r.compute(Context(this, r, old), in)
      val out_str = write(out, indent = 1)
      val sha = MessageDigest.getInstance("sha1").nn
      val sig = Signature(sha.digest(out_str.getBytes("utf-8")).nn)
      val dependsOn = SortedMap(
        new_dependencies.zip(new_signatures).map { case (d, s) => (d.path, s) }*
      )
      // Avoid serializing twice, use streaming API
      val saved = Saved(value = out, signature = sig, dependsOn = dependsOn)
      monitor.onSave(this, r, in, saved)
      os.write.over(file, write(saved, indent = 1), createFolders = true)
      Result(out, sig)
    }

    if (new_dependencies.nonEmpty && os.exists(file)) {
      // logger.debug(
      //  s"$file exists"
      // ) // We have some old data, see if any of the dependencies changed.

      Try(read[Saved[Out]](os.read(file))) match { // read the old state
        case Success(saved) =>
          // iterate over all dependencies and see if they're identical to the old ones
          val use_old_result = new_dependencies.zip(new_signatures).forall {
            case (d, sig) => saved.dependsOn.get(d.path).contains(sig)
          }
          if (use_old_result) Result(saved.value, saved.signature)
          else evaluate(Some(saved.value))
        case Failure(e) =>
          say(s"failed to read from $file, remove and retry", e)
          os.remove.all(file)
          evaluate(None)
      }
    } else {
      evaluate(None)
    }
  }
}
