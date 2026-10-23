package ag.r2

import ag.task.Task
import os.Path

import scala.collection.mutable

// `force_roots`: rebuild every target the program tracks directly (its
// "roots", tracked at depth 0), even if its saved result is current. Their
// inputs are reused as usual.
class State(val workspace: os.Path, val force_roots: Boolean = false)
    extends Tracker {

  // Context[Nothing] methods
  override val depth: Int = 0
  override val state: State = this
  override def producing_opt: Option[Target[Nothing]] = None

  override val route = Seq()

  def target_path(target: TargetBase | os.RelPath): os.Path =
    workspace / "targets" / (target match {
      case t: TargetBase => t.path
      case p: os.RelPath => p
    })
  def data_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "data"
  def saved_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "saved.json"
  def dirty_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "dirty"

  // Makes `target` rebuild the next time it's used, as if its last run had
  // failed: its saved result and data are discarded.
  def force(target: TargetBase): Unit =
    os.write.over(dirty_path(target), "", createFolders = true)
  def log_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "log.txt"

  private val cache = mutable.Map[os.RelPath, Task[Result[?]]]()

  def track[A](
      target: Target[A]
  )(using tracker: Tracker[?]): Task[A] = {
    tracker.check_phase(tracker.Phase.Open)
    if (tracker.route.map(_.path).contains(target.path)) {
      throw new Exception(
        s"Circular dependency detected: ${(tracker.route.map(_.path) :+ target.path).map(_.toString).mkString(" -> ")}"
      )
    }
    val result: Task[Result[?]] = cache.synchronized {
      cache.getOrElseUpdate(
        target.path, {
          // first use of this target in this run
          if (force_roots && tracker.depth == 0) force(target)
          Task {
            Context.say(
              Some(tracker),
              s"miss for ${target.path.toString} in ${this.toString}"
            )
            target.make(using
              new Tracker {
                override val depth: Int = tracker.depth + 1

                override val route = tracker.route :+ target
                override val state: State = tracker.state

                override def producing_opt: Option[Target[A]] = Some(target)
              }
            )
          }.flatMap(identity)
        }
      )
    }

    tracker.add_dependency(target, result)

    for {
      r <- result
    } yield r.value.asInstanceOf[A]

  }

}

object State {
  def run[A](path: os.Path, force_roots: Boolean = false)(
      f: State ?=> A
  ): A = {
    given State = new State(path, force_roots)
    f
  }
}
