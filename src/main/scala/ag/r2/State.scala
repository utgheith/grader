package ag.r2

import ag.task.Task
import os.Path

import scala.collection.mutable

class State(val workspace: os.Path) extends Tracker {

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
        target.path,
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
      )
    }

    tracker.add_dependency(target, result)

    for {
      r <- result
    } yield r.value.asInstanceOf[A]

  }

}
