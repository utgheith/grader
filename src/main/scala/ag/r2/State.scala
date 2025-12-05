package ag.r2

import os.Path

import java.lang.Thread.Builder
import scala.collection.mutable
import ag.common.Fork

object State {
  val builder: Builder.OfVirtual = Thread.ofVirtual().name("v", 0)
}

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

  private val cache = mutable.Map[os.RelPath, Fork[Result[?]]]()

  def track[A](
      target: Target[A]
  )(using tracker: Tracker): Fork[A] = {
    if (tracker.route.map(_.path).contains(target.path)) {
      throw new Exception(
        s"Circular dependency detected: ${(tracker.route.map(_.path) :+ target.path).map(_.toString).mkString(" -> ")}"
      )
    }
    val result: Fork[Result[A]] = cache.synchronized {
      val res = cache.getOrElseUpdate(
        target.path, {
          val out: Fork[Result[A]] = Fork {
            Context.say(
              Some(tracker),
              s"miss for ${target.path.toString} in ${this.toString}"
            )
            val made = target.make(using
              new Tracker {
                override val depth: Int = tracker.depth + 1

                override val route = tracker.route :+ target
                override val state: State = tracker.state

                override def producing_opt: Option[Target[A]] = Some(target)
              }
            )
            made
          }
          out
        }
      )
      // Scala doesn't have heterogeneous maps, but we know from "val out" that we have the correct type
      res.asInstanceOf[Fork[Result[A]]]
    }

    tracker.add_dependency(target, result)

    result.map { case Result(a, _) =>
      a.asInstanceOf[A]
    }

  }

}
