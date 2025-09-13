package ag.r2

import os.Path

import java.lang.Thread.Builder
import scala.concurrent.Future
import scala.collection.mutable

object State {
  val builder: Builder.OfVirtual = Thread.ofVirtual().name("v", 0)
}

class State(val workspace: os.Path) extends Tracker {

  // Context[Nothing] methods
  override val depth: Int = 0
  override val state: State = this
  override def producing_opt: Option[Target[Nothing]] = None

  override val route = Seq()

  // ExecutionContext methods
  override def execute(runnable: Runnable): Unit = {
    val _ = State.builder.start(runnable)
  }
  override def reportFailure(cause: Throwable): Unit = {
    cause.printStackTrace()
    sys.exit(-1)
  }

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

  private val cache = mutable.Map[os.RelPath, Future[Result[?]]]()

  def track[A](
      target: Target[A]
  )(using tracker: Tracker[?]): Future[A] = {
    if (tracker.route.map(_.path).contains(target.path)) {
      throw new Exception(
        s"Circular dependency detected: ${(tracker.route.map(_.path) :+ target.path).map(_.toString).mkString(" -> ")}"
      )
    }
    val result: Future[Result[?]] = cache.synchronized {
      cache.getOrElseUpdate(
        target.path, {
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

              override def execute(runnable: Runnable): Unit =
                tracker.state.execute(runnable)

              override def reportFailure(cause: Throwable): Unit =
                tracker.state.reportFailure(cause)
            }
          )
          made
        }
      )
    }

    tracker.add_dependency(target, result)

    for {
      r <- result
    } yield r.value.asInstanceOf[A]

  }

}
