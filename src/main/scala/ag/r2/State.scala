package ag.r2

import os.Path
import upickle.default.ReadWriter

import java.lang.Thread.Builder
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

object State {
  val builder: Builder.OfVirtual = Thread.ofVirtual().name("v", 0)
}

class State(val workspace: os.Path) extends Tracker {

  // Context[Nothing] methods
  override val depth: Int = 0
  override val state: State = this
  override def producing_opt: Option[Target[Nothing]] = None

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
  def backup_path(target: TargetBase | os.RelPath): os.Path = {
    val tp = target_path(target)
    tp / os.up / s"${tp.last}.bak"
  }

  private val cache = TrieMap[os.RelPath, Future[Result[?]]]()

  def track[A: ReadWriter](
      target: Target[A]
  )(using tracker: Tracker[?]): Future[A] = {
    val result: Future[Result[?]] = cache.getOrElseUpdate(
      target.path, {
        Context.say(Some(tracker), s"miss for ${target.path}")
        target.make(using
          new Tracker {
            override val depth: Int = tracker.depth + 1
            override val state: State = tracker.state

            override def producing_opt: Option[Target[A]] = Some(target)

            override def execute(runnable: Runnable): Unit =
              tracker.state.execute(runnable)

            override def reportFailure(cause: Throwable): Unit =
              tracker.state.reportFailure(cause)
          }
        )
      }
    )

    tracker.add_dependency(target, result)

    for {
      r <- result
    } yield r.value.asInstanceOf[A]

  }

}
