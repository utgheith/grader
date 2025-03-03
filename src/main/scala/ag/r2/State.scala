package ag.r2

import ag.common.Signer
import os.Path
import upickle.default.{ReadWriter, read, write}

import java.lang.Thread.Builder
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.control.NonFatal

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

  def target_path(target: TargetBase): os.Path =
    workspace / "targets" / target.path
  def data_path(target: TargetBase): os.Path = target_path(target) / "data"
  def saved_path(target: TargetBase): os.Path =
    target_path(target) / "saved.json"
  def dirty_path(target: TargetBase): os.Path = target_path(target) / "dirty"

  private val cache = TrieMap[os.RelPath, Future[Result[?]]]()

  // pre-condition: ctx is populated with dependencies for this target
  // The simplest way to achieve this is to write code that looks like this:
  //  Target {
  //       a.track
  //       b.track
  //       run_if_needed { // depends on a and b
  //            ...
  //       }
  //  }
  def run_if_needed[A: ReadWriter](using
      tracker: Tracker[A]
  )(
      f: Producer[A] ?=> Future[A]
  ): Future[Result[A]] = {

    val s = this

    val producer = new Producer[A] {
      override val depth: Int = tracker.depth
      override val producing: Target[A] = tracker.producing_opt.get
      override val state: State = s

      override def producing_opt: Option[Target[A]] = tracker.producing_opt

      override def execute(runnable: Runnable): Unit = s.execute(runnable)

      override def reportFailure(cause: Throwable): Unit =
        s.reportFailure(cause)
    }

    if (os.exists(producer.dirty_path)) {
      say("removing dirty state")
      os.remove.all(producer.target_path)
    }

    // (1) let's find out if we have a saved value
    val old_saved: Option[Saved[A]] = if (os.isFile(producer.saved_path)) {
      try {
        say("loading old state")
        val old = read[Saved[A]](os.read(producer.saved_path))
        // (2) we seem to have one, check if the dependencies changed
        // ctx has the newly discovered dependencies
        // old has the dependency at the time the value was saved
        say("checking dependencies")
        if (
          tracker.dependencies
            .forall((p, s) => old.depends_on.get(p).contains(s))
        ) {
          say("keeping old state")
          Some(old)
        } else {
          None
        }
      } catch {
        case NonFatal(e) =>
          say("load error")
          os.remove.all(producer.saved_path)
          None
      }
    } else {
      None
    }

    old_saved match {
      case Some(old_saved) =>
        // We found a result we like on disk, done
        Future.successful(old_saved.result)
      case None =>
        // We either didn't find a result on disk or we found one with changed dependencies.
        // In either case, we forget the old result and evaluate again

        say("making")

        // remove the old result
        os.remove.all(producer.target_path)

        // mark it as dirty while we compute it. This allows is to recover is the program
        // terminates in the middle of the computation
        os.write.over(producer.dirty_path, "", createFolders = true)

        // Run the computation (asynchronous)
        f(using producer).map { new_value =>
          // We have a new result, store it on disk
          val new_result = Result(new_value, Signer.sign(new_value))
          val new_saved = Saved(new_result, tracker.dependencies)
          os.write.over(
            producer.saved_path,
            write(new_saved, indent = 2),
            createFolders = true
          )

          os.remove.all(producer.dirty_path)
          say("done")
          new_result
        }

    }
  }

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
