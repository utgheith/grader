package ag.r2

import ag.common.{Signature, Signer, given_VirtualExecutionContext}
import os.Path
import upickle.default.{ReadWriter, read, write}

import java.security.MessageDigest
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class State(val workspace: os.Path) extends Tracker {
  
  override val state: State = this
  
  def target_path(target: TargetBase): os.Path = workspace / "targets" / target.path
  def data_path(target: TargetBase): os.Path = target_path(target) / "data"
  def saved_path(target: TargetBase): os.Path = target_path(target) / "saved.json"
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
  def run_if_needed[A: ReadWriter](using tracker: Tracker[A])(
      f: Producer[A] ?=> Future[A]
  ): Future[Result[A]] = {
    
    val producer = new Producer[A] {
      override val producing: Target[A] = tracker.producing_opt.get 
    }
    
    if (os.exists(producer.dirty_path)) {
      config.trace_dirty(producer.producing)
      os.remove.all(producer.target_path)
    }

    // (1) let's find out if we have a saved value
    val old_saved: Option[Saved[A]] = if (os.isFile(producer.saved_path)) {
      try {
        val old = read[Saved[A]](os.read(producer.saved_path))
        // (2) we seem to have one, check if the dependencies changed
        // ctx has the newly discovered dependencies
        // old has the dependency at the time the value was saved
        if (
          tracker.dependencies.forall((p, s) => old.depends_on.get(p).contains(s))
        ) {
          Some(old)
        } else {
          None
        }
      } catch {
        case NonFatal(e) =>
          config.trace_read_error(producer, producer.producing, e)
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
          new_result
        }

    }
  }

  def track[A: ReadWriter](
      target: Target[A]
  )(using tracker: Tracker[?]): Future[A] = {
    val result: Future[Result[?]] = cache.getOrElseUpdate(
      target.path, {
        config.trace_miss(tracker, target)
        target.make(using new Tracker {

        })
      }
    )

    tracker.add_dependency(target, result)

    for {
      r <- result
    } yield r.value.asInstanceOf[A]

  }

}
