package ag.r2

import ag.common.{Signature, Signer, given_VirtualExecutionContext}
import os.Path
import upickle.default.{ReadWriter, read, write}

import java.security.MessageDigest
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.control.NonFatal

class State(val workspace: os.Path) {
  
  def target_path(target: TargetBase): os.Path = workspace / "targets" / target.path
  def data_path(target: TargetBase): os.Path = target_path(target) / "data"
  def saved_path(target: TargetBase): os.Path = target_path(target) / "saved.json"
  def dirty_path(target: TargetBase): os.Path = target_path(target) / "dirty"

  private val cache = TrieMap[os.RelPath, Future[Result[?]]]()

  def apply[A](f: Consumer ?=> Future[A]): Future[A] = {
    f(using Consumer(this))
  }

  // pre-condition: ctx is populated with dependencies for this target
  // The simplest way to achieve this is to write code that looks like this:
  //  Target {
  //       a.track
  //       b.track
  //       run_if_needed { // depends on a and b
  //            ...
  //       }
  //  }
  def run_if_needed[A: ReadWriter](ctx: Producer[A])(
      f: () => Future[A]
  ): Future[Result[A]] = {
    import ctx.target
    
    if (os.exists(ctx.state.dirty_path(target))) {
      config.trace_dirty(target)
      os.remove.all(ctx.state.target_path(target))
    }

    // (1) let's find out if we have a saved value
    val old_saved: Option[Saved[A]] = if (os.isFile(ctx.state.saved_path(target))) {
      try {
        val old = read[Saved[A]](os.read(ctx.state.saved_path(target)))
        // (2) we seem to have one, check if the dependencies changed
        // ctx has the newly discovered dependencies
        // old has the dependency at the time the value was saved
        if (
          ctx.dependencies.forall((p, s) => old.depends_on.get(p).contains(s))
        ) {
          Some(old)
        } else {
          None
        }
      } catch {
        case NonFatal(e) =>
          config.trace_read_error(ctx, target, e)
          os.remove.all(ctx.state.saved_path(target))
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
        os.remove.all(ctx.target_path)
        
        // mark it as dirty while we compute it. This allows is to recover is the program
        // terminates in the middle of the computation
        os.write.over(ctx.dirty_path, "", createFolders = true)
        
        // Run the computation (asynchronous)
        f().map { new_value =>
          // We have a new result, store it on disk
          
          val md = MessageDigest.getInstance("SHA1")
          Signer.signWith(md, new_value)
          /* we use the skip_data filter as a signal that we have data */
          val skip_filter = ctx.skip_filter
          if (skip_filter != null) {
            Signer.signWith(md, (ctx.data_path, skip_filter))
          }
          val signature = Signature(md.digest())
          val new_result = Result(new_value, signature)
          val new_saved = Saved(new_result, ctx.dependencies)
          os.write.over(
            ctx.saved_path,
            write(new_saved, indent = 2),
            createFolders = true
          )

          os.remove.all(ctx.dirty_path)
          new_result
        }

    }
  }

  def track[A: ReadWriter](
      ctx: Context,
      target: Target[A]
  ): Future[A] = {
    val result: Future[Result[?]] = cache.getOrElseUpdate(
      target.path, {
        config.trace_miss(ctx, target)
        target.make(ctx)
      }
    )

    ctx.add_dependency(target, result)

    for {
      r <- result
    } yield r.value.asInstanceOf[A]

  }

}
