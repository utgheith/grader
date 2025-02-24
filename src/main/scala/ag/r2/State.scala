package ag.r2

import ag.common.{Signature, Signer, given_VirtualExecutionContext}
import upickle.default.{ReadWriter, read, write}

import java.security.MessageDigest
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.control.NonFatal

class State(val targets: os.Path) {

  private val cache = TrieMap[os.RelPath, Future[Result[?]]]()

  def apply[A: ReadWriter](f: Context[A] ?=> Future[A]): Future[A] = {
    val ctx: Context[A] = Context(this, None, None)
    f(using ctx)
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
  def run_if_needed[A: ReadWriter](ctx: Context[A])(
      f: () => Future[A]
  ): Future[Result[A]] = ctx.target match {
    case None =>
      ???
    case Some(target) =>
      if (os.exists(ctx.dirty_path)) {
        config.trace_dirty(target)
        os.remove.all(ctx.target_path)
      }

      // (1) let's find out if we have a saved value
      val old_saved: Option[Saved[A]] = if (os.isFile(ctx.saved_path)) {
        try {
          val old = read[Saved[A]](os.read(ctx.saved_path))
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
            os.remove.all(ctx.saved_path)
            None
        }
      } else {
        None
      }

      old_saved match {
        case Some(old_saved) => Future.successful(old_saved.result)
        case None =>
          os.remove.all(ctx.target_path)
          os.write.over(ctx.dirty_path, "", createFolders = true)
          f().map { new_value =>
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
      ctx: Context[?],
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
