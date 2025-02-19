package ag.r2

import ag.common.given_VirtualExecutionContext
import ag.rules.Signature
import upickle.default.{read, ReadWriter, write}

import scala.collection.SortedMap
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.control.NonFatal

class State(val targets: os.Path) {
  
  private val cache = TrieMap[os.RelPath, Future[Saved[?]]]()
  
  
  def apply[A: ReadWriter](f: Context[A] ?=> Future[A]): Future[A] = {
    given ctx: Context[A] = Context(this, Target(os.RelPath("$")) {
      run_if_needed(f) }, None)
    ctx.target.track
  }

  def track[A: ReadWriter](target: Target[A])(using ctx: Context[?]): Future[A] = {
    val saved = cache.getOrElseUpdate (target.path, {
      config.on_miss(target)

      val target_path = targets / target.path
      val old_saved: Option[Saved[A]] = if (os.isFile(target_path)) {
        try {
          Some(read[Saved[A]](os.read(target_path)))
        } catch {
          case NonFatal(e) =>
            config.on_read_error(target, e)
            os.remove.all(target_path)
            None
        }
      } else {
        None
      }

      val saved: Future[Saved[A]] =
        for {
          // run make(Phase I) to generate the new dependencies
          made: Made[A] <- target.make
          new_dependencies: SortedMap[os.RelPath, Signature] = made.dependencies

          // decide if we want to use the old saved value or generate a new one
          result: Saved[A] <- old_saved match {
            case Some(old_saved) if new_dependencies.forall((p, s) => old_saved.depends_on.get(p) == s) =>
              // we have a saved value and dependencies are unchanged, keep what we have
              Future.successful(old_saved)
            case _ =>
              // we either don't have an old saved value or we have one and dependencies changed, run
              // phase II
              made.result.get().map { new_result =>
                val s = Saved(new_result, new_dependencies)
                os.write.over(target_path, write(s, indent = 2), createFolders = true)
                s
              }
          }
        } yield {
          result
        }
      saved
    })

    ctx.add_dependency(target, saved)
    saved.map { s => s.result.value.asInstanceOf[A] }
  }

}
