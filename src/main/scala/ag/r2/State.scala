package ag.r2

import ag.common.given_VirtualExecutionContext
import ag.rules.Signature
import upickle.default.{read, ReadWriter, write}

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.control.NonFatal

class State(val targets: os.Path) {
  
  private val cache = TrieMap[os.RelPath, Future[Result[?]]]()
  
  
  def apply[A: ReadWriter](f: Context[A] ?=> Future[A]): Future[A] = {
    given ctx: Context[A] = Context(this, None, None)
    Target(os.RelPath("$")) {
      run_if_needed { () => f }
    }.track
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
  def run_if_needed[A: ReadWriter](f: () => Future[A])(using ctx: Context[?]): Future[Result[A]] = ctx.target match {
    case None =>
      ???
    case Some(target) =>
      val target_path = targets / target.path

      // (1) let's find out if we have a saved value
      val old_saved: Option[Saved[A]] = if (os.isFile(target_path)) {
        try {
          val old = read[Saved[A]](os.read(target_path))
          // (2) we seem to have one, check if the dependencies changed
          // ctx has the newly discovered dependencies
          // old has the dependency at the time the value was saved
          if (ctx.dependencies.forall((p, s) => old.depends_on.get(p) == s)) {
            Some(old)
          } else {
            None
          }
        } catch {
          case NonFatal(e) =>
            config.on_read_error(target, e)
            os.remove.all(target_path)
            None
        }
      } else {
        None
      }

      old_saved match {
        case Some(old_saved) => Future.successful(old_saved.result)
        case None =>
          f().map { new_value =>
            val new_result = Result(new_value, Signature.of(new_value))
            val new_saved = Saved(new_result, ctx.dependencies)
            os.write.over(target_path, write(new_saved, indent = 2))
            new_result
          }

      }
  }

  def track[A: ReadWriter](target: Target[A])(using ctx: Context[?]): Future[A] = {
    val result: Future[Result[?]] = cache.getOrElseUpdate(target.path, {
      config.on_miss(target)
      target.make
    })
      
    ctx.add_dependency(target, result)
    
    for {
      r <- result
    } yield r.value.asInstanceOf[A]
    
  }
    

}
