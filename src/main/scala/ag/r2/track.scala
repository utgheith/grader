package ag.r2

import ag.common.given_ReadWriter_RelPath
import ag.common.given_ReadWriter_SortedMap
import ag.rules.Signature

import scala.concurrent.Future
import upickle.default.{ReadWriter, macroRW, read}

import scala.caps.Capability
import scala.collection.SortedMap
import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

trait Target[+A: ReadWriter] {
  val path: os.RelPath
  def make(using Context[?]): () => Future[A]
  def track(using ctx: Context[?]): Future[A] = {
    ctx.state.track(this)
  } 
  
}

trait Context[+A] extends Capability {
  val state: State
  val target: Target[A]
  private val added_dependencies = TrieMap[os.RelPath, Signature]()
  def add_dependency(target: Target[?], res: Result[?]): Unit = {
    added_dependencies.update(target.path, res.signature)
  }
  def get_dependencies(): SortedMap[os.RelPath, Signature] = added_dependencies.to(SortedMap)
}

object Context {
  def apply(using Context[?]): Context[?] = summon[Context[?]]
}

case class Result[A](value: A, signature: Signature)
object Result {
  given [A: ReadWriter] => ReadWriter[Result[A]] = macroRW
}

case class Saved[A](result: Result[A], depends_on: SortedMap[os.RelPath, Signature])
object Saved {
  given [A: ReadWriter] => ReadWriter[Saved[A]] = macroRW
}

class State(targets: os.Path) {
  private val cache = TrieMap[os.RelPath, Result[?]]()
  
  def track[A: ReadWriter](target: Target[A])(using ctx: Context[?]): Future[A] = { 
      val res = cache.getOrElseUpdate (target.path, {
        config.on_miss(target)
        // evaluate dependencies, stored in ctx, returns a function that computes A (if needed)
        val compute_a_if_needed: () => Future[A] = target.make
        
        // do we have an old saved value?
        val target_path = targets / target.path
        val old_but_valid_saved_state: Option[Saved[A]] = if (os.isFile(target_path)) {
          try {
            // read old state
            val old_state = read[Saved[A]](os.read(target_path))
            val old_dependencies = old_state.depends_on
            val new_dependencies = ctx.get_dependencies()
            
            if (!new_dependencies.keySet.subsetOf(old_dependencies.keySet)) {
              None
            } else if (!new_dependencies.forall { case (p,s) =>
              old_dependencies.get(p) == s
            }) {
              None
            } else {
              Some(old_state)
            }
          } catch {
            case NonFatal(e) =>
              config.on_read_error(target, e)
              None
          }
        } else {
          None
        }
        
        old_but_valid_saved_state match {
          case Some(s) => s.result
        }
      })
      ctx.add_dependency(target, res)
      res.value.asInstanceOf[Future[A]]
  }

}

