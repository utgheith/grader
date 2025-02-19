package ag.r2

import ag.common.Lazy
import ag.common.given_VirtualExecutionContext
import ag.rules.Signature

import scala.concurrent.Future
import upickle.default.ReadWriter

//
// A target describes a named, canonicalized, persistent, lazy computation that could be tracked
//    - named -> uniquely identified by an os.RelPath
//    - lazy -> evaluated on first use (see tracked below for more constraints)
//    - canonicalized -> there is a 1-to-1 mapping from names to values
//    - persistent -> the canonical mapping survives across program restarts
//    - tracked -> a value is recomputed iff:
//            * it has never been computed before
//            * one of its dependencies is recomputed
//
trait Target[+A: ReadWriter] {
  // our unique name
  val path: os.RelPath

  // our compute logic (implemented by the value producer)
  // We could return a more general value because Target[A]
  // is co-variant in A
  def make[B >: A](using Context[?]): Future[Made[B]]

  // returns our current value (used by the value consumer)
  def track(using ctx: Context[?]): Future[A] = {
    ctx.state.track(this)
  }
}

object Target {
  def apply[A: ReadWriter](p: os.RelPath)(f: Context[A] ?=> Lazy[Future[A]]): Target[A] = new Target[A] {
    override val path: os.RelPath = p
    override def make[B >: A](using old_ctx: Context[?]): Future[Made[B]] = Future {
      given new_ctx: Context[A] = Context[A](state=old_ctx.state, target=this, parent=Some(old_ctx))
      val phase2: Lazy[Future[A]] = f(using new_ctx)
      
      Made(dependencies = new_ctx.dependencies,result = Lazy { () =>
        phase2.get().map(v => Result(v, Signature.of(v)))
      })
    }
  }
}
