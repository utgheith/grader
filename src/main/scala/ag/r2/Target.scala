package ag.r2

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
  def make[B >: A](using Context[?]): Future[Result[B]]

  // returns our current value (used by the value consumer)
  def track(using ctx: Context[?]): Future[A] = {
    ctx.state.track(this)
  }
}

object Target {
  def apply[A: ReadWriter](p: os.RelPath)(f: Context[A] ?=> Future[Result[A]]): Target[A] = new Target[A] {
    override val path: os.RelPath = p
    override def make[B >: A](using old_ctx: Context[?]): Future[Result[B]] =
      f(using Context[A](state=old_ctx.state, target=Some(this), parent=Some(old_ctx)))
  }
}
