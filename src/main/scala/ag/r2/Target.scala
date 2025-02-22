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
trait Target[A: ReadWriter] {
  // our unique name
  val path: os.RelPath

  // our compute logic (implemented by the value producer)
  // We could return a more general value because Target[A]
  // is co-variant in A
  def make(ctx: Context[?]): Future[Result[A]]

  // returns our current value (used by the value consumer)
  def track(ctx: Context[?]): Future[A] = {
    ctx.state.track(ctx, this)
  }
}

object Target {
  def apply[A: ReadWriter](
      p: os.RelPath
  )(f: Context[A] => Future[Result[A]]): Target[A] = new Target[A] {
    override val path: os.RelPath = p
    override def make(old_ctx: Context[?]): Future[Result[A]] =
      f(
        Context[A](
          state = old_ctx.state,
          target = Some(this),
          parent = Some(old_ctx)
        )
      )
  }
}

def target[Out: ReadWriter](
    f: Context[Out] => Future[Result[Out]]
)(using fn: sourcecode.FullName): Target[Out] = Target(
  ToRelPath(fn)
)(f)

def scoped_target[A: ToRelPath, Out: ReadWriter](
    f: (Context[Out], A) => Future[Result[Out]]
)(using fn: sourcecode.FullName): A => Target[Out] = { a =>
  Target[Out](
    ToRelPath(fn) / ToRelPath(a)
  ) { ctx => f(ctx, a) }
}

def scoped_target[A: ToRelPath, B: ToRelPath, Out: ReadWriter](
    f: (Context[Out], A, B) => Future[Result[Out]]
)(using fn: sourcecode.FullName): (A, B) => Target[Out] = { (a, b) =>
  Target[Out](
    ToRelPath(fn) / ToRelPath(a) / ToRelPath(b)
  ) { ctx => f(ctx, a, b) }
}
