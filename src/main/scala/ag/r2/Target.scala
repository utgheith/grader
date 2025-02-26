package ag.r2

import scala.concurrent.Future
import upickle.default.ReadWriter


case class WithData[A](value: A, target_name: os.RelPath) {
  def get_data_path(using ctx: Context): os.Path = TargetBase(target_name).data_path
}

object WithData {
  given [A: ReadWriter] => ReadWriter[WithData[A]] = ReadWriter.derived[WithData[A]]
}

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

trait TargetBase {
  // Our unique name
  val path: os.RelPath

  def data_path(using ctx: Consumer): os.Path = ctx.state.data_path(this)
}

object TargetBase {
  def apply(p: os.RelPath): TargetBase = new TargetBase {
    override val path: os.RelPath = p
  }
}

trait Target[A: ReadWriter] extends TargetBase { outer =>

  // our compute logic (implemented by the value producer)
  // We could return a more general value because Target[A]
  // is co-variant in A
  def make(ctx: Context): Future[Result[A]]

  // returns our current value (used by the value consumer)
  def track(using ctx: Context): Future[A] = {
    ctx.state.track(ctx, this)
  }
  
  def append(p: os.RelPath): Target[A] = new Target[A] {
    override val path: os.RelPath = outer.path / p
    override def make(c: Consumer): Future[Result[A]] = outer.make(c)
  }
  
}

object Target {
  def apply[A: ReadWriter](
      p: os.RelPath
  )(f: Producer[A] => Future[Result[A]]): Target[A] = new Target[A] {
    override val path: os.RelPath = p
    override def make(old_ctx: Consumer): Future[Result[A]] =
      f(
        Producer[A](
          state = old_ctx.state,
          target = this,
          parent = old_ctx
        )
      )
  }
}
