package ag.r2

import ag.common.Signature

import scala.concurrent.Future
import upickle.default.ReadWriter


case class WithData[A](value: A, target_name: os.RelPath, data_signature: Signature) {
  def get_data_path(using p: Producer[?]): os.Path = p.state.data_path(TargetBase(target_name))
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

  //def data_path(using ctx: Tracker[?]): os.Path = ctx.state.data_path(this)
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
  def make(using Tracker[A]): Future[Result[A]]

  // returns our current value (used by the value consumer)
  def track(using ctx: Tracker[?]): Future[A] = {
    ctx.state.track(this)
  }
  
  def append(p: os.RelPath): Target[A] = new Target[A] {
    override val path: os.RelPath = outer.path / p
    override def make(using Tracker[A]): Future[Result[A]] = outer.make
  }
  
  def peek: Target[A] = ???
  
}

object Target {
  def apply[A: ReadWriter](
      p: os.RelPath
  )(f: Tracker[A] ?=> Future[Result[A]]): Target[A] = new Target[A] { outer =>
    override val path: os.RelPath = p
    override def make(using Tracker[A]): Future[Result[A]] =
      f
  }
}
