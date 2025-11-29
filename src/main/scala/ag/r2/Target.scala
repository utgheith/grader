package ag.r2

import language.experimental.saferExceptions
import ag.common.{Fork, Signature, given_ReadWriter_RelPath}

import upickle.default.ReadWriter

case class WithData[A](
    value: A,
    target_name: os.RelPath,
    data_signature: Signature
) {
  def get_data_path(using p: Context[?, ?]): os.Path =
    p.state.data_path(target_name)
}

object WithData {
  given [A: ReadWriter] => ReadWriter[WithData[A]] =
    ReadWriter.derived[WithData[A]]
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
  val is_peek: Boolean
}

object TargetBase {
  def apply(p: os.RelPath, peek: Boolean): TargetBase = new TargetBase {
    override val path: os.RelPath = p
    override val is_peek: Boolean = peek
  }
}

trait Target[-E <: Exception, +A: ReadWriter] extends TargetBase {
  outer =>

  // our compute logic (implemented by the value producer)
  // We could return a more general value because Target[A]
  // is co-variant in A
  def make(using Tracker[E, A], CanThrow[E]): Result[A]

  // returns our current value (used by the value consumer)
  def track(using ctx: Tracker[E, ?]): Fork[E, A] = {
    ctx.state.track(this)
  }

  def guilty(using ctx: Tracker[E, ?]): A throws E = track.join

  def append(p: os.RelPath): Target[E, A] = new Target[E, A] {
    override val path: os.RelPath = outer.path / p

    override def make(using Tracker[E, A]): Result[A] throws E = outer.make

    override val is_peek: Boolean = outer.is_peek
  }

  def peek: Target[E, A] = if (is_peek) this
  else
    new Target[E, A] {
      override val path: os.RelPath = outer.path

      override def make(using Tracker[E, A]): Result[A] throws E = outer.make

      override val is_peek: Boolean = true
    }

}

object Target {
  def apply[E <: Exception, A: ReadWriter](
      p: os.RelPath
  )(f: Tracker[E, A] ?=> Result[A] throws E): Target[E, A] = new Target[E, A] {
    outer =>
    override val path: os.RelPath = p
    override def make(using Tracker[E, A]): Result[A] throws E =
      f
    override val is_peek: Boolean = false
  }
}
