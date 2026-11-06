package ag.r2

import ag.common.{given_ReadWriter_RelPath, Signature}
import ag.task.Task

import upickle.default.ReadWriter

import scala.NamedTuple.NamedTuple

@upickle.implicits.allowUnknownKeys(false)
case class WithData[A](
    value: A,
    target_name: os.RelPath,
    data_signature: Signature
) {
  def get_data_path(ctx: Context[?]): os.Path =
    ctx.state.data_path(target_name)
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

trait Target[A] extends TargetBase {
  outer =>

  // our compute logic (implemented by the value producer)
  // We could return a more general value because Target[A]
  // is co-variant in A
  def make(tracker: Tracker[A]): Task[Result[A]]

  def append(p: os.RelPath): Target[A] = new Target[A] {
    override val path: os.RelPath = outer.path / p

    override def make(tracker: Tracker[A]): Task[Result[A]] =
      outer.make(tracker)

    override val is_peek: Boolean = outer.is_peek
  }

  def peek: Target[A] = if (is_peek) this
  else
    new Target[A] {
      override val path: os.RelPath = outer.path

      override def make(tracker: Tracker[A]): Task[Result[A]] =
        outer.make(tracker)

      override val is_peek: Boolean = true
    }

}

object Target {
  // the value of a Target
  type Value[T] = T match { case Target[a] => a }

  // the values of a named tuple of Targets, under the same names
  type Values[N <: Tuple, V <: Tuple] = NamedTuple[N, Tuple.Map[V, Value]]

  def apply[A](
      p: os.RelPath
  )(f: Tracker[A] => Task[Result[A]]): Target[A] = new Target[A] { outer =>
    override val path: os.RelPath = p
    override def make(tracker: Tracker[A]): Task[Result[A]] =
      f(tracker)
    override val is_peek: Boolean = false
  }
}
