package ag.r2

import ag.common.{given_ReadWriter_RelPath, Signature}

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
  // The name of the definition that made us: our path, without the scope and
  // parameters that tell us apart from the other targets it makes
  val kind: os.RelPath
  val is_peek: Boolean
}

trait Target[A] extends TargetBase {
  outer =>

  // our compute logic (implemented by the value producer)
  // We could return a more general value because Target[A]
  // is co-variant in A
  def make(tracker: Tracker[A]): Result[A]

  def append(p: os.RelPath): Target[A] = new Target[A] {
    override val path: os.RelPath = outer.path / p
    override val kind: os.RelPath = outer.kind

    override def make(tracker: Tracker[A]): Result[A] =
      outer.make(tracker)

    override val is_peek: Boolean = outer.is_peek
  }

  def peek: Target[A] = if (is_peek) this
  else
    new Target[A] {
      override val path: os.RelPath = outer.path
      override val kind: os.RelPath = outer.kind

      override def make(tracker: Tracker[A]): Result[A] =
        outer.make(tracker)

      override val is_peek: Boolean = true
    }

}

// Many targets of one type, as a field of the named tuple given to target or
// Tracker.track_all. Its value is the targets' values, in order.
final class Many[A](val targets: Seq[Target[A]])

object Target {
  // the value of a Target or a Many
  type Value[T] = T match {
    case Many[a]   => Seq[a]
    case Target[a] => a
  }

  // the values of a named tuple of Targets and Manys, under the same names
  type Values[N <: Tuple, V <: Tuple] = NamedTuple[N, Tuple.Map[V, Value]]

  def apply[A](
      p: os.RelPath,
      k: os.RelPath
  )(f: Tracker[A] => Result[A]): Target[A] = new Target[A] { outer =>
    override val path: os.RelPath = p
    override val kind: os.RelPath = k
    override def make(tracker: Tracker[A]): Result[A] =
      f(tracker)
    override val is_peek: Boolean = false
  }
}
