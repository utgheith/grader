package ag.r3

import ag.common.ToRelPath
import upickle.default.ReadWriter

import scala.NamedTuple.NamedTuple

// Where targets are defined: a target's name is where its definition is in
// the code (sourcecode.FullName), under `base`, then `fun`'s arguments.
trait Scoped {
  def base: os.RelPath
  def /[A: ToRelPath](more: A): Scope = new Scope(base / ToRelPath(more))

  // val x = target((a = ta, b = tb)) { (producer, deps) => ... deps.a ... }
  def target[N <: Tuple, V <: Tuple, Out: ReadWriter](
      targets: NamedTuple[N, V]
  )(
      f: (Producer[?], Target.Values[N, V]) => Out
  )(using
      only_targets: Tuple.Union[V] <:< (Target[?] | Many[?]),
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base, ToRelPath(fn)) { tracker =>
    val values = tracker.track_all(targets)
    tracker.run_if_needed(producer => f(producer, values))
  }

  def fun[A: ToRelPath, Out](f: A => Target[Out]): A => Target[Out] = {
    (a: A) => f(a).append(ToRelPath(a))
  }
  def fun[A: ToRelPath, B: ToRelPath, Out](
      f: (A, B) => Target[Out]
  ): (A, B) => Target[Out] = { (a: A, b: B) =>
    f(a, b).append(ToRelPath(a) / ToRelPath(b))
  }
  def fun[A: ToRelPath, B: ToRelPath, C: ToRelPath, Out](
      f: (A, B, C) => Target[Out]
  ): (A, B, C) => Target[Out] = { (a: A, b: B, c: C) =>
    f(a, b, c).append(ToRelPath(a) / ToRelPath(b) / ToRelPath(c))
  }
  def fun[A: ToRelPath, B: ToRelPath, C: ToRelPath, D: ToRelPath, Out](
      f: (A, B, C, D) => Target[Out]
  ): (A, B, C, D) => Target[Out] = { (a: A, b: B, c: C, d: D) =>
    f(a, b, c, d).append(
      ToRelPath(a) / ToRelPath(b) / ToRelPath(c) / ToRelPath(d)
    )
  }

  def fun[
      A: ToRelPath,
      B: ToRelPath,
      C: ToRelPath,
      D: ToRelPath,
      E: ToRelPath,
      Out
  ](
      f: (A, B, C, D, E) => Target[Out]
  ): (A, B, C, D, E) => Target[Out] = { (a: A, b: B, c: C, d: D, e: E) =>
    f(a, b, c, d, e).append(
      ToRelPath(a) / ToRelPath(b) / ToRelPath(c) / ToRelPath(d) / ToRelPath(e)
    )
  }

  def complex_target[Out](f: Tracker[Out] => Result[Out])(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base, ToRelPath(fn))(f)
}

// Where targets are defined; as in ag.r2.
class Scope(base_ : os.RelPath | String | Scope) extends Scoped {
  val base: os.RelPath = base_ match {
    case r: os.RelPath => r
    case s: String     => ToRelPath(s)
    case s: Scope      => s.base
  }
}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))
}
