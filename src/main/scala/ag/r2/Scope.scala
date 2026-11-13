package ag.r2

import ag.common.ToRelPath

import upickle.default.ReadWriter

import scala.NamedTuple.NamedTuple

class Scope(base_ : os.RelPath | String | Scope) { self =>

  val base: os.RelPath = base_ match {
    case r: os.RelPath => r
    case s: String     => ToRelPath(s)
    case s: Scope      => ToRelPath(s)
  }
  def /[A: ToRelPath](more: A): Scope = new Scope(base / ToRelPath(more))

  // val x = target((a = ta, b = tb)) { (producer, deps) => ... deps.a ... }
  // One dependency is (a = ta), none is EmptyTuple, and Many(ts) is a Seq of
  // them. They're all read at once, before run_if_needed.
  def target[N <: Tuple, V <: Tuple, Out: ReadWriter](
      targets: NamedTuple[N, V]
  )(
      f: (Producer[?], Target.Values[N, V]) => Out
  )(using
      only_targets: Tuple.Union[V] <:< (Target[?] | Many[?]),
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base, ToRelPath(fn)) { tracker =>
    val values = tracker.track_all(targets)
    run_if_needed(tracker)(producer => f(producer, values))
  }

  // fun { (a: A) => target { ... } } same as target { ... } but appends "/a" to the target path
  def fun[A: ToRelPath, Out](
      f: A => Target[Out]
  ): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
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
  def fun[
      A: ToRelPath,
      B: ToRelPath,
      C: ToRelPath,
      D: ToRelPath,
      Out
  ](f: (A, B, C, D) => Target[Out]): (A, B, C, D) => Target[Out] = {
    (a: A, b: B, c: C, d: D) =>
      f(a, b, c, d).append(
        ToRelPath(a) / ToRelPath(b) / ToRelPath(c) / ToRelPath(d)
      )
  }

  def complex_target[Out](f: Tracker[Out] => Result[Out])(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base, ToRelPath(fn))(f)
}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))

  given ToRelPath[Scope] = { s => s.base }
}
