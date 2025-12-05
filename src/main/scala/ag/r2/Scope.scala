package ag.r2

import upickle.default.ReadWriter

class Scope(base_ : os.RelPath | String | Scope) { self =>

  val base: os.RelPath = base_ match {
    case r: os.RelPath => r
    case s: String     => ToRelPath(s)
    case s: Scope      => ToRelPath(s)
  }
  def /[A: ToRelPath](more: A): Scope = new Scope(base / ToRelPath(more))

  // fun { (a: A) => target { ... } } same as target { ... } but appends "/a" to the target path
  def fun[E <: Exception, A: ToRelPath, Out](
      f: A => Target[Out]
  ): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
  def fun[E <: Exception, A: ToRelPath, B: ToRelPath, Out](
      f: (A, B) => Target[Out]
  ): (A, B) => Target[Out] = { (a: A, b: B) =>
    f(a, b).append(ToRelPath(a) / ToRelPath(b))
  }
  def fun[E <: Exception, A: ToRelPath, B: ToRelPath, C: ToRelPath, Out](
      f: (A, B, C) => Target[Out]
  ): (A, B, C) => Target[Out] = { (a: A, b: B, c: C) =>
    f(a, b, c).append(ToRelPath(a) / ToRelPath(b) / ToRelPath(c))
  }
  def fun[
      Ex <: Exception,
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

  def target[Out: ReadWriter]()(
      f: Tracker ?=> Out | Result[Out]
  )(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    f match {
      case r: Result[?] => r.asInstanceOf[Result[Out]]
      case o            => run_if_needed(o.asInstanceOf[Out])
    }
  }

  def target[A, Out: ReadWriter](
      a: Target[A]
  )(f: Producer[Out] ?=> A => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    run_if_needed {
      f(ta.join)
    }
  }

  def target[A, B, Out: ReadWriter](
      a: Target[A],
      b: Target[B]
  )(f: Producer[Out] ?=> (A, B) => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    run_if_needed {
      f(ta.join, tb.join)
    }
  }

  def target[A, B, C, Out: ReadWriter](
      a: Target[A],
      b: Target[B],
      c: Target[C]
  )(f: Producer[Out] ?=> (A, B, C) => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    run_if_needed {
      f(ta.join, tb.join, tc.join)
    }
  }

  def target[A, B, C, D, Out: ReadWriter](
      a: Target[A],
      b: Target[B],
      c: Target[C],
      d: Target[D]
  )(f: Producer[Out] ?=> (A, B, C, D) => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    run_if_needed {
      f(ta.join, tb.join, tc.join, td.join)
    }
  }

  def target[A, B, C, D, E, Out: ReadWriter](
      a: Target[A],
      b: Target[B],
      c: Target[C],
      d: Target[D],
      e: Target[E]
  )(f: Producer[Out] ?=> (A, B, C, D, E) => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    run_if_needed {
      f(ta.join, tb.join, tc.join, td.join, te.join)
    }
  }

  def target[A, B, C, D, E, F, Out: ReadWriter](
      a: Target[A],
      b: Target[B],
      c: Target[C],
      d: Target[D],
      e: Target[E],
      f: Target[F]
  )(func: Producer[Out] ?=> (A, B, C, D, E, F) => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    val tf = f.track
    run_if_needed {
      func(ta.join, tb.join, tc.join, td.join, te.join, tf.join)
    }
  }

  def target[A, B, C, D, E, F, G, Out: ReadWriter](
      a: Target[A],
      b: Target[B],
      c: Target[C],
      d: Target[D],
      e: Target[E],
      f: Target[F],
      g: Target[G]
  )(func: Producer[Out] ?=> (A, B, C, D, E, F, G) => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    val tf = f.track
    val tg = g.track
    run_if_needed {
      func(ta.join, tb.join, tc.join, td.join, te.join, tf.join, tg.join)
    }
  }

  def target[A, B, C, D, E, F, G, H, Out: ReadWriter](
      a: Target[A],
      b: Target[B],
      c: Target[C],
      d: Target[D],
      e: Target[E],
      f: Target[F],
      g: Target[G],
      h: Target[H]
  )(func: Producer[Out] ?=> (A, B, C, D, E, F, G, H) => Out)(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    val tf = f.track
    val tg = g.track
    val th = h.track
    run_if_needed {
      func(
        ta.join,
        tb.join,
        tc.join,
        td.join,
        te.join,
        tf.join,
        tg.join,
        th.join
      )
    }
  }
}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))

  given ToRelPath[Scope] = { s => s.base }
}
