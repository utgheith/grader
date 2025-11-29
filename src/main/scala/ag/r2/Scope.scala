package ag.r2

import language.experimental.saferExceptions

import ag.common.Fork
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
      f: A => Target[E, Out]
  ): A => Target[E, Out] = { (a: A) => f(a).append(ToRelPath(a)) }
  def fun[E <: Exception, A: ToRelPath, B: ToRelPath, Out](
      f: (A, B) => Target[E, Out]
  ): (A, B) => Target[E, Out] = { (a: A, b: B) =>
    f(a, b).append(ToRelPath(a) / ToRelPath(b))
  }
  def fun[E <: Exception, A: ToRelPath, B: ToRelPath, C: ToRelPath, Out](
      f: (A, B, C) => Target[E, Out]
  ): (A, B, C) => Target[E, Out] = { (a: A, b: B, c: C) =>
    f(a, b, c).append(ToRelPath(a) / ToRelPath(b) / ToRelPath(c))
  }
  def fun[
      Ex <: Exception,
      A: ToRelPath,
      B: ToRelPath,
      C: ToRelPath,
      D: ToRelPath,
      Out
  ](f: (A, B, C, D) => Target[Ex, Out]): (A, B, C, D) => Target[Ex, Out] = {
    (a: A, b: B, c: C, d: D) =>
      f(a, b, c, d).append(
        ToRelPath(a) / ToRelPath(b) / ToRelPath(c) / ToRelPath(d)
      )
  }

  def target[Ex <: Exception, Out: ReadWriter]()(
      f: Tracker[Ex, Out] ?=> Out | Result[Out] throws Ex
  )(using
      fn: sourcecode.FullName
  ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    f match {
      case r : Result[Out] => r
      case o : Out => run_if_needed(o)
    }
  }

  def target[Ex <: Exception, A, Out: ReadWriter](
      a: Target[Ex, A]
  )(f: Producer[Ex, Out] ?=> A => Out throws Ex)(using
      fn: sourcecode.FullName
  ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    run_if_needed {
      //summon[Producer[Ex, Out]]
      f(ta.join)
    }
  }

  def target[Ex <: Exception, A, B, Out: ReadWriter](
      a: Target[Ex, A],
      b: Target[Ex, B]
  )(f: Producer[Ex, Out] ?=> (A, B) => Out throws Ex)(using
      fn: sourcecode.FullName
  ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    run_if_needed {
      summon[Producer[Ex, Out]]
      f(ta.join, tb.join)
    }
  }

  def target[Ex <: Exception, A, B, C, Out: ReadWriter](
      a: Target[Ex, A],
      b: Target[Ex, B],
      c: Target[Ex, C]
  )(f: Producer[Ex, Out] ?=> (A, B, C) => Out throws Ex)(using
      fn: sourcecode.FullName
  ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    run_if_needed {
      summon[Producer[Ex, Out]]
      f(ta.join, tb.join, tc.join)
    }
  }

  def target[Ex <: Exception, A, B, C, D, Out: ReadWriter](
      a: Target[Ex, A],
      b: Target[Ex, B],
      c: Target[Ex, C],
      d: Target[Ex, D]
  )(f: Producer[Ex, Out] ?=> (A, B, C, D) => Out throws Ex)(using
      fn: sourcecode.FullName
  ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    run_if_needed {
      summon[Producer[Ex, Out]]
      f(ta.join, tb.join, tc.join, td.join)
    }
  }

  def target[Ex <: Exception, A, B, C, D, E, Out: ReadWriter](
      a: Target[Ex, A],
      b: Target[Ex, B],
      c: Target[Ex, C],
      d: Target[Ex, D],
      e: Target[Ex, E]
  )(f: Producer[Ex, Out] ?=> (A, B, C, D, E) => Out throws Ex)(using
      fn: sourcecode.FullName
  ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    run_if_needed {
      summon[Producer[Ex, Out]]
      f(ta.join, tb.join, tc.join, td.join, te.join)
    }
  }

  def target[Ex <: Exception, A, B, C, D, E, F, Out: ReadWriter](
      a: Target[Ex, A],
      b: Target[Ex, B],
      c: Target[Ex, C],
      d: Target[Ex, D],
      e: Target[Ex, E],
      f: Target[Ex, F]
  )(body: Producer[Ex, Out] ?=> (A, B, C, D, E, F) => Out throws Ex)(using
      fn: sourcecode.FullName
  ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    val tf = f.track
    run_if_needed {
      summon[Producer[Ex, Out]]
      body(ta.join, tb.join, tc.join, td.join, te.join, tf.join)
    }
  }

  def target[Ex <: Exception, A, B, C, D, E, F, G, Out: ReadWriter](
                                                                  a: Target[Ex, A],
                                                                  b: Target[Ex, B],
                                                                  c: Target[Ex, C],
                                                                  d: Target[Ex, D],
                                                                  e: Target[Ex, E],
                                                                  f: Target[Ex, F],
                                                                  g: Target[Ex, G],
                                                                )(body: Producer[Ex, Out] ?=> (A, B, C, D, E, F, G) => Out throws Ex)(using
                                                                                                                                   fn: sourcecode.FullName
                                                                ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    val tf = f.track
    val tg = g.track
    run_if_needed {
      summon[Producer[Ex, Out]]
      body(ta.join, tb.join, tc.join, td.join, te.join, tf.join, tg.join)
    }
  }

  def target[Ex <: Exception, A, B, C, D, E, F, G, H, Out: ReadWriter](
                                                                     a: Target[Ex, A],
                                                                     b: Target[Ex, B],
                                                                     c: Target[Ex, C],
                                                                     d: Target[Ex, D],
                                                                     e: Target[Ex, E],
                                                                     f: Target[Ex, F],
                                                                     g: Target[Ex, G],
                                                                     h: Target[Ex, H]
                                                                   )(body: Producer[Ex, Out] ?=> (A, B, C, D, E, F, G, H) => Out throws Ex)(using
                                                                                                                                         fn: sourcecode.FullName
                                                                   ): Target[Ex, Out] = Target(ToRelPath(fn) / base) {
    val ta = a.track
    val tb = b.track
    val tc = c.track
    val td = d.track
    val te = e.track
    val tf = f.track
    val tg = g.track
    val th = h.track
    run_if_needed {
      summon[Producer[Ex, Out]]
      body(ta.join, tb.join, tc.join, td.join, te.join, tf.join, tg.join, th.join)
    }
  }
}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))

  given ToRelPath[Scope] = { s => s.base }
}
