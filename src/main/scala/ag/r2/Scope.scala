package ag.r2

import upickle.default.ReadWriter

import scala.concurrent.Future

class Scope(base_ : os.RelPath | String | Scope) { self =>
  
  val base: os.RelPath = base_ match {
    case r: os.RelPath => r
    case s: String => ToRelPath(s)
    case s: Scope => ToRelPath(s)
  }
  def / [A: ToRelPath](more: A): Scope = new Scope(base / ToRelPath(more))
  
  def target[Out: ReadWriter]()(f: Producer[Out] ?=> Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    run_if_needed {
      Future.successful(f)
    }
  }

  // val x = target(ta) { va => ... }
  def target[A, Out: ReadWriter](ta: Target[A])(f: Producer[Out] ?=> A => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base 
  ) {
    val fa = ta.track
    run_if_needed {
      for {
        a <- fa
      } yield f(a)
    }
  }

  // val ta: Target[A]
  // val tb: Target[B]
  // val x = target(ta, tb) { (va, vb) => ... }
  def target[A, B, Out: ReadWriter](ta: Target[A], tb: Target[B])(f: Producer[Out] ?=> (A, B) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    run_if_needed {
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
    }
  }
  
  // val x = target(ta, tb, tc) { (va, vb, vc) => ... }
  def target[A, B, C, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C])(f: Producer[Out] ?=> (A, B, C) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    run_if_needed {
      for {
        a <- fa
        b <- fb
        c <- fc
      } yield f(a, b, c)
    }
  }

  // val x = target(ta, tb, tc, td) { (va, vb, vc, vd) => ... }
  def target[A, B, C, D, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C], td: Target[D])(f: Producer[Out] ?=> (A, B, C, D) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    run_if_needed {
      for {
        a <- fa
        b <- fb
        c <- fc
        d <- fd
      } yield f(a, b, c, d)
    }
  }

  // val x = target(ta, tb, tc, td, te) { (va, vb, vc, vd, ve) => ... }
  def target[A, B, C, D, E, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C], td: Target[D], te: Target[E])(f: Producer[Out] ?=> (A, B, C, D, E) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    val fe = te.track
    run_if_needed {
      for {
        a <- fa
        b <- fb
        c <- fc
        d <- fd
        e <- fe
      } yield f(a, b, c, d, e)
    }
  }

  // val x = target(ta, tb, tc, td, te, tf) { (va, vb, vc, vd, ve, vf) => ... }
  def target[A, B, C, D, E, F, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C], td: Target[D], te: Target[E], tf: Target[F])(f: Producer[Out] ?=> (A, B, C, D, E, F) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    val fe = te.track
    val ff = tf.track
    run_if_needed {
      for {
        va <- fa
        vb <- fb
        vc <- fc
        vd <- fd
        ve <- fe
        vf <- ff
      } yield f(va, vb, vc, vd, ve, vf)
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg) { (va, vb, vc, vd, ve, vf, vg) => ... }
  def target[A, B, C, D, E, F, G, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C], td: Target[D], te: Target[E], tf: Target[F], tg: Target[G])(f: Producer[Out] ?=> (A, B, C, D, E, F, G) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    val fe = te.track
    val ff = tf.track
    val fg = tg.track
    run_if_needed {
      for {
        va <- fa
        vb <- fb
        vc <- fc
        vd <- fd
        ve <- fe
        vf <- ff
        vg <- fg
      } yield f(va, vb, vc, vd, ve, vf, vg)
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th) { (va, vb, vc, vd, ve, vf, vg, vh) => ... }
  def target[A, B, C, D, E, F, G, H, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C], td: Target[D], te: Target[E], tf: Target[F], tg: Target[G], th: Target[H])(f: Producer[Out] ?=> (A, B, C, D, E, F, G, H) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    val fe = te.track
    val ff = tf.track
    val fg = tg.track
    val fh = th.track
    run_if_needed {
      for {
        va <- fa
        vb <- fb
        vc <- fc
        vd <- fd
        ve <- fe
        vf <- ff
        vg <- fg
        vh <- fh
      } yield f(va, vb, vc, vd, ve, vf, vg, vh)
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti) { (va, vb, vc, vd, ve, vf, vg, vh, vi) => ... }
  def target[A, B, C, D, E, F, G, H, I, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C], td: Target[D], te: Target[E], tf: Target[F], tg: Target[G], th: Target[H], ti: Target[I])(f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    val fe = te.track
    val ff = tf.track
    val fg = tg.track
    val fh = th.track
    val fi = ti.track
    run_if_needed {
      for {
        va <- fa
        vb <- fb
        vc <- fc
        vd <- fd
        ve <- fe
        vf <- ff
        vg <- fg
        vh <- fh
        vi <- fi
      } yield f(va, vb, vc, vd, ve, vf, vg, vh, vi)
    }
  }


  // fun { (a: A) => target { ... } } same as target { ... } but appends "/a" to the target path
  def fun[A: ToRelPath, Out: ReadWriter](f:  A => Target[Out]): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
  def fun[A: ToRelPath, B: ToRelPath, Out: ReadWriter](f:  (A,B) => Target[Out]): (A,B) => Target[Out] = { (a: A, b: B) => f(a,b).append(ToRelPath(a) / ToRelPath(b)) }
  def fun[A: ToRelPath, B: ToRelPath, C: ToRelPath, Out: ReadWriter](f: (A, B, C) => Target[Out]): (A, B, C) => Target[Out] = { (a: A, b: B, c: C) => f(a, b, c).append(ToRelPath(a) / ToRelPath(b) / ToRelPath(c)) }
  def fun[A: ToRelPath, B: ToRelPath, C: ToRelPath, D: ToRelPath, Out: ReadWriter](f: (A, B, C, D) => Target[Out]): (A, B, C, D) => Target[Out] = { (a: A, b: B, c: C, d: D) => f(a, b, c, d).append(ToRelPath(a) / ToRelPath(b) / ToRelPath(c) / ToRelPath(d)) }

  def complex_target[Out: ReadWriter](f: Tracker[Out] ?=> Future[Result[Out]])(using fn: sourcecode.FullName): Target[Out] = Target(ToRelPath(fn) / base){ f }
}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))
  
  given ToRelPath[Scope] = { s => s.base }
}
