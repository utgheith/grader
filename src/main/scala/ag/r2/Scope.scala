package ag.r2

import upickle.default.ReadWriter

import scala.concurrent.Future
import scala.reflect.ClassTag

class Scope(base_ : os.RelPath | String | Scope) { self =>

  val base: os.RelPath = base_ match {
    case r: os.RelPath => r
    case s: String     => ToRelPath(s)
    case s: Scope      => ToRelPath(s)
  }
  def /[A: ToRelPath](more: A): Scope = new Scope(base / ToRelPath(more))

  def target[Out: {ClassTag, ReadWriter}]()(
      f: Producer[Out] ?=> Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    run_if_needed {
      f
    }
  }

  // val x = target(ta) { va => ... }
  def target[A, Out: {ClassTag, ReadWriter}](ta: Target[A])(
      f: Producer[Out] ?=> A => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    run_if_needed {
      for {
        a <- fa
        out <- force_future(f(a))
      } yield out
    }
  }

  // val ta: Target[A]
  // val tb: Target[B]
  // val x = target(ta, tb) { (va, vb) => ... }
  def target[A, B, Out: {ClassTag, ReadWriter}](ta: Target[A], tb: Target[B])(
      f: Producer[Out] ?=> (A, B) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    run_if_needed {
      for {
        a <- fa
        b <- fb
        out <- force_future(f(a, b))
      } yield out
    }
  }

  // val x = target(ta, tb, tc) { (va, vb, vc) => ... }
  def target[A, B, C, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C]
  )(
      f: Producer[Out] ?=> (A, B, C) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
        out <- force_future(f(a, b, c))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td) { (va, vb, vc, vd) => ... }
  def target[A, B, C, D, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D]
  )(
      f: Producer[Out] ?=> (A, B, C, D) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
        out <- force_future(f(a, b, c, d))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te) { (va, vb, vc, vd, ve) => ... }
  def target[A, B, C, D, E, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
        out <- force_future(f(a, b, c, d, e))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te, tf) { (va, vb, vc, vd, ve, vf) => ... }
  def target[A, B, C, D, E, F, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
        out <- force_future(f(va, vb, vc, vd, ve, vf))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg) { (va, vb, vc, vd, ve, vf, vg) => ... }
  def target[A, B, C, D, E, F, G, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
        out <- force_future(f(va, vb, vc, vd, ve, vf, vg))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th) { (va, vb, vc, vd, ve, vf, vg, vh) => ... }
  def target[A, B, C, D, E, F, G, H, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G],
      th: Target[H]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
        out <- force_future(f(va, vb, vc, vd, ve, vf, vg, vh))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti) { (va, vb, vc, vd, ve, vf, vg, vh, vi) => ... }
  def target[A, B, C, D, E, F, G, H, I, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G],
      th: Target[H],
      ti: Target[I]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
        out <- force_future(f(va, vb, vc, vd, ve, vf, vg, vh, vi))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj) => ... }
  def target[A, B, C, D, E, F, G, H, I, J, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G],
      th: Target[H],
      ti: Target[I],
      tj: Target[J]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I, J) => Out | Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
    val fj = tj.track
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
        vj <- fj
        out <- force_future(f(va, vb, vc, vd, ve, vf, vg, vh, vi, vj))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj, tk) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk) => ... }
  def target[A, B, C, D, E, F, G, H, I, J, K, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G],
      th: Target[H],
      ti: Target[I],
      tj: Target[J],
      tk: Target[K]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I, J, K) => Out
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
    val fj = tj.track
    val fk = tk.track
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
        vj <- fj
        vk <- fk
        out <- force_future(f(va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk))
      } yield out
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj, tk) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk) => ... }
  def target[A, B, C, D, E, F, G, H, I, J, K, L, Out: {ClassTag, ReadWriter}](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G],
      th: Target[H],
      ti: Target[I],
      tj: Target[J],
      tk: Target[K],
      tl: Target[L]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I, J, K, L) => Out |
        Future[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
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
    val fj = tj.track
    val fk = tk.track
    val fl = tl.track
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
        vj <- fj
        vk <- fk
        vl <- fl
        out <- force_future(f(va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk, vl))
      } yield out
    }
  }

  // fun { (a: A) => target { ... } } same as target { ... } but appends "/a" to the target path
  def fun[A: ToRelPath, Out: {ClassTag, ReadWriter}](
      f: A => Target[Out]
  ): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
  def fun[A: ToRelPath, B: ToRelPath, Out: ReadWriter](
      f: (A, B) => Target[Out]
  ): (A, B) => Target[Out] = { (a: A, b: B) =>
    f(a, b).append(ToRelPath(a) / ToRelPath(b))
  }
  def fun[A: ToRelPath, B: ToRelPath, C: ToRelPath, Out: ReadWriter](
      f: (A, B, C) => Target[Out]
  ): (A, B, C) => Target[Out] = { (a: A, b: B, c: C) =>
    f(a, b, c).append(ToRelPath(a) / ToRelPath(b) / ToRelPath(c))
  }
  def fun[
      A: ToRelPath,
      B: ToRelPath,
      C: ToRelPath,
      D: ToRelPath,
      Out: ReadWriter
  ](f: (A, B, C, D) => Target[Out]): (A, B, C, D) => Target[Out] = {
    (a: A, b: B, c: C, d: D) =>
      f(a, b, c, d).append(
        ToRelPath(a) / ToRelPath(b) / ToRelPath(c) / ToRelPath(d)
      )
  }

  def complex_target[Out: ReadWriter](f: Tracker[Out] ?=> Future[Result[Out]])(
      using fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) { f }
}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))

  given ToRelPath[Scope] = { s => s.base }
}
