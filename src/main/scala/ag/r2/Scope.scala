package ag.r2

import upickle.default.ReadWriter

import ag.task.Task

class Scope(base_ : os.RelPath | String | Scope) { self =>

  val base: os.RelPath = base_ match {
    case r: os.RelPath => r
    case s: String     => ToRelPath(s)
    case s: Scope      => ToRelPath(s)
  }
  def /[A: ToRelPath](more: A): Scope = new Scope(base / ToRelPath(more))

  def target[Out: ReadWriter]()(
      f: Producer[Out] ?=> Out | Task[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    run_if_needed {
      f
    }
  }

  // val x = target(ta) { va => ... }
  def target[A, Out: ReadWriter](ta: Target[A])(
      f: Producer[Out] ?=> A => Out | Task[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    run_if_needed {
      force_task(f(fa.block))
    }
  }

  // val ta: Target[A]
  // val tb: Target[B]
  // val x = target(ta, tb) { (va, vb) => ... }
  def target[A, B, Out: ReadWriter](ta: Target[A], tb: Target[B])(
      f: Producer[Out] ?=> (A, B) => Out | Task[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    run_if_needed {
      force_task(f(fa.block, fb.block))
    }
  }

  // val x = target(ta, tb, tc) { (va, vb, vc) => ... }
  def target[A, B, C, Out: ReadWriter](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C]
  )(
      f: Producer[Out] ?=> (A, B, C) => Out | Task[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    run_if_needed {
      force_task(f(fa.block, fb.block, fc.block))
    }
  }

  // val x = target(ta, tb, tc, td) { (va, vb, vc, vd) => ... }
  def target[A, B, C, D, Out: ReadWriter](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D]
  )(
      f: Producer[Out] ?=> (A, B, C, D) => Out | Task[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    run_if_needed {
      force_task(f(fa.block, fb.block, fc.block, fd.block))
    }
  }

  // val x = target(ta, tb, tc, td, te) { (va, vb, vc, vd, ve) => ... }
  def target[A, B, C, D, E, Out: ReadWriter](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E) => Out | Task[Out]
  )(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) {
    val fa = ta.track
    val fb = tb.track
    val fc = tc.track
    val fd = td.track
    val fe = te.track
    run_if_needed {
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf) { (va, vb, vc, vd, ve, vf) => ... }
  def target[A, B, C, D, E, F, Out: ReadWriter](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F) => Out | Task[Out]
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
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg) { (va, vb, vc, vd, ve, vf, vg) => ... }
  def target[A, B, C, D, E, F, G, Out: ReadWriter](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G) => Out | Task[Out]
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
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th) { (va, vb, vc, vd, ve, vf, vg, vh) => ... }
  def target[A, B, C, D, E, F, G, H, Out: ReadWriter](
      ta: Target[A],
      tb: Target[B],
      tc: Target[C],
      td: Target[D],
      te: Target[E],
      tf: Target[F],
      tg: Target[G],
      th: Target[H]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H) => Out | Task[Out]
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
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti) { (va, vb, vc, vd, ve, vf, vg, vh, vi) => ... }
  def target[A, B, C, D, E, F, G, H, I, Out: ReadWriter](
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
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I) => Out | Task[Out]
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
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block, fi.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj) => ... }
  def target[A, B, C, D, E, F, G, H, I, J, Out: ReadWriter](
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
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I, J) => Out | Task[Out]
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
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block, fi.block, fj.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj, tk) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk) => ... }
  def target[A, B, C, D, E, F, G, H, I, J, K, Out: ReadWriter](
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
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block, fi.block, fj.block, fk.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj, tk, tl) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk, vl) => ... }
  def target[A, B, C, D, E, F, G, H, I, J, K, L, Out: ReadWriter](
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
        Task[Out]
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
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block, fi.block, fj.block, fk.block, fl.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj, tk, tl, tm) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk, vk, vm) => ... }
  def target[
      A,
      B,
      C,
      D,
      E,
      F,
      G,
      H,
      I,
      J,
      K,
      L,
      M,
      Out: ReadWriter
  ](
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
      tl: Target[L],
      tm: Target[M]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out |
        Task[Out]
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
    val fm = tm.track
    run_if_needed {
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block, fi.block, fj.block, fk.block, fl.block, fm.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj, tk, tl, tm, tn) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk, vk, vm, vn) => ... }
  def target[
      A,
      B,
      C,
      D,
      E,
      F,
      G,
      H,
      I,
      J,
      K,
      L,
      M,
      N,
      Out: ReadWriter
  ](
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
      tl: Target[L],
      tm: Target[M],
      tn: Target[N]
  )(
      f: Producer[Out] ?=> (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out |
        Task[Out]
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
    val fm = tm.track
    val fn = tn.track
    run_if_needed {
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block, fi.block, fj.block, fk.block, fl.block, fm.block, fn.block))
    }
  }

  // val x = target(ta, tb, tc, td, te, tf, tg, th, ti, tj, tk, tl, tm, tn) { (va, vb, vc, vd, ve, vf, vg, vh, vi, vj, vk, vk, vm, vn) => ... }
  def target[
      A,
      B,
      C,
      D,
      E,
      F,
      G,
      H,
      I,
      J,
      K,
      L,
      M,
      N,
      O,
      Out: ReadWriter
  ](
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
      tl: Target[L],
      tm: Target[M],
      tn: Target[N],
      to: Target[O]
  )(
      f: Producer[Out] ?=> (
          A,
          B,
          C,
          D,
          E,
          F,
          G,
          H,
          I,
          J,
          K,
          L,
          M,
          N,
          O
      ) => Out | Task[Out]
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
    val fm = tm.track
    val fn = tn.track
    val fo = to.track
    run_if_needed {
      force_task(f(fa.block, fb.block, fc.block, fd.block, fe.block, ff.block, fg.block, fh.block, fi.block, fj.block, fk.block, fl.block, fm.block, fn.block, fo.block))
    }
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

  def complex_target[Out](f: Tracker[Out] ?=> Task[Result[Out]])(using
      fn: sourcecode.FullName
  ): Target[Out] = Target(ToRelPath(fn) / base) { f }
}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))

  given ToRelPath[Scope] = { s => s.base }
}
