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
  ) { ctx =>
    given Producer[Out] = ctx

    run_if_needed {
      Future.successful(f)
    }
  }

  // val x = target(ta) { va => ... }
  def target[A, Out: ReadWriter](ta: Target[A])(f: Producer[Out] ?=> A => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base 
  ) { ctx =>
    given Producer[Out] = ctx

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
  def target[A, B, Out: ReadWriter](ta: Target[A], tb: Target[B])(f: (A, B) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) { ctx =>
    given Producer[Out] = ctx

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
  def target[A, B, C, Out: ReadWriter](ta: Target[A], tb: Target[B], tc: Target[C])(f: (A, B, C) => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
    ToRelPath(fn) / base
  ) { ctx =>
    given Producer[Out] = ctx

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
  
  // fun { (a: A) => target { ... } } same as target { ... } but appends "/a" to the target path
  def fun[A: ToRelPath, Out: ReadWriter](f:  A => Target[Out]): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
  def fun[A: ToRelPath, B: ToRelPath, Out: ReadWriter](f:  (A,B) => Target[Out]): (A,B) => Target[Out] = { (a: A, b: B) => f(a,b).append(ToRelPath(a) / ToRelPath(b)) }
  
  def complex_target[Out: ReadWriter](f: Producer[Out] ?=> Future[Result[Out]])(using fn: sourcecode.FullName): Target[Out] = Target(ToRelPath(fn) / base){ ctx => f(using ctx) }


}

object Scope {
  def apply(): Scope = new Scope(os.RelPath("."))
  def apply[A: ToRelPath](a: A): Scope = new Scope(ToRelPath(a))
  
  given ToRelPath[Scope] = { s => s.base }
}
