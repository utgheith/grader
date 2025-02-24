package ag.r2

import ag.common.given_VirtualExecutionContext
import scala.concurrent.Future
import upickle.default.ReadWriter

// Called from within a target's function, runs f iff the target's value needs to be recomputed
def run_if_needed[A: ReadWriter](f: => A|Future[A])(using
    ctx: Context[A]
): Future[Result[A]] =
  ctx.run_if_needed(f match {
    case a: A => Future.successful(a)
    case fa: Future[A] => fa
  })

// Called from inside the code called by run_if_needed if it need to populate the data directory
def create_data(skip: os.RelPath => Boolean)(
    f: Context[?] ?=> os.Path => Unit
)(using ctx: Context[?]): Unit = {
  ctx.skip_filter = skip
  val data_dir: os.Path = ctx.data_path
  os.remove.all(data_dir)
  os.makeDir.all(data_dir)
  f(using ctx)(data_dir)
}

val periodic = scoped { (ms: Long) =>
  target() {
    (System.currentTimeMillis() / ms) * ms
  }
}




// val x = target { ... }
def target[Out: ReadWriter]()(f: Context[?] ?=> Out)(using fn: sourcecode.FullName): Target[Out] = Target(
  ToRelPath(fn)
) { ctx =>
  given Context[Out] = ctx
  
  run_if_needed {
    Future.successful(f)
  }
}

// val x = target(ta) { va => ... }
def target[A, Out: ReadWriter](ta: Target[A])(f: Context[?] ?=> A => Out)(using fn: sourcecode.FullName): Target[Out] = Target(
  ToRelPath(fn)
) { ctx =>
  given Context[Out] = ctx

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
  ToRelPath(fn)
) { ctx =>
  given Context[Out] = ctx
  val fa = ta.track
  val fb = tb.track
  run_if_needed {
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
  }
}

def scoped[A: ToRelPath, Out: ReadWriter](f: A => Target[Out]): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
def scoped[A: ToRelPath, B: ToRelPath, Out: ReadWriter](f: (A,B) => Target[Out]): (A,B) => Target[Out] = {(a: A,b: B) => f(a,b).append(ToRelPath(a) / ToRelPath(b))}

//
// defines a scoped target, 'a' will contribute to the scope
// val s = target { a: Int => {
//     x.track
//     run_if_needed {
//         ...
//     }
// }
//
// use: s(10).path ===> ".../s/10"
//
def target[A: ToRelPath, Out: ReadWriter](
    f: Context[Out] ?=> A => Future[Result[Out]]
)(using fn: sourcecode.FullName): A => Target[Out] = { a =>
  Target[Out](
    ToRelPath(fn) / ToRelPath(a)
  ) { ctx => f(using ctx)(a) }
}

// val x = target { (a: Person, b: Tree) => { ... } }
// path ===> ".../x/Bob/oak"
def target[A: ToRelPath, B: ToRelPath, Out: ReadWriter](
    f: Context[Out] ?=> (A, B) => Future[Result[Out]]
)(using fn: sourcecode.FullName): (A, B) => Target[Out] = { (a, b) =>
  Target[Out](
    ToRelPath(fn) / ToRelPath(a) / ToRelPath(b)
  ) { ctx => f(using ctx)(a, b) }
}

def target[A: ToRelPath, B: ToRelPath, C: ToRelPath, Out: ReadWriter](
    f: (Context[Out], A, B, C) => Future[Result[Out]]
)(using fn: sourcecode.FullName): (A, B, C) => Target[Out] = { (a, b, c) =>
  Target[Out](
    ToRelPath(fn) / ToRelPath(a) / ToRelPath(b) / ToRelPath(c)
  ) { ctx => f(ctx, a, b, c) }
}

