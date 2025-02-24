package ag.r2

import scala.concurrent.Future
import upickle.default.ReadWriter

// Called from within a target's function, runs f iff the target's value needs to be recomputed
def run_if_needed[A: ReadWriter](f: => Future[A])(using
    ctx: Context[A]
): Future[Result[A]] =
  ctx.run_if_needed(f)

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

// defines a simple target whose, the LHS should be a val
// val x = target { ... }
def target[Out: ReadWriter](
    f: Context[Out] ?=> Future[Result[Out]]
)(using fn: sourcecode.FullName): Target[Out] = Target(
  ToRelPath(fn)
) { ctx => f(using ctx) }

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
