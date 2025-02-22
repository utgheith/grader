package ag.r2

import scala.concurrent.Future
import upickle.default.ReadWriter

def run_if_needed[A: ReadWriter](f: => Future[A])(using
    ctx: Context[A]
): Future[Result[A]] =
  ctx.run_if_needed(f)

def target[Out: ReadWriter](
    f: Context[Out] ?=> Future[Result[Out]]
)(using fn: sourcecode.FullName): Target[Out] = Target(
  ToRelPath(fn)
) { ctx => f(using ctx) }

def target[A: ToRelPath, Out: ReadWriter](
    f: Context[Out] ?=> A => Future[Result[Out]]
)(using fn: sourcecode.FullName): A => Target[Out] = { a =>
  Target[Out](
    ToRelPath(fn) / ToRelPath(a)
  ) { ctx => f(using ctx)(a) }
}

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
