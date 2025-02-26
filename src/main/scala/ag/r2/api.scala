package ag.r2

import ag.common.given_VirtualExecutionContext
import scala.concurrent.Future
import upickle.default.ReadWriter

// Called from within a target's function, runs f iff the target's value needs to be recomputed
def run_if_needed[A: ReadWriter](f: => A|Future[A])(using
    ctx: Producer[A]
): Future[Result[A]] =
  ctx.run_if_needed(f match {
    case a: A => Future.successful(a)
    case fa: Future[A] => fa
  })

// Called from inside the code called by run_if_needed if it need to populate the data directory
def create_data[A](skip: os.RelPath => Boolean)(
    f: Producer[WithData[A]] ?=> os.Path => A
)(using ctx: Producer[WithData[A]]): WithData[A] = {
  ctx.skip_filter = skip
  val data_dir: os.Path = ctx.data_path
  os.remove.all(data_dir)
  os.makeDir.all(data_dir)
  val a = f(using ctx)(data_dir)
  WithData(a, ctx.target.path)
}

val periodic = Scope().fun { (ms: Long) =>
  Scope().target() {
    (System.currentTimeMillis() / ms) * ms
  }
}




// val x = target { ... }

//def scoped[A: ToRelPath, Out: ReadWriter](f: A => Target[Out]): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
//def scoped[A: ToRelPath, B: ToRelPath, Out: ReadWriter](f: (A,B) => Target[Out]): (A,B) => Target[Out] = {(a: A,b: B) => f(a,b).append(ToRelPath(a) / ToRelPath(b))}

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
/*
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
*/

