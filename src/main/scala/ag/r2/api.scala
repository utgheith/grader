package ag.r2

import ag.common.Signer

import scala.concurrent.Future
import upickle.default.ReadWriter

import scala.compiletime.summonFrom
import scala.reflect.ClassTag

object Noise {
  private var noise: Boolean = false

  def apply(): Boolean = noise

  Thread.ofPlatform().daemon(true).start { () =>
    while (true) {
      val _ = System.in.read()
      noise = !noise
    }
  }
}

inline def force_future[A: ClassTag](v: A | Future[A]): Future[A] = v match {
  case fa: Future[?] => fa.mapTo[A]
  case v             => Future.successful(v).mapTo[A]
}

inline def say(inline msg: => Any): Unit = if (Noise()) {
  summonFrom[Context[?]] {
    case ctx: Context[?] => Context.say(Some(ctx), msg)
    case _               => Context.say(None, msg)
  }
}

// Called from within a target's function, runs f iff the target's value needs to be recomputed
def run_if_needed[A: {ClassTag, ReadWriter}](
    f: Producer[A] ?=> A | Future[A]
)(using tracker: Tracker[A]): Future[Result[A]] =
  tracker.state.run_if_needed(force_future(f))

def eval[A, B, Out: {ClassTag, ReadWriter}](fa: Future[A], fb: Future[B])(
    f: Producer[Out] ?=> (A, B) => Out | Future[Out]
)(using Tracker[Out]): Future[Result[Out]] =
  run_if_needed {
    for {
      a <- fa
      b <- fb
      out <- force_future(f(a, b))
    } yield out
  }

inline def eval[A, B, C, Out: {ClassTag, ReadWriter}](
    fa: Future[A],
    fb: Future[B],
    fc: Future[C]
)(
    f: Producer[Out] ?=> (A, B, C) => Out | Future[Out]
)(using Tracker[Out]): Future[Result[Out]] =
  run_if_needed {
    for {
      a <- fa
      b <- fb
      c <- fc
      out <- force_future(f(a, b, c))
    } yield out
  }

def eval[A, B, C, D, Out: {ClassTag, ReadWriter}](
    fa: Future[A],
    fb: Future[B],
    fc: Future[C],
    fd: Future[D]
)(
    f: Producer[Out] ?=> (A, B, C, D) => Out | Future[Out]
)(using Tracker[Out]): Future[Result[Out]] =
  run_if_needed {
    for {
      a <- fa
      b <- fb
      c <- fc
      d <- fd
      out <- force_future(f(a, b, c, d))
    } yield out
  }

def create_data[A](skip: os.RelPath => Boolean)(
    f: Producer[WithData[A]] ?=> os.Path => A
)(using ctx: Producer[WithData[A]]): WithData[A] = {
  val data_dir: os.Path = ctx.data_path
  os.remove.all(data_dir)
  os.makeDir.all(data_dir)
  val a = f(using ctx)(data_dir)
  WithData(
    a,
    ctx.producing.path,
    Signer[(os.Path, os.RelPath => Boolean)].sign((data_dir, skip))
  )
}

def update_data[A](skip: os.RelPath => Boolean)(
    f: Producer[WithData[A]] ?=> os.Path => A
)(using ctx: Producer[WithData[A]]): WithData[A] = {
  val data_dir: os.Path = ctx.data_path
  if (!os.isDir(data_dir)) {
    os.remove.all(data_dir)
    os.makeDir.all(data_dir)
  }
  val a = f(using ctx)(data_dir)
  WithData(
    a,
    ctx.producing.path,
    Signer[(os.Path, os.RelPath => Boolean)].sign((data_dir, skip))
  )
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
