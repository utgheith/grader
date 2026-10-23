package ag.r2

import ag.common.Signer
import ag.task.Task

import upickle.default.ReadWriter

import scala.compiletime.summonFrom

object Noise {
  private var noise: Boolean = false

  def apply(): Boolean = noise

  Thread.ofPlatform().daemon(true).start { () =>
    while (true) {
      println("-- hit enter to toggle noise --")
      val _ = System.in.read()
      noise = !noise
    }
  }
}

inline def force_task[A](v: A | Task[A]): Task[A] = v match {
  case fa: Task[?] => fa.asInstanceOf[Task[A]]
  case v           => Task.successful(v.asInstanceOf[A])
}

inline def say(inline msg: => Any): Unit = {
  summonFrom[Context[?]] {
    case ctx: Context[?] => Context.say(Some(ctx), msg)
    case _               => Context.say(None, msg)
  }
}

inline def shout(inline msg: => Any): Unit = {
  say(msg)
  println(msg)
}

// Called from within a target's function, runs f iff the target's value needs to be recomputed
def run_if_needed[A: ReadWriter](
    f: Producer[A] ?=> A | Task[A]
)(using tracker: Tracker[A]): Task[Result[A]] =
  tracker.run_if_needed(force_task(f))

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

lazy val periodic = Scope().fun { (ms: Long) =>
  Scope().target() {
    (System.currentTimeMillis() / ms) * ms
  }
}

// val x = target { ... }

//def scoped[A: ToRelPath, Out: ReadWriter](f: A => Target[Out]): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
//def scoped[A: ToRelPath, B: ToRelPath, Out: ReadWriter](f: (A,B) => Target[Out]): (A,B) => Target[Out] = {(a: A,b: B) => f(a,b).append(ToRelPath(a) / ToRelPath(b))}
