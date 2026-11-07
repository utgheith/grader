package ag.r2

import ag.common.Signer

import upickle.default.ReadWriter

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

def say(ctx: Context[?], msg: => Any): Unit = Context.say(Some(ctx), msg)

// for code that isn't part of any target's computation
def say(msg: => Any): Unit = Context.say(None, msg)

def shout(ctx: Context[?], msg: => Any): Unit = {
  say(ctx, msg)
  println(msg)
}

def shout(msg: => Any): Unit = {
  say(msg)
  println(msg)
}

// Called from within a target's function, runs f iff the target's value needs to be recomputed
def run_if_needed[A: ReadWriter](tracker: Tracker[A])(
    f: Producer[A] => A
): Result[A] =
  tracker.run_if_needed(f)

def create_data[A](ctx: Producer[?])(skip: os.RelPath => Boolean)(
    f: os.Path => A
): WithData[A] = {
  val data_dir: os.Path = ctx.data_path
  os.remove.all(data_dir)
  os.makeDir.all(data_dir)
  val a = f(data_dir)
  WithData(
    a,
    ctx.producing.path,
    Signer[(os.Path, os.RelPath => Boolean)].sign((data_dir, skip))
  )
}

def update_data[A](ctx: Producer[?])(skip: os.RelPath => Boolean)(
    f: os.Path => A
): WithData[A] = {
  val data_dir: os.Path = ctx.data_path
  if (!os.isDir(data_dir)) {
    os.remove.all(data_dir)
    os.makeDir.all(data_dir)
  }
  val a = f(data_dir)
  WithData(
    a,
    ctx.producing.path,
    Signer[(os.Path, os.RelPath => Boolean)].sign((data_dir, skip))
  )
}

lazy val periodic = Scope().fun { (ms: Long) =>
  Scope().target(EmptyTuple) { (_, _) =>
    (System.currentTimeMillis() / ms) * ms
  }
}

// val x = target { ... }

//def scoped[A: ToRelPath, Out: ReadWriter](f: A => Target[Out]): A => Target[Out] = { (a: A) => f(a).append(ToRelPath(a)) }
//def scoped[A: ToRelPath, B: ToRelPath, Out: ReadWriter](f: (A,B) => Target[Out]): (A,B) => Target[Out] = {(a: A,b: B) => f(a,b).append(ToRelPath(a) / ToRelPath(b))}
