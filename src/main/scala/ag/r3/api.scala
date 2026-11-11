package ag.r3

import upickle.default.ReadWriter

def run_if_needed[A: ReadWriter](tracker: Tracker[A])(
    f: Producer[A] => A
): Result[A] = tracker.run_if_needed(f)

// Gives `f` an empty directory to fill, and keeps what it leaves there as the
// target's data. A git repo anywhere in it fails the target.
def create_data[A](ctx: Producer[?])(f: os.Path => A): WithData[A] =
  with_scratch(ctx.state) { dir =>
    val a = f(dir)
    keep(ctx, a, dir)
  }

// The same, but the directory starts with the target's previous data.
def update_data[A](ctx: Producer[?])(f: os.Path => A): WithData[A] =
  with_scratch(ctx.state) { dir =>
    ctx.previous_data.foreach(t =>
      ctx.state.store.checkout_to(t, dir, writable = true)
    )
    val a = f(dir)
    keep(ctx, a, dir)
  }

private def keep[A](ctx: Producer[?], a: A, dir: os.Path): WithData[A] = {
  val tree = ctx.state.store.import_dir(dir)
  ctx.data = Some(tree)
  WithData(a, Tree(tree.name.nn))
}

lazy val periodic: Long => Target[Long] = Scope().fun { (ms: Long) =>
  Scope().target(EmptyTuple) { (_, _) =>
    (System.currentTimeMillis() / ms) * ms
  }
}
