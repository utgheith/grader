package ag.r2

import java.util.concurrent.atomic.AtomicInteger

object ForceThings extends Scope("force") {
  val runs = new AtomicInteger(0)

  // no dependencies: re-evaluated every time, but its value never changes
  val input: Target[Int] = target(EmptyTuple) {(_, _) =>  1 }

  val counted: Target[Int] = target((i = input)) { (_, deps) =>
    val _ = runs.incrementAndGet()
    deps.i
  }
}

object RootThings extends Scope("force_roots") {
  val leaf_runs = new AtomicInteger(0)
  val top_runs = new AtomicInteger(0)

  val input: Target[Int] = target(EmptyTuple) {(_, _) =>  1 }
  val leaf: Target[Int] = target((i = input)) { (_, deps) =>
    val _ = leaf_runs.incrementAndGet()
    deps.i
  }
  val top: Target[Int] = target((i = leaf)) { (_, deps) =>
    val _ = top_runs.incrementAndGet()
    deps.i + 1
  }
}

class ForceTests extends munit.FunSuite {
  test("force makes a cached target run again, once") {
    def run_once(force: Boolean = false): Int = {
      val root = State(config.get_test_dir)
      if (force) root.force(ForceThings.counted)
      root.track(ForceThings.counted)
    }

    assertEquals(run_once(), 1)
    assertEquals(ForceThings.runs.get, 1)
    assertEquals(run_once(), 1)
    assertEquals(
      ForceThings.runs.get,
      1,
      "unchanged inputs: kept the saved result"
    )
    assertEquals(run_once(force = true), 1)
    assertEquals(ForceThings.runs.get, 2, "forced: ran again")
    assertEquals(run_once(), 1)
    assertEquals(ForceThings.runs.get, 2, "and only that once")
  }

  test("force_roots rebuilds what's tracked directly, not its inputs") {
    def run_once(force_roots: Boolean): Int =
      State.run(config.get_test_dir, force_roots) { root =>
        root.track(RootThings.top)
      }

    assertEquals(run_once(false), 2)
    assertEquals((RootThings.top_runs.get, RootThings.leaf_runs.get), (1, 1))
    assertEquals(run_once(false), 2)
    assertEquals((RootThings.top_runs.get, RootThings.leaf_runs.get), (1, 1))
    assertEquals(run_once(true), 2)
    assertEquals(
      (RootThings.top_runs.get, RootThings.leaf_runs.get),
      (2, 1),
      "the root ran again; its input kept its saved result"
    )
  }
}
