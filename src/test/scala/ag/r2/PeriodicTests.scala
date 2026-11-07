package ag.r2

import java.util.concurrent.atomic.AtomicInteger

// The shape of Project.state: a target that, besides its real inputs,
// depends on periodic(ms) to be looked at again as time passes.
object Timed extends Scope("timed") {
  val fast_runs = new AtomicInteger(0)
  val slow_runs = new AtomicInteger(0)

  val fast: Target[Long] = target((t = periodic(1))) { (_, deps) =>
    val _ = fast_runs.incrementAndGet()
    deps.t
  }

  val slow: Target[Long] = target((t = periodic(24L * 60 * 60 * 1000))) { (_, deps) =>
    val _ = slow_runs.incrementAndGet()
    deps.t
  }
}

class PeriodicTests extends munit.FunSuite {
  private def run_once[A](t: Target[A]): A =
    State.run(config.get_test_dir)(root => root.track(t))

  test("a target depending on periodic reruns once its period has passed") {
    val _ = run_once(Timed.fast)
    Thread.sleep(5) // several 1 ms periods
    val _ = run_once(Timed.fast)
    assertEquals(Timed.fast_runs.get, 2)
  }

  test("... and not before") {
    val first = run_once(Timed.slow)
    val second = run_once(Timed.slow)
    // two runs a moment apart are in the same day, unless they straddle
    // midnight (UTC); only then may the target have run twice
    if (first == second) assertEquals(Timed.slow_runs.get, 1)
  }
}
