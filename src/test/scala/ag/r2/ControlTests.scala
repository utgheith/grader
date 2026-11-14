package ag.r2

import ag.common.TempDirs
import munit.FunSuite

import java.util.concurrent.atomic.AtomicInteger

class ControlTests extends FunSuite {
  private def state() = State(TempDirs.dir())

  // tracks `target` on a thread of its own; join() returns its value
  private class Tracking[A](root: State, target: Target[A]) {
    @volatile private var value: Option[A] = None
    private val thread = Thread.ofVirtual().start(() => value = Some(root.track(target)))
    def join(): Option[A] = {
      thread.join(30000)
      value
    }
  }

  // waits until `p` holds for root's control
  private def eventually(root: State)(p: Control.Status => Boolean): Control.Status = {
    val limit = System.currentTimeMillis() + 30000
    var s = root.control.status
    while (!p(s) && System.currentTimeMillis() < limit) {
      Thread.sleep(10)
      s = root.control.status
    }
    assert(p(s), s.toString)
    s
  }

  test("a breakpoint holds a target once its dependencies are ready") {
    val s = Scope("control_break")
    val leaf_runs = new AtomicInteger(0)
    val top_runs = new AtomicInteger(0)
    val leaf: Target[Int] = s.target(EmptyTuple) { (_, _) => leaf_runs.incrementAndGet() }
    val top: Target[Int] = s.target((l = leaf)) { (_, deps) =>
      val _ = top_runs.incrementAndGet()
      deps.l + 1
    }
    val root = state()
    root.control.enable()
    root.control.set_breakpoint(top.kind.toString, on = true)
    val t = Tracking(root, top)
    val held = eventually(root)(_.waiting.nonEmpty)
    assertEquals(held.waiting.map(w => (w.path, w.gate, w.hit)), Seq((top.path.toString, "Run", true)))
    assert(held.paused, "a breakpoint pauses the run")
    assertEquals(leaf_runs.get, 1)
    assertEquals(top_runs.get, 0)
    root.control.resume()
    assertEquals(t.join(), Some(2))
    assertEquals(top_runs.get, 1)
  }

  test("paused, targets wait at each gate; release lets one go one gate") {
    val s = Scope("control_pause")
    val runs = new AtomicInteger(0)
    val only: Target[Int] = s.target(EmptyTuple) { (_, _) => runs.incrementAndGet() }
    val root = state()
    root.control.enable()
    root.control.pause()
    val t = Tracking(root, only)
    val at_start = eventually(root)(_.waiting.nonEmpty).waiting.head
    assertEquals((at_start.gate, at_start.hit), ("Start", false))
    root.control.release(at_start.id)
    val at_run = eventually(root)(_.waiting.exists(_.id != at_start.id)).waiting.head
    assertEquals(at_run.gate, "Run")
    assertEquals(runs.get, 0)
    root.control.release(at_run.id)
    assertEquals(t.join(), Some(1))
    assert(root.control.status.paused, "still paused")
  }

  test("failures break once, where they happen") {
    val s = Scope("control_fail")
    val broken: Target[Int] = s.target(EmptyTuple) { (_, _) => throw Exception("on purpose") }
    val above: Target[Int] = s.target((b = broken)) { (_, deps) => deps.b }
    val root = state()
    root.control.enable()
    root.control.set_break_on_failure(true)
    val t = Tracking(root, above)
    val held = eventually(root)(_.waiting.nonEmpty).waiting
    assertEquals(held.map(w => (w.path, w.gate)), Seq((broken.path.toString, "Failed")))
    root.control.resume()
    assertEquals(t.join(), None)
    assertEquals(root.control.status.waiting, Seq())
  }

  test("without enable, nothing is held") {
    val s = Scope("control_off")
    val only: Target[Int] = s.target(EmptyTuple) { (_, _) => 3 }
    val root = state()
    root.control.pause()
    root.control.set_breakpoint(only.kind.toString, on = true)
    assertEquals(root.track(only), 3)
  }
}
