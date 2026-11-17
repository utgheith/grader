package ag.rules

import ag.common.TempDirs
import munit.FunSuite

// What a State's Progress counts, and how the status screen lays it out
class StatusScreenTests extends FunSuite {
  import Progress.{InFlight, Phase, Snapshot}

  test(
    "a target that ran counts as ran, a current one as reused, a throw as failed"
  ) {
    val dir = TempDirs.dir()
    val s = Scope("progress")
    val a = s.target(EmptyTuple)((_, _) => 1)
    val b = s.target((a = a))((_, d) => d.a + 1)
    val bad = s.target((a = a))((_, _) => (throw Exception("boom")): Int)
    val first = State(dir)
    val _ = first.track(b)
    val p1 = first.progress.snapshot
    assertEquals((p1.started, p1.ran, p1.reused, p1.failed), (2L, 2L, 0L, 0L))
    val second = State(dir)
    val _ = second.track(b)
    val _ = intercept[Exception](second.track(bad))
    val p2 = second.progress.snapshot
    // a, with nothing to depend on, always runs again; b is current
    assertEquals((p2.started, p2.ran, p2.reused, p2.failed), (3L, 1L, 1L, 1L))
    assertEquals(p2.in_flight, Seq())
  }

  test(
    "a target is shown by its display, then by what it says, while it's in flight"
  ) {
    val s = Scope("describe")
    var seen = Seq[String]()
    val t = s.target(EmptyTuple) { (producer, _) =>
      def shown = producer.state.progress.snapshot.in_flight.map(_.display)
      seen = seen ++ shown
      producer.describe("run 2 of 3")
      producer.waiting(())
      seen = seen ++ shown
      1
    }
    val st = State(TempDirs.dir())
    val _ = st.track(t)
    assertEquals(seen, Seq(t.path.toString, "run 2 of 3"))
    assertEquals(st.progress.snapshot.in_flight, Seq())
  }

  test("counts, then the longest running first, as many as fit") {
    def r(n: Int, since: Long, phase: Phase = Phase.Running) =
      InFlight(os.RelPath(s"keyed/x/$n"), s"t/$n", phase, since)
    val snap = Snapshot(
      started = 20,
      reused = 5,
      ran = 3,
      failed = 1,
      in_flight =
        Seq(r(1, 50_000), r(2, 10_000), r(3, 90_000), r(4, 0, Phase.Checking))
    )
    val all = StatusScreen.frame(snap, 0, 100_000, 80, 24)
    assertEquals(
      all,
      Seq(
        "1m40s: 20 targets started",
        "in flight: 1 checking, 0 queued, 3 running",
        "finished: 5 reused, 3 ran, 1 failed",
        "",
        "running, longest first:",
        "   1m30s  t/2",
        "     50s  t/1",
        "     10s  t/3"
      )
    )
    assertEquals(
      StatusScreen.frame(snap, 0, 100_000, 80, 7).drop(5),
      Seq("   1m30s  t/2", "  … and 2 more")
    )
    assert(StatusScreen.frame(snap, 0, 100_000, 12, 24).forall(_.length <= 12))
  }
}
