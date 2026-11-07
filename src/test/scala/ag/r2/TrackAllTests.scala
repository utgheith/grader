package ag.r2

import munit.FunSuite

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class TrackAllTests extends FunSuite {
  private def root() = State(os.temp.dir(deleteOnExit = true))

  test("track_all really runs its targets at the same time") {
    val n = 20
    val all_running = new CountDownLatch(n)
    val s = Scope("together")
    val t: Int => Target[Boolean] = s.fun { (i: Int) =>
      s.target(EmptyTuple) { (_, _) =>
        all_running.countDown()
        // true only if all of them got here while this one was still running
        i >= 0 && all_running.await(30, TimeUnit.SECONDS)
      }
    }
    val together: Target[Seq[Boolean]] = s.complex_target { tracker =>
      val vs = tracker.track_all((0 until n).map(t))
      run_if_needed(tracker)(_ => vs)
    }
    assertEquals(root().track(together), Seq.fill(n)(true))
  }

  test("a named tuple is read at once too, under its names") {
    val both_running = new CountDownLatch(2)
    val s = Scope("named_together")
    val wait_both = () => {
      both_running.countDown()
      both_running.await(30, TimeUnit.SECONDS)
    }
    val a: Target[Boolean] = s.target(EmptyTuple)((_, _) => wait_both())
    val b: Target[Boolean] = s.target(EmptyTuple)((_, _) => wait_both())
    val c: Target[Boolean] =
      s.target((a = a, b = b))((_, deps) => deps.a && deps.b)
    assert(root().track(c))
  }

  test("Many reads targets of mixed groups at once, in order") {
    val n = 6
    val all_running = new CountDownLatch(n)
    val s = Scope("many")
    val wait_all = () => {
      all_running.countDown()
      all_running.await(30, TimeUnit.SECONDS)
    }
    val one: Target[Boolean] = s.target(EmptyTuple)((_, _) => wait_all())
    val ints: Int => Target[Int] = s.fun { (i: Int) =>
      s.target(EmptyTuple)((_, _) => if (wait_all()) i else -1)
    }
    val strs: Int => Target[String] = s.fun { (i: Int) =>
      s.target(EmptyTuple)((_, _) => if (wait_all()) s"s$i" else "late")
    }
    val all: Target[(Boolean, Seq[Int], Seq[String])] =
      s.target(
        (
          first = one,
          ints = Many(Seq(3, 1, 2).map(ints)),
          strs = Many(Seq(5, 4).map(strs))
        )
      )((_, deps) => (deps.first, deps.ints, deps.strs))
    assertEquals(root().track(all), (true, Seq(3, 1, 2), Seq("s5", "s4")))
  }

  test("an empty Many is fine, and a Many's targets are dependencies") {
    val dir = os.temp.dir(deleteOnExit = true)
    val s = Scope("many_deps")
    val a: Target[Int] = s.target(EmptyTuple)((_, _) => 1)
    val c: Target[Int] =
      s.target((xs = Many(Seq(a)), none = Many(Seq[Target[Int]]()))) {
        (_, deps) => deps.xs.sum + deps.none.size
      }
    assertEquals(State(dir).track(c), 1)
    assert(clue(os.read(State(dir).saved_path(c))).contains("many_deps"))
  }

  test("readers of one target share one computation") {
    val runs = new AtomicInteger(0)
    val s = Scope("shared")
    val shared: Target[Int] = s.target(EmptyTuple) { (_, _) =>
      Thread.sleep(50)
      runs.incrementAndGet()
    }
    val reader: Int => Target[Int] = s.fun { (i: Int) =>
      s.target((a = shared))((_, deps) => deps.a + i)
    }
    val all: Target[Seq[Int]] = s.complex_target { tracker =>
      val vs = tracker.track_all((0 until 10).map(reader))
      run_if_needed(tracker)(_ => vs)
    }
    assertEquals(root().track(all), (1 to 10).toSeq)
    assertEquals(runs.get, 1)
  }

  test("dynamic reads are all recorded before the dependencies close") {
    val dir = os.temp.dir(deleteOnExit = true)
    val session = new AtomicInteger(0)
    val s = Scope("dynamic")
    val roots: Target[Seq[Int]] = s.target(EmptyTuple) { (_, _) =>
      if (session.get == 1) Seq(1, 2) else Seq(1, 2, 3)
    }
    val elem: Int => Target[Boolean] = s.fun { (i: Int) =>
      s.target(EmptyTuple)((_, _) => i % 2 == 0)
    }
    val combined: Target[Seq[Boolean]] = s.complex_target { tracker =>
      val all = tracker.track(roots)
      val flags = tracker.track_all(all.map(elem))
      run_if_needed(tracker)(_ => flags)
    }
    session.set(1)
    assertEquals(State(dir).track(combined), Seq(false, true))
    session.set(2)
    assertEquals(State(dir).track(combined), Seq(false, true, false))
    assert(
      clue(os.read(State(dir).saved_path(combined))).contains("dynamic/3"),
      "the dependency found last is recorded"
    )
  }

  test("a failure is reported after the other reads finish") {
    val finished = new AtomicInteger(0)
    val s = Scope("failure")
    val t: Int => Target[Int] = s.fun { (i: Int) =>
      s.target(EmptyTuple) { (_, _) =>
        if (i == 0) throw new IllegalArgumentException("boom")
        Thread.sleep(100)
        finished.incrementAndGet()
      }
    }
    val all: Target[Seq[Int]] = s.complex_target { tracker =>
      val vs = tracker.track_all((0 until 5).map(t))
      run_if_needed(tracker)(_ => vs)
    }
    val _ = intercept[IllegalArgumentException](root().track(all))
    assertEquals(finished.get, 4)
  }
}
