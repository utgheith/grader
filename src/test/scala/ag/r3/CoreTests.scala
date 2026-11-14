package ag.r3

import ag.common.TempDirs
import munit.FunSuite
import upickle.default.ReadWriter

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

// ag.r2's behavior, which r3 keeps (ported from ag.r2's tests)
class CoreTests extends FunSuite {
  private def ws(): os.Path = TempDirs.dir()

  test("scope base path, and a target's path") {
    assertEquals(Scope().base, os.RelPath("."))
    assertEquals((Scope("foo") / "bar").base, os.RelPath("foo/bar"))
    case class Data(i: Int) derives ReadWriter
    val t1 = Scope("test").target(EmptyTuple)((_, _) => Data(1))
    assertEquals(t1.path, os.RelPath("ag/r3/CoreTests/t1/test"))
    assertEquals(State(ws()).track(t1), Data(1))
    val f = Scope("fun").fun((i: Int) => Scope("fun").target(EmptyTuple)((_, _) => i))
    assertEquals(f(10).path.last, "10")
  }

  test("a target reruns only when what it depends on changes") {
    val dir = ws()
    var input = 1
    val runs = AtomicInteger(0)
    val s = Scope("deps")
    val in: Target[Int] = s.target(EmptyTuple)((_, _) => input)
    val out: Target[Int] = s.target((i = in)) { (_, deps) =>
      val _ = runs.incrementAndGet()
      deps.i * 10
    }
    assertEquals(State(dir).track(out), 10)
    assertEquals(State(dir).track(out), 10)
    assertEquals(runs.get, 1, "kept")
    input = 2
    assertEquals(State(dir).track(out), 20)
    assertEquals(runs.get, 2, "reran")
  }

  test("a rerun with the same result doesn't rerun its dependents") {
    val dir = ws()
    var input = 1
    val (mid_runs, top_runs) = (AtomicInteger(0), AtomicInteger(0))
    val s = Scope("cutoff")
    val in: Target[Int] = s.target(EmptyTuple)((_, _) => input)
    val parity: Target[Boolean] = s.target((i = in)) { (_, deps) =>
      val _ = mid_runs.incrementAndGet()
      deps.i % 2 == 0
    }
    val top: Target[String] = s.target((p = parity)) { (_, deps) =>
      val _ = top_runs.incrementAndGet()
      if (deps.p) "even" else "odd"
    }
    assertEquals(State(dir).track(top), "odd")
    input = 3
    assertEquals(State(dir).track(top), "odd")
    assertEquals((mid_runs.get, top_runs.get), (2, 1))
  }

  test("a named tuple, and Many, are read at once and in order") {
    val n = 5
    val all_running = CountDownLatch(n)
    val s = Scope("many")
    val wait_all = () => {
      all_running.countDown()
      all_running.await(30, TimeUnit.SECONDS)
    }
    val one: Target[Boolean] = s.target(EmptyTuple)((_, _) => wait_all())
    val ints: Int => Target[Int] = s.fun((i: Int) =>
      s.target(EmptyTuple)((_, _) => if (wait_all()) i else -1)
    )
    val all: Target[(Boolean, Seq[Int])] =
      s.target((first = one, ints = Many(Seq(4, 1, 3, 2).map(ints)))) {
        (_, deps) => (deps.first, deps.ints)
      }
    assertEquals(State(ws()).track(all), (true, Seq(4, 1, 3, 2)))
  }

  test("readers of one target share one computation") {
    val runs = AtomicInteger(0)
    val s = Scope("shared")
    val shared: Target[Int] = s.target(EmptyTuple) { (_, _) =>
      Thread.sleep(50)
      runs.incrementAndGet()
    }
    val reader: Int => Target[Int] =
      s.fun((i: Int) => s.target((a = shared))((_, deps) => deps.a + i))
    val all: Target[Seq[Int]] = s.complex_target { tracker =>
      val vs = tracker.track_all((0 until 10).map(reader))
      run_if_needed(tracker)(_ => vs)
    }
    assertEquals(State(ws()).track(all), (1 to 10).toSeq)
    assertEquals(runs.get, 1)
  }

  test("a failure is reported after the other reads finish") {
    val finished = AtomicInteger(0)
    val s = Scope("failure")
    val t: Int => Target[Int] = s.fun((i: Int) =>
      s.target(EmptyTuple) { (_, _) =>
        if (i == 0) throw IllegalArgumentException("boom")
        Thread.sleep(100)
        finished.incrementAndGet()
      }
    )
    val all: Target[Seq[Int]] = s.complex_target { tracker =>
      val vs = tracker.track_all((0 until 5).map(t))
      run_if_needed(tracker)(_ => vs)
    }
    val _ = intercept[IllegalArgumentException](State(ws()).track(all))
    assertEquals(finished.get, 4)
  }

  test("force makes a current target run again, once; force_roots only the roots") {
    val dir = ws()
    val (leaf_runs, top_runs) = (AtomicInteger(0), AtomicInteger(0))
    val s = Scope("force")
    val in: Target[Int] = s.target(EmptyTuple)((_, _) => 1)
    val leaf: Target[Int] = s.target((i = in)) { (_, deps) =>
      val _ = leaf_runs.incrementAndGet()
      deps.i
    }
    val top: Target[Int] = s.target((l = leaf)) { (_, deps) =>
      val _ = top_runs.incrementAndGet()
      deps.l + 1
    }
    def runs = (top_runs.get, leaf_runs.get)
    assertEquals(State(dir).track(top), 2)
    assertEquals(State(dir).track(top), 2)
    assertEquals(runs, (1, 1))
    val st = State(dir)
    st.force(leaf)
    assertEquals(st.track(top), 2)
    assertEquals(runs, (1, 2), "the leaf ran; its result didn't change")
    assertEquals(State(dir).track(top), 2)
    assertEquals(runs, (1, 2), "and only once")
    assertEquals(State(dir, force_roots = true).track(top), 2)
    assertEquals(runs, (2, 2), "the root ran again, not its input")
  }

  test("a saved value that reads back as null is computed again") {
    val dir = ws()
    val runs = AtomicInteger(0)
    val s = Scope("nulls")
    val in: Target[Int] = s.target(EmptyTuple)((_, _) => 1)
    val either: Target[Either[String, Int]] = s.target((i = in)) { (_, deps) =>
      val _ = runs.incrementAndGet()
      Right(deps.i)
    }
    assertEquals(State(dir).track(either), Right(1))
    // what an older version could have saved
    val store = State(dir).store
    val l = store.load(either.path).get
    val _ = store.save(either.path, Some(l.commit), "null".getBytes, l.deps, None, "test")
    assertEquals(State(dir).track(either), Right(1))
    assertEquals(runs.get, 2)
  }

  test("periodic: a target reruns once its period has passed") {
    val dir = ws()
    val runs = AtomicInteger(0)
    val s = Scope("timed")
    val fast: Target[Long] = s.target((t = periodic(1))) { (_, deps) =>
      val _ = runs.incrementAndGet()
      deps.t
    }
    val _ = State(dir).track(fast)
    Thread.sleep(5)
    val _ = State(dir).track(fast)
    assertEquals(runs.get, 2)
  }

  test("a circular dependency is reported") {
    val s = Scope("circle")
    lazy val a: Target[Int] = s.complex_target { tracker =>
      val v = tracker.track(b)
      run_if_needed(tracker)(_ => v)
    }
    lazy val b: Target[Int] = s.complex_target { tracker =>
      val v = tracker.track(a)
      run_if_needed(tracker)(_ => v)
    }
    val e = intercept[Exception](State(ws()).track(a))
    assert(
      Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null)
        .exists(t => String.valueOf(t.getMessage).contains("Circular")),
      e
    )
  }
}
