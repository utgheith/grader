package ag.r2

import ag.task.Task

object Things extends Scope(".") {
  val a: Target[WithData[Int]] = target(EmptyTuple) {(producer, _) => 
    create_data(producer)(_ => false) { path =>
      os.write(path / "xyz", "hello\n")
      10
    }
  }

  val b: (String, Int) => Target[Int] = fun { (s, i) =>
    target((a = a)) { (_, deps) =>
      println(s"running $s $i, a:${deps.a.toString}")
      deps.a.value + i
    }
  }
}

// Mirrors the shape of ag.grader.Course.active_courses / Project.all_projects
// etc: track one target eagerly (`roots`), then, once its value is known,
// *asynchronously* discover a further set of per-element targets to track
// (via Task.flatMap/Task.sequence rather than eager/blocking tracking).
object RaceThings extends Scope("race") {
  // roots' value changes between "runs" (simulating a real, external change)
  // so a later run is forced to actually *recompute* `combined` -- and not
  // just compare against, and keep, its old saved state.
  val run_number = new java.util.concurrent.atomic.AtomicInteger(0)

  val roots: Target[Seq[Int]] = target(EmptyTuple) {(_, _) => 
    if (run_number.incrementAndGet() == 1) Seq(1, 2) else Seq(1, 2, 3)
  }

  val elem: Int => Target[Boolean] = fun { (i: Int) =>
    target(EmptyTuple) {(_, _) =>  i % 2 == 0 }
  }

  lazy val combined: Target[Seq[Boolean]] = complex_target { tracker =>
    val all_task: Task[Seq[Int]] = tracker.track(roots)
    val flags_task: Task[Seq[Boolean]] =
      all_task.flatMap { all =>
        // stand-in for the non-trivial work of turning each element into
        // its target and kicking off tracking; widens the race window so
        // the bug below reproduces reliably instead of only occasionally
        Thread.sleep(50)
        Task.sequence(all.map(it => tracker.track(elem(it))))
      }
    run_if_needed(tracker) { _ =>
      flags_task.block
    }
  }
}

class TargetTests extends munit.FunSuite {
  test("basic tracking") {
    def doit(d: Int): Unit = {
      val root = State(config.get_test_dir)

      val a = root.track(Things.a).block
      assertEquals(a.value, 10)
      val b = root.track(Things.b("thing", d)).block
      assertEquals(b, 10 + d)

      assertEquals(a.value + b, 20 + d)
    }
    doit(6)
    doit(4)
    doit(6)
  }

  // KNOWN BUG: Tracker.dependencies is a `lazy val` that gets forced (and
  // memoized) the first time it's read. On a target's *first* run there's
  // no saved.json, so it's only forced after the whole computation Task
  // completes -- by which point every `tracker.track` call the body made,
  // including asynchronous ones, has already registered its dependency.
  //
  // But on a *later* run, `run`'s old-state check reads saved.json and
  // forces `dependencies` immediately, to compare it against the old
  // dependency map -- synchronously, right as `run_if_needed` is called,
  // before any dependency that's only discovered asynchronously (i.e.
  // after some other tracked Task completes, as with `RaceThings.combined`
  // above) has had a chance to register itself. That closes the tracker's
  // phase before the deferred `tracker.track(elem(i))` calls run, so they blow up.
  test(
    "KNOWN BUG: dependencies discovered asynchronously after a restart race Tracker.dependencies"
  ) {
    // first run: nothing to race against yet (no saved.json), so the
    // dependency set comes out correct
    locally {
      val root = State(config.get_test_dir)
      assertEquals(root.track(RaceThings.combined).block, Seq(false, true))
    }

    // second run: saved.json now exists, so the old-state check forces
    // Tracker.dependencies early, racing the async per-element discovery.
    // `roots` now resolves to a different value, so `combined` is forced
    // to actually recompute rather than just compare against, and keep,
    // its old state.
    locally {
      val root = State(config.get_test_dir)
      intercept[IllegalStateException] {
        root.track(RaceThings.combined).block
      }
    }
  }
}
