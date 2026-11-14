package ag.r3

import ag.common.TempDirs
import munit.FunSuite

// Tracker.run_keyed: a keyed target's saved value is always handed back
class KeyedTests extends FunSuite {
  private def ws(): os.Path = TempDirs.dir()

  // a log that grows by one entry each time it's asked for more than it has
  private def log(n: Int): Target[Seq[Int]] =
    Target[Seq[Int]](os.rel / "log", os.rel / "log") { tracker =>
      tracker.run_keyed {
        case (_, saved @ Some(r)) if r.value.size >= n => saved
        case (_, saved) => () => saved.toSeq.flatMap(_.value) :+ saved.size
      }
    }

  test("a keyed target's value is kept across States, and grows as it decides") {
    val dir = ws()
    assertEquals(State(dir).track(log(1)), Seq(0))
    assertEquals(State(dir).track(log(1)), Seq(0))
    assertEquals(State(dir).track(log(2)), Seq(0, 1))
    assertEquals(State(dir).track(log(0)), Seq(0, 1))
  }

  test("a keyed target tracks nothing") {
    val in = Scope("keyed").target(EmptyTuple)((_, _) => 1)
    val bad = Target[Int](os.rel / "bad", os.rel / "bad") { tracker =>
      val _ = tracker.track(in)
      tracker.run_keyed((_, _) => () => 1)
    }
    val e = intercept[Throwable](State(ws()).track(bad))
    assert(clue(e.getMessage).contains("bad is keyed, but tracks"))
  }

  test("forcing a keyed target starts it over") {
    val dir = ws()
    assertEquals(State(dir).track(log(2)), Seq(0))
    State(dir).force(log(2))
    assertEquals(State(dir).track(log(2)), Seq(0))
  }
}
