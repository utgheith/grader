package ag.grader

import munit.FunSuite

import java.time.LocalDateTime

class ProjectStateTests extends FunSuite {
  private val first = LocalDateTime.of(2026, 10, 1, 23, 59)
  private val second = LocalDateTime.of(2026, 10, 8, 23, 59)
  private def at(t: LocalDateTime) = ProjectState.at(t, first, second)

  test(
    "the state follows the two deadlines, each taking effect at its instant"
  ) {
    assertEquals(at(first.minusMinutes(1)), ProjectState.BeforeFirstDeadline)
    assertEquals(at(first), ProjectState.BetweenDeadlines)
    assertEquals(at(second.minusMinutes(1)), ProjectState.BetweenDeadlines)
    assertEquals(at(second), ProjectState.AfterSecondDeadline)
  }

  test("each state's liveness, keep-going default and tests") {
    import ProjectState.*
    assertEquals(
      ProjectState.values.toList.map(s =>
        (s, s.is_live, s.latest_keeps_going, s.latest_tests)
      ),
      List(
        (BeforeFirstDeadline, true, false, "all"),
        (BetweenDeadlines, true, false, "good"),
        (AfterSecondDeadline, false, true, "chosen")
      )
    )
    for (s <- ProjectState.values) {
      assert(s.keeps_going(Phase.One) && s.keeps_going(Phase.Two))
      assertEquals(s.keeps_going(Phase.Latest), s.latest_keeps_going)
      assert(s.tests(Phase.One) == "phase1")
      assert(s.tests(Phase.Two) == "phase2")
      assert(s.tests(Phase.Latest) == s.latest_tests)
    }
  }

  test("moving a deadline moves the state") {
    val now = first.plusDays(2)
    assertEquals(at(now), ProjectState.BetweenDeadlines)
    assertEquals(
      ProjectState.at(
        now,
        first,
        now.minusHours(1)
      ), // second deadline moved earlier
      ProjectState.AfterSecondDeadline
    )
    assertEquals(
      ProjectState.at(now, now.plusDays(1), second), // first deadline extended
      ProjectState.BeforeFirstDeadline
    )
  }

  test("a live project goes first, then the lowest numbered") {
    def pick(items: (String, Boolean)*) =
      Priority.pick(items.map((n, live) => (n, n, live)))
    assertEquals(pick("p1" -> false, "p3" -> true, "p2" -> false), Some("p3"))
    assertEquals(pick("p4" -> true, "p3" -> true, "p1" -> false), Some("p3"))
    assertEquals(pick("p2" -> false, "p1" -> false), Some("p1"))
    assertEquals(pick("p10" -> false, "p9" -> false), Some("p9"))
    assertEquals(pick(), None)
  }

  test("a state survives the cache") {
    for (s <- ProjectState.values)
      assertEquals(
        upickle.default.read[ProjectState](upickle.default.write(s)),
        s
      )
  }
}
