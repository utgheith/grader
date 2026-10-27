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

  test("each state's priority, keep-going default and tests") {
    import ProjectState.*
    assertEquals(
      ProjectState.values.toList.map(s =>
        (s, s.priority, s.latest_keeps_going, s.latest_tests)
      ),
      List(
        (BeforeFirstDeadline, Priority.Medium, false, "all"),
        (BetweenDeadlines, Priority.High, false, "good"),
        (AfterSecondDeadline, Priority.Low, true, "chosen")
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

  test("each priority is twice as likely to be picked as the one below") {
    assertEquals(Priority.High.weight, 2 * Priority.Medium.weight)
    assertEquals(Priority.Medium.weight, 2 * Priority.Low.weight)
  }

  test("a project is picked in proportion to its priority's weight") {
    val items =
      Seq("p1" -> Priority.Low, "p4" -> Priority.Medium, "p5" -> Priority.High)
    val draws = (0 until 700).map(_ / 700.0) // uniform over [0, 1)
    val counts = draws
      .flatMap(Priority.pick(items, _))
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap
    // weights 1 : 2 : 4 of a total of 7
    assertEquals(counts, Map("p1" -> 100, "p4" -> 200, "p5" -> 400))
  }

  test("picking from nothing picks nothing; from one, always that one") {
    assertEquals(Priority.pick(Seq.empty[(String, Priority)], 0.5), None)
    assertEquals(Priority.pick(Seq("p0" -> Priority.Low), 0.99), Some("p0"))
  }

  test("a state survives the cache") {
    for (s <- ProjectState.values)
      assertEquals(
        upickle.default.read[ProjectState](upickle.default.write(s)),
        s
      )
  }
}
