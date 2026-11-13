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

  test("the latest results show every good test after the second deadline, but run only the chosen") {
    import ProjectState.*
    assertEquals(
      ProjectState.values.toList.map(s => (s, s.tests(Phase.Latest), s.shows(Phase.Latest))),
      List(
        (BeforeFirstDeadline, "all", "all"),
        (BetweenDeadlines, "good", "good"),
        (AfterSecondDeadline, "chosen", "good")
      )
    )
    // a phase's results show just what they run
    for (s <- ProjectState.values; phase <- Seq(Phase.One, Phase.Two))
      assertEquals(s.shows(phase), s.tests(phase))
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

  test("the most urgent band goes first, then the lowest numbered") {
    import Band.*
    def pick(items: (String, Band)*) =
      Priority.pick(items.map((n, band) => (n, n, band)))
    assertEquals(pick("p1" -> Other, "p3" -> LiveFirst, "p2" -> Other), Some("p3"))
    assertEquals(pick("p4" -> LiveFirst, "p3" -> LiveFirst, "p1" -> Other), Some("p3"))
    // a live project's remaining tries wait for everything else
    assertEquals(pick("p3" -> LiveRest, "p2" -> Other), Some("p2"))
    assertEquals(pick("p3" -> LiveRest, "p4" -> LiveFirst), Some("p4"))
    assertEquals(pick("p4" -> LiveRest, "p3" -> LiveRest), Some("p3"))
    assertEquals(pick("p2" -> Other, "p1" -> Other), Some("p1"))
    assertEquals(pick("p10" -> Other, "p9" -> Other), Some("p9"))
    assertEquals(pick(), None)
  }

  test("a project's band and tries come from its iterations and its work") {
    val its = Iterations(min = 10, max = 100)
    // has work below `tries`
    def below(tries: Int)(n: Int) = n > tries
    assertEquals(Priority.band(true, its, below(3)), Some((Band.LiveFirst, 10)))
    assertEquals(Priority.band(true, its, below(10)), Some((Band.LiveRest, 100)))
    assertEquals(Priority.band(true, its, below(100)), None)
    assertEquals(Priority.band(false, its, below(3)), Some((Band.Other, 100)))
    assertEquals(Priority.band(false, its, below(100)), None)
  }

  test("iterations need both min and max, with 1 <= min <= max") {
    assertEquals(
      upickle.default.read[Iterations]("""{"min": 5, "max": 20}"""),
      Iterations(5, 20)
    )
    assert(scala.util.Try(upickle.default.read[Iterations]("""{"max": 20}""")).isFailure)
    val _ = intercept[IllegalArgumentException](Iterations(min = 0, max = 10))
    val _ = intercept[IllegalArgumentException](Iterations(min = 20, max = 10))
  }

  test("a state survives the cache") {
    for (s <- ProjectState.values)
      assertEquals(
        upickle.default.read[ProjectState](upickle.default.write(s)),
        s
      )
  }
}
