package ag.grader

import ag.grader.json.{RawMilestone, Iterations}

import munit.FunSuite

import java.time.LocalDateTime

class ProjectStateTests extends FunSuite {
  private val first = LocalDateTime.of(2026, 10, 1, 23, 59).nn
  private val second = LocalDateTime.of(2026, 10, 8, 23, 59).nn
  private val third = LocalDateTime.of(2026, 10, 15, 23, 59).nn
  private def milestones(a: LocalDateTime, b: LocalDateTime, c: LocalDateTime) =
    Seq(
      RawMilestone("a", a, Seq("t0"), 1),
      RawMilestone("b", b, Seq("t1"), 1),
      RawMilestone("c", c, Seq("t1", "t2"), 1)
    )
  private def at(t: LocalDateTime) =
    ProjectState.at(t, milestones(first, second, third))
  private val (a, b, c) =
    (Milestone.Named("a"), Milestone.Named("b"), Milestone.Named("c"))
  // the states: before every deadline, after one, after two, after all
  private val states = (0 to 3).map(ProjectState(Seq("a", "b", "c"), _))

  test("the state counts the deadlines passed, each taking effect at its instant") {
    assertEquals(at(first.minusMinutes(1)).passed, 0)
    assertEquals(at(first).passed, 1)
    assertEquals(at(second.minusMinutes(1)).passed, 1)
    assertEquals(at(second).passed, 2)
    assertEquals(at(third).passed, 3)
    assertEquals(states.map(_.all_milestones).distinct, Seq(Seq(a, b, c, Milestone.Live)))
  }

  test("live until the final deadline; the current milestone is the first open milestone, then Live") {
    assertEquals(states.map(_.is_live), Seq(true, true, true, false))
    assertEquals(states.map(_.current), Seq(a, b, c, Milestone.Live))
    assertEquals(
      states.map(_.description),
      Seq(
        "before a's deadline",
        "after a's deadline, before b's",
        "after b's deadline, before c's",
        "after the final deadline"
      )
    )
  }

  test("an open milestone is feedback: the good tests, stopping at a failure; a closed one runs its own to the end") {
    val s = states(1) // a closed; b and c open
    assertEquals((s.tests(a), s.shows(a), s.keeps_going(a)), (TestSet.Of("a"), TestSet.Of("a"), true))
    for (open <- Seq(b, c))
      assertEquals((s.tests(open), s.shows(open), s.keeps_going(open)), (TestSet.Good, TestSet.Good, false))
  }

  test("Live runs the chosen tests to the end and shows every good one, once the final deadline has passed") {
    for (s <- states) {
      assertEquals((s.tests(Milestone.Live), s.shows(Milestone.Live)), (TestSet.Chosen, TestSet.Good))
      assert(s.keeps_going(Milestone.Live))
      // every milestone is published, Live only at the end
      assert(Seq(a, b, c).forall(s.publishes))
      assertEquals(s.publishes(Milestone.Live), !s.is_live)
    }
  }

  test("moving a deadline moves the state") {
    val now = first.plusDays(2)
    assertEquals(at(now).passed, 1)
    // the last two moved earlier
    assertEquals(ProjectState.at(now, milestones(first, now.minusHours(2), now.minusHours(1))).passed, 3)
    // the first extended
    assertEquals(ProjectState.at(now, milestones(now.plusDays(1), second, third)).passed, 0)
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
    // and work pushed after a final deadline waits for all of it
    assertEquals(pick("p1" -> Late, "p3" -> LiveRest), Some("p3"))
    assertEquals(pick(), None)
  }

  test("a project's band and tries come from its iterations and its work") {
    val its = Iterations(min = 10, max = 100)
    // has work below `tries`
    def below(tries: Int)(n: Int) = n > tries
    val none = (_: Int) => false
    assertEquals(Priority.band(true, its, below(3), none), Some((Band.LiveFirst, 10)))
    assertEquals(Priority.band(true, its, below(10), none), Some((Band.LiveRest, 100)))
    assertEquals(Priority.band(true, its, below(100), none), None)
    assertEquals(Priority.band(false, its, below(3), none), Some((Band.Other, 100)))
    assertEquals(Priority.band(false, its, below(100), none), None)
    // Live's work alone is late; with milestones' work it waits its turn
    assertEquals(Priority.band(false, its, none, below(3)), Some((Band.Late, 100)))
    assertEquals(Priority.band(false, its, below(3), below(3)), Some((Band.Other, 100)))
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
    for (s <- states)
      assertEquals(upickle.default.read[ProjectState](upickle.default.write(s)), s)
  }
}
