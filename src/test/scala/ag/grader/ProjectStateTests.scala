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

  test(
    "the state counts the deadlines passed, each taking effect at its instant"
  ) {
    assertEquals(at(first.minusMinutes(1)).passed, 0)
    assertEquals(at(first).passed, 1)
    assertEquals(at(second.minusMinutes(1)).passed, 1)
    assertEquals(at(second).passed, 2)
    assertEquals(at(third).passed, 3)
    assertEquals(
      states.map(_.all_milestones).distinct,
      Seq(Seq(a, b, c, Milestone.Live))
    )
  }

  test(
    "live until the final deadline; the current milestone is the first open milestone, then Live"
  ) {
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

  test(
    "an open milestone is feedback: the good tests, stopping at a failure; a closed one runs its own to the end"
  ) {
    val s = states(1) // a closed; b and c open
    assertEquals(
      (s.tests(a), s.shows(a), s.keeps_going(a)),
      (TestSet.Of("a"), TestSet.Of("a"), true)
    )
    for (open <- Seq(b, c))
      assertEquals(
        (s.tests(open), s.shows(open), s.keeps_going(open)),
        (TestSet.Good, TestSet.Good, false)
      )
  }

  test(
    "Live runs the chosen tests to the end and shows every good one, once the final deadline has passed"
  ) {
    for (s <- states) {
      assertEquals(
        (s.tests(Milestone.Live), s.shows(Milestone.Live)),
        (TestSet.Chosen, TestSet.Good)
      )
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
    assertEquals(
      ProjectState
        .at(now, milestones(first, now.minusHours(2), now.minusHours(1)))
        .passed,
      3
    )
    // the first extended
    assertEquals(
      ProjectState.at(now, milestones(now.plusDays(1), second, third)).passed,
      0
    )
  }

  test(
    "the most urgent work goes first, then the fewest tries, then the lowest numbered"
  ) {
    import Work.*
    def pick(items: (String, Work, Int)*) =
      Priority.pick(items.map((n, w, k) => (n, n, Round(w, Seq(), k))))
    // students' before TAs', whatever the tries
    assertEquals(pick(("p1", Closed, 1), ("p3", Open, 5)), Some("p3"))
    assertEquals(pick(("p1", Late, 1), ("p3", Closed, 9)), Some("p3"))
    // first tries before retries
    assertEquals(pick(("p1", Open, 2), ("p3", Open, 1)), Some("p3"))
    assertEquals(pick(("p4", Open, 1), ("p3", Open, 1)), Some("p3"))
    assertEquals(pick(("p10", Closed, 1), ("p9", Closed, 1)), Some("p9"))
    assertEquals(pick(), None)
  }

  test(
    "a project's next round: its most urgent work's milestones furthest behind, one more try"
  ) {
    val a = Milestone.Named("a")
    val b = Milestone.Named("b")
    val st = ProjectState(Seq("a", "b"), 1) // a closed, b open
    // tries each milestone has: work below them
    def behind(tries: (Milestone, Int)*)(m: Milestone, n: Int) =
      tries.toMap.get(m).exists(n > _)
    assertEquals(
      Priority.round(st, 5, behind(a -> 0, b -> 3)),
      Some(Round(Work.Open, Seq(b), 4))
    )
    assertEquals(
      Priority.round(st, 5, behind(a -> 0, b -> 5)),
      Some(Round(Work.Closed, Seq(a), 1))
    )
    assertEquals(Priority.round(st, 5, behind()), None)
    // Live: only once it's published, after the final deadline
    val done = ProjectState(Seq("a", "b"), 2)
    assertEquals(
      Priority.round(done, 5, behind(a -> 2, b -> 1, Milestone.Live -> 0)),
      Some(Round(Work.Closed, Seq(b), 2))
    )
    assertEquals(
      Priority.round(done, 5, behind(a -> 5, b -> 5, Milestone.Live -> 0)),
      Some(Round(Work.Late, Seq(Milestone.Live), 1))
    )
    assertEquals(Priority.round(st, 5, behind(Milestone.Live -> 0)), None)
  }

  test("iterations need both min and max, with 1 <= min <= max") {
    assertEquals(
      upickle.default.read[Iterations]("""{"min": 5, "max": 20}"""),
      Iterations(5, 20)
    )
    assert(
      scala.util
        .Try(upickle.default.read[Iterations]("""{"max": 20}"""))
        .isFailure
    )
    val _ = intercept[IllegalArgumentException](Iterations(min = 0, max = 10))
    val _ = intercept[IllegalArgumentException](Iterations(min = 20, max = 10))
  }

  test("a state survives the cache") {
    for (s <- states)
      assertEquals(
        upickle.default.read[ProjectState](upickle.default.write(s)),
        s
      )
  }
}
