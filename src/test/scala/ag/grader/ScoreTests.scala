package ag.grader

import munit.FunSuite

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

class ScoreTests extends FunSuite {
  private val project = Project(Course("cs439t_f26"), "p4")
  private val cutoff_local = LocalDateTime.of(2026, 10, 1, 23, 59)
  private val cutoff = ZonedDateTime.of(cutoff_local, ZoneId.systemDefault()).nn

  private def results(
      commit: ZonedDateTime,
      runs: (String, Seq[OutcomeStatus])*
  ): RedactedStudentResults = RedactedStudentResults(
    alias = Some(Alias(1)),
    has_test = true,
    prepare_info = PrepareInfo(commit, "abc", true, None, CommitSource.Latest)
      .redacted(cutoff),
    outcomes = runs
      .map((t, ss) =>
        RedactedTestId(t) -> RedactedOutcome(
          project,
          RedactedTestId(t),
          ss.map(_ -> Some(1.0))
        )
      )
      .to(SortedMap)
  )

  // t0 is phase 1 (weight 1), t1 and t2 phase 2 (weights 1 and 3); with
  // phases 1:1, phase 1 is scaled to match phase 2's 4: total 8.
  private val data = ReportData(
    title = "cs439t_f26_p4",
    generated = cutoff_local,
    test_cutoff = cutoff_local.minusDays(7),
    code_cutoff = cutoff_local,
    enrollments = 1,
    results = SortedMap(),
    phase1_tests = SortedSet("t0"),
    phase1_weight = 1,
    phase2_tests = SortedSet("t1", "t2"),
    phase2_weight = 1,
    weights = Seq(Weight("t2", 3), Weight(".*", 1)),
    bad_tests = SortedSet(),
    staff_aliases = Set()
  )

  import OutcomeStatus.{pass, timeout}
  private val failed = OutcomeStatus.fail

  test("lateness is measured in fractional days") {
    assertEqualsDouble(PrepareInfo.days_late(cutoff, cutoff), 0.0, 1e-9)
    assertEqualsDouble(
      PrepareInfo.days_late(cutoff.minusHours(5), cutoff),
      0.0,
      1e-9
    )
    assertEqualsDouble(
      PrepareInfo.days_late(cutoff.plusHours(6), cutoff),
      0.25,
      1e-9
    )
    assertEqualsDouble(
      PrepareInfo.days_late(cutoff.plusHours(36), cutoff),
      1.5,
      1e-9
    )
  }

  test("full marks on time") {
    val r = results(
      cutoff.minusDays(1),
      "t0" -> Seq(pass, pass),
      "t1" -> Seq(pass),
      "t2" -> Seq(pass, pass, pass)
    )
    val s = HtmlGen.score_of(data, r).get
    assertEquals((s.earned, s.possible, s.days_late), (8.0, 8, 0.0))
    assertEqualsDouble(s.correctness, 1.0, 1e-9)
  }

  test("each test earns the fraction of its tries that passed") {
    val r = results(
      cutoff.minusDays(1),
      "t0" -> Seq(pass, failed), // 4 × 1/2
      "t1" -> Seq(timeout), //      1 × 0
      "t2" -> Seq(pass, pass, failed, pass) // 3 × 3/4
    )
    val s = HtmlGen.score_of(data, r).get
    assertEqualsDouble(s.earned, 2.0 + 0.0 + 2.25, 1e-9)
    assertEqualsDouble(s.correctness, 4.25 / 8, 1e-9)
  }

  test("a test that never ran earns nothing") {
    val r = results(cutoff.minusDays(1), "t0" -> Seq(pass))
    assertEqualsDouble(HtmlGen.score_of(data, r).get.correctness, 0.5, 1e-9)
  }

  test("10% off per day late, pro-rated continuously") {
    val all = Seq("t0" -> Seq(pass), "t1" -> Seq(pass), "t2" -> Seq(pass))
    val day = HtmlGen.score_of(data, results(cutoff.plusHours(24), all*)).get
    val six = HtmlGen.score_of(data, results(cutoff.plusHours(6), all*)).get
    val two_and_a_half =
      HtmlGen.score_of(data, results(cutoff.plusHours(60), all*)).get
    assertEqualsDouble(day.correctness, 0.9, 1e-9)
    assertEqualsDouble(six.correctness, math.pow(0.9, 0.25), 1e-9) // 0.974
    assertEqualsDouble(two_and_a_half.correctness, math.pow(0.9, 2.5), 1e-9)
  }

  test("no weights, no score") {
    val none = data.copy(phase1_tests = SortedSet(), phase2_tests = SortedSet())
    assertEquals(HtmlGen.score_of(none, results(cutoff)), None)
  }

  test("push and commit times more than 30 minutes apart are flagged") {
    def info(push: Option[ZonedDateTime]) =
      PrepareInfo(
        cutoff,
        "abc",
        true,
        push.map(_.toInstant.nn),
        CommitSource.Latest
      )
    assertEquals(PrepareInfo.push_gap(info(None)), None)
    assertEquals(PrepareInfo.push_gap(info(Some(cutoff.plusMinutes(29)))), None)
    assertEquals(
      PrepareInfo.push_gap(info(Some(cutoff.plusMinutes(31)))).map(_.toMinutes),
      Some(31L)
    )
    assertEquals(
      PrepareInfo.push_gap(info(Some(cutoff.minusHours(2)))).map(_.toMinutes),
      Some(-120L),
      "a push before the commit's own time is flagged too"
    )
  }

  test("the page shows the score, sortable, with its arithmetic") {
    val r = results(
      cutoff.plusHours(26),
      "t0" -> Seq(pass),
      "t1" -> Seq(pass),
      "t2" -> Seq(pass)
    )
    val page = HtmlGen.render(data.copy(results = SortedMap(Alias(1) -> r)))
    val ratio = math.pow(0.9, 26 / 24.0) // 0.892
    assert(page.contains("""data-key="score""""))
    assert(page.contains(f"""data-score="$ratio%.6f""""))
    assert(
      clue(page).contains(
        f"8.0 of 8 points × 0.9^1.08 (1 day 2 hours late) = $ratio%.3f"
      )
    )
    assert(page.contains(f">$ratio%.3f</td>"))
    assert(page.contains("median correctness"))
  }

}
