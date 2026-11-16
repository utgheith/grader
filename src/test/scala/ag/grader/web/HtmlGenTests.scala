package ag.grader.web

import ag.grader.Milestone

import ag.grader.name.{Alias, CourseName, ProjectName}

import ag.grader.json.{RawMilestone, Weight, RedactedStudentResults, RedactedOutcome, OutcomeStatus, RedactedTestId, PrepareInfo, CommitSource}

import munit.FunSuite

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

class HtmlGenTests extends FunSuite {
  private val course = CourseName("cs439t_f26")
  private val project = ProjectName("p4")
  private val cutoff = LocalDateTime.of(2026, 10, 1, 23, 59)

  private def student(
      alias: Int,
      late: Boolean,
      from_file: Boolean,
      outcomes: (String, OutcomeStatus)*
  ): (Alias, RedactedStudentResults) = {
    val commit = ZonedDateTime.of(
      if (late) cutoff.plusDays(1) else cutoff.minusDays(1),
      ZoneOffset.UTC
    )
    Alias(alias) -> RedactedStudentResults(
      alias = Some(Alias(alias)),
      has_test = true,
      prepare_info = PrepareInfo(
        commit_time = commit,
        sha = f"$alias%04x" + "beef",
        has_report = true,
        push_time = Some(commit.plusHours(2).toInstant.nn),
        commit_source =
          if (from_file) CommitSource.FromFile("commit_id_2", "abc123")
          else CommitSource.Latest
      ).redacted(ZonedDateTime.of(cutoff, java.time.ZoneId.systemDefault()).nn),
      outcomes = outcomes
        .map((t, s) =>
          RedactedTestId(t) -> RedactedOutcome(
            course,
            project,
            RedactedTestId(t),
            Seq((s, Some(1.5)), (s, Some(2.25)))
          )
        )
        .to(SortedMap)
    )
  }

  import OutcomeStatus.{pass, timeout, unknown}
  private val failed = OutcomeStatus.fail

  // m1, a week before m2
  private def milestones(m1_tests: Seq[String], m2_tests: Seq[String]) = Seq(
    RawMilestone("m1", cutoff.minusDays(7), m1_tests, 1),
    RawMilestone("m2", cutoff, m2_tests, 1)
  )
  private val m1 = Milestone.Named("m1")

  private val data = ReportData(
    title = "cs439t_f26_p4",
    generated = cutoff,
    milestones = milestones(Seq("t0"), Seq("t1")),
    enrollments = 4,
    results = SortedMap(
      student(1, false, false, "t0" -> pass, "t1" -> pass, "<b>'x" -> failed),
      student(2, true, false, "t0" -> failed, "t1" -> timeout, "<b>'x" -> pass),
      student(3, false, true, "t0" -> pass, "t1" -> unknown, "bad" -> pass)
    ),
    weights = Seq(Weight(".*", 1)),
    bad_tests = SortedSet("bad"),
    staff_aliases = Set(Alias(3))
  )

  private lazy val page = HtmlGen.render(data)

  // set REPORT_SAMPLE=/some/file.html to look at (or script-test) the page,
  // and REPORT_SAMPLE_BIG=/other/file.html for a realistically sized one
  sys.env.get("REPORT_SAMPLE").foreach(f => os.write.over(os.Path(f), page))
  // ... and a later version of it, where alias 001 now fails t1
  private lazy val changed = data.copy(results =
    data.results ++ Seq(
      student(1, false, false, "t0" -> pass, "t1" -> failed, "<b>'x" -> failed)
    )
  )
  // ... and two sibling project pages with tabs, into a directory
  private val tabs = Seq(
    "p4" -> "cs439t_f26_p4",
    "p5" -> "cs439t_f26_p5"
  )
  sys.env.get("REPORT_SAMPLE_TABS").foreach { d =>
    os.write.over(
      os.Path(d) / "cs439t_f26_p4.html",
      HtmlGen.render(data.copy(tabs = tabs)),
      createFolders = true
    )
    os.write.over(
      os.Path(d) / "cs439t_f26_p5.html",
      HtmlGen.render(
        changed
          .copy(title = "cs439t_f26_p5", tabs = tabs, staff_aliases = Set())
      ),
      createFolders = true
    )
  }
  sys.env
    .get("REPORT_SAMPLE_V2")
    .foreach(f => os.write.over(os.Path(f), HtmlGen.render(changed)))
  sys.env.get("REPORT_SAMPLE_BIG").foreach { f =>
    val rnd = new scala.util.Random(439)
    val names = (0 until 10).map(i => s"t$i") ++
      (1 to 20).map(_ => rnd.alphanumeric.filter(_.isLower).take(6).mkString)
    def status(): OutcomeStatus = rnd.nextInt(20) match {
      case 0     => timeout
      case 1     => unknown
      case 2 | 3 => failed
      case _     => pass
    }
    // each test has a typical time; each submission a typical speed
    val base = names.map(_ -> (0.2 + rnd.nextDouble() * 6)).toMap
    def with_times(
        r: RedactedStudentResults,
        speed: Double
    ): RedactedStudentResults =
      r.copy(outcomes = r.outcomes.map { (id, o) =>
        id -> o.copy(outcomes =
          o.outcomes.map((st, _) =>
            (
              // some passing tests are flaky
              if (st == pass && rnd.nextInt(12) == 0) failed else st,
              Some(
                base(id.external_name) * speed * (0.9 + rnd.nextDouble() * 0.2)
              )
            )
          )
        )
      })
    val big = data.copy(
      enrollments = 45,
      results = (1 to 40)
        .map { a =>
          val (alias, r) = student(
            a * 7 % 1000,
            rnd.nextInt(8) == 0,
            rnd.nextInt(12) == 0,
            names.map(_ -> status())*
          )
          alias -> with_times(r, math.exp(rnd.nextGaussian() * 0.4))
        }
        .to(SortedMap),
      milestones = milestones(Seq("t0", "t1", "t2", "t3"), Seq("t4", "t5", "t6")),
      bad_tests = SortedSet("t9"),
      staff_aliases = Set(Alias(7))
    )
    os.write.over(os.Path(f), HtmlGen.render(big))
  }

  test("a page's version ignores when it was generated, and can be read back") {
    val later = data.copy(generated = data.generated.plusHours(1).nn)
    assertEquals(HtmlGen.version(later), HtmlGen.version(data))
    assertNotEquals(HtmlGen.render(later), page)
    assertEquals(HtmlGen.version_of(page), Some(HtmlGen.version(data)))
    assertNotEquals(
      HtmlGen.version(data.copy(title = "other")),
      HtmlGen.version(data)
    )
    assertEquals(HtmlGen.version_of("<html></html>"), None)
  }

  test("one row per submission, with the staff row marked") {
    assertEquals("""<tr class="staff"""".r.findAllIn(page).size, 1)
    assertEquals("data-alias=\"".r.findAllIn(page).size, 3)
  }

  test("text and attributes are escaped") {
    assert(!page.contains("<b>'x"))
    // attribute values are double-quoted, so a ' needn't be escaped there
    assert(page.contains("""data-id="&lt;b&gt;'x""""), "in attributes")
    assert(page.contains(""">&lt;b&gt;'x<"""), "in text")
  }

  test("ignored tests get no column") {
    assert(!page.contains("data-id=\"bad\""))
    assert(page.contains("Ignored tests"))
  }

  test("chosen tests come first and are counted per row") {
    val order = """<th class="test[^"]*" data-col="\d+" data-id="([^"]*)"""".r
      .findAllMatchIn(page)
      .map(_.group(1))
      .toList
    assertEquals(order, List("t0", "t1", "&lt;b&gt;'x"))
    assert(page.contains("""data-alias="001" data-chosen="2" data-all="2""""))
    assert(page.contains("""data-alias="002" data-chosen="0" data-all="1""""))
  }

  test("late, commit-id-file and push-gap submissions are marked") {
    assert(page.contains("late commit"))
    assert(page.contains("commit chosen by a commit id file"))
    // its test data pushes 2 hours after the commit: the red flag
    assert(page.contains("pushed over 30 min after it was committed"))
    assert(page.contains("""<span class="mark pushed-late">!</span>"""))
  }

  test("each row shows its commit's short sha, with the details on hover") {
    for (a <- 1 to 3) {
      val sha = f"$a%04x" + "beef"
      assert(page.contains(s">${sha.take(6)}</span>"), s"short sha of $a")
      assert(page.contains(s"title=\"$sha\ncommitted "), s"full sha of $a")
    }
    assert(page.contains("\npushed "), "push time")
    assert(!page.contains("abc123"), "commit id file contents")
  }

  test("tooltips show the success rate and times") {
    assert(page.contains("t0: pass · 2 of 2 tries passed (100%) · 1.50–2.25s"))
  }

  test("a test that fails in any try shows as failing") {
    val (alias, r) = student(1, false, false, "t0" -> pass)
    val flaky = r.copy(outcomes =
      r.outcomes.map((id, o) =>
        id -> o.copy(outcomes = Seq((pass, Some(1.0)), (failed, Some(2.0))))
      )
    )
    val p = HtmlGen.render(data.copy(results = SortedMap(alias -> flaky)))
    assert(p.contains("t0: fail · 1 of 2 tries passed (50%)"))
    assert(p.contains("""data-alias="001" data-chosen="0" data-all="0""""))
    // ... with a bar for its pass rate; tests that always pass or fail have none
    assert(p.contains("""class="cell fail flaky" style="--rate: 0.500""""))
    assert(!page.contains("""class="cell pass flaky""""))
    assert(!"""<td class="cell[^"]* flaky""".r.findFirstIn(page).isDefined)
  }

  test("a try's worst status wins: fail, then timeout, then didn't run") {
    def worst(s: OutcomeStatus*) =
      RedactedOutcome(course, project, RedactedTestId("t"), s.map(_ -> None)).worst
    assertEquals(worst(pass, timeout, failed), Some(failed))
    assertEquals(worst(timeout, pass), Some(timeout))
    assertEquals(worst(pass, unknown), Some(unknown))
    assertEquals(worst(pass, pass), Some(pass))
    assertEquals(worst(), None)
  }

  private def version_of(page: String): String =
    """name="report-version" content="([0-9a-f]+)"""".r
      .findFirstMatchIn(page)
      .map(_.group(1).nn)
      .getOrElse(fail("no version"))

  test("the version ignores when the page was generated") {
    val later =
      HtmlGen.render(data.copy(generated = data.generated.plusHours(3)))
    assertNotEquals(later, page)
    assertEquals(version_of(later), version_of(page))
  }

  test("the version changes when the results do") {
    assertNotEquals(version_of(HtmlGen.render(changed)), version_of(page))
  }

  test("a course's front page links each project, in project order") {
    val index = HtmlGen.render_index(
      "cs439c_f26",
      Seq("cs439c_f26_p10.html", "cs439c_f26_p2.html", "cs439c_f26_p1.html")
    )
    val links = """href="([^"]+)">([^<]+)<""".r
      .findAllMatchIn(index)
      .map(m => (m.group(1), m.group(2)))
      .toList
    assertEquals(
      links,
      List(
        ("cs439c_f26_p1.html", "p1"),
        ("cs439c_f26_p2.html", "p2"),
        ("cs439c_f26_p10.html", "p10")
      )
    )
  }

  test("a course's front page sends visitors on to the latest project") {
    val index = HtmlGen.render_index(
      "cs439c_f26",
      Seq("cs439c_f26_p10.html", "cs439c_f26_p2.html", "cs439c_f26_p1.html")
    )
    assert(index.contains("""location.replace("cs439c_f26_p10.html")"""), index)
    assert(index.contains("""has("all")"""), index)
    assert(!HtmlGen.render_index("cs439c_f26", Nil).contains("location.replace"))
  }

  private def report_data(page: String): ujson.Value =
    ujson.read(
      """(?s)<script type="application/json" id="report-data">(.*?)</script>""".r
        .findFirstMatchIn(page)
        .map(_.group(1).nn)
        .getOrElse(fail("no report data"))
    )

  test("the page embeds per-test results and times for the comparison view") {
    val j = report_data(page)
    assertEquals(j("tests").arr.map(_.str).toList, List("t0", "t1", "<b>'x"))
    assertEquals(j("chosen").arr.map(_.bool).toList, List(true, true, false))
    // [rank, passing tries, tries, median passing time]
    assertEquals(j("rows")("001")(0), ujson.Arr(3, 2, 2, 1.875))
    assertEquals(j("rows")("002")(1), ujson.Arr(2, 0, 2, ujson.Null))
  }

  test("a test named like a closing script tag can't end the data early") {
    val tricky = data.copy(results =
      SortedMap(student(1, false, false, "</script><b>" -> pass))
    )
    val j = report_data(HtmlGen.render(tricky))
    assertEquals(j("tests").arr.map(_.str).toList, List("</script><b>"))
  }

  test("each project page has a tab per project, with its own marked") {
    val p5 = HtmlGen.render(data.copy(title = "cs439t_f26_p5", tabs = tabs))
    assert(p5.contains("""<a href="cs439t_f26_p4.html">p4</a>"""))
    assert(
      p5.contains(
        """<a class="active" href="cs439t_f26_p5.html" aria-current="page">p5</a>"""
      )
    )
    assert(
      !page.contains("""aria-label="projects""""),
      "no project tabs without tabs"
    )
  }

  test("switching projects goes to the other project's main page") {
    val p5 = HtmlGen.render(
      data.copy(title = "cs439t_f26_p5", tabs = tabs, milestone = m1)
    )
    assert(p5.contains("""<a href="cs439t_f26_p4.html">p4</a>"""), p5)
    assert(
      p5.contains(
        """<a class="active" href="cs439t_f26_p5.html" aria-current="page">p5</a>"""
      ),
      p5
    )
  }

  test("every page has a tab per milestone, its own marked; the current one is the main page") {
    // Live, the current milestone
    assert(
      page.contains(
        """<a class="active" href="cs439t_f26_p4.html" aria-current="page">live</a>"""
      )
    )
    assert(page.contains("""<a href="cs439t_f26_p4--m1.html">m1</a>"""))
    assert(page.contains("""<a href="cs439t_f26_p4--m2.html">m2</a>"""))
    assert(page.contains("""data-milestone="live""""))

    val one = HtmlGen.render(data.copy(milestone = m1))
    assert(
      one.contains(
        """<a class="active" href="cs439t_f26_p4--m1.html" aria-current="page">m1</a>"""
      )
    )
    assert(one.contains("""<a href="cs439t_f26_p4.html">live</a>"""))
    assert(one.contains("""data-milestone="m1""""))

    // while m1 is the current milestone, it's the main page
    val current = HtmlGen.render(data.copy(milestone = m1, current = m1))
    assert(current.contains("""<a class="active" href="cs439t_f26_p4.html" aria-current="page">m1</a>"""))
    assert(current.contains("""<a href="cs439t_f26_p4--live.html">live</a>"""))
  }

  test("milestone pages are named after the project, and only the main page is on the index") {
    assertEquals(HtmlGen.page_file("c_p4", Milestone.Live, Milestone.Live), "c_p4.html")
    assertEquals(HtmlGen.page_file("c_p4", m1, Milestone.Live), "c_p4--m1.html")
    assertEquals(HtmlGen.page_file("c_p4", m1, m1), "c_p4.html")
    assertEquals(HtmlGen.page_file("c_p4", Milestone.Live, m1), "c_p4--live.html")
    assert(!HtmlGen.is_milestone_page("c_p4.html"))
    assert(HtmlGen.is_milestone_page("c_p4--m1.html"))
    assert(HtmlGen.is_milestone_page("c_p4--live.html"))
  }

  test("the heading is the project's name; the title stays the full name") {
    val p5 =
      HtmlGen.render(data.copy(title = "cs439t_f26_p5", project_name = "p5"))
    assert(p5.contains("<h1>p5</h1>"))
    assert(p5.contains("<title>cs439t_f26_p5</title>"))
  }

  test("unreadable results show a notice instead of breaking the page") {
    val waiting = HtmlGen.render(
      data.copy(results = SortedMap(), notice = Some("being updated"))
    )
    assert(
      waiting.contains(
        """<div class="card notice" role="status">being updated</div>"""
      )
    )
    assert(!waiting.contains("enrollments"), "no misleading summary")
    assert(
      waiting.contains("""<table class="results">"""),
      "the page's script still finds its grid"
    )
  }
}
