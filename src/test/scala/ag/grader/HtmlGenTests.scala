package ag.grader

import munit.FunSuite

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

class HtmlGenTests extends FunSuite {
  private val project = Project(Course("cs439t_f26"), "p4")
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
          if (from_file) CommitSource.FromFile("commit_id", "abc123")
          else CommitSource.Latest
      ).redacted(ZonedDateTime.of(cutoff, java.time.ZoneId.systemDefault()).nn),
      outcomes = outcomes
        .map((t, s) =>
          RedactedTestId(t) -> RedactedOutcome(
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

  private val data = ReportData(
    title = "cs439t_f26_p4",
    generated = cutoff,
    test_cutoff = cutoff.minusDays(7),
    code_cutoff = cutoff,
    enrollments = 4,
    results = SortedMap(
      student(1, false, false, "t0" -> pass, "t1" -> pass, "<b>'x" -> failed),
      student(2, true, false, "t0" -> failed, "t1" -> timeout, "<b>'x" -> pass),
      student(3, false, true, "t0" -> pass, "t1" -> unknown, "bad" -> pass)
    ),
    phase1_tests = SortedSet("t0"),
    phase1_weight = 1,
    phase2_tests = SortedSet("t1"),
    phase2_weight = 1,
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
    "p4" -> "cs439t_f26_p4.html",
    "p5" -> "cs439t_f26_p5.html"
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
              st,
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
      phase1_tests = SortedSet("t0", "t1", "t2", "t3"),
      phase2_tests = SortedSet("t4", "t5", "t6"),
      bad_tests = SortedSet("t9"),
      staff_aliases = Set(Alias(7))
    )
    os.write.over(os.Path(f), HtmlGen.render(big))
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
    assert(page.contains("push and commit times differ by over 30 min"))
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

  test("tooltips show tries and times") {
    assert(page.contains("t0: pass · 2 tries · 1.50–2.25s"))
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
    assert(!page.contains("<nav"), "no tab bar without tabs")
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
