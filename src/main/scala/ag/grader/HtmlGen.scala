package ag.grader

import ag.r2.{create_data, say, Target, WithData}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.{SortedMap, SortedSet}

// Everything a report page shows, as plain data (no targets), so pages can
// be rendered -- and tested -- without a grader workspace.
final case class ReportData(
    title: String,
    generated: LocalDateTime,
    test_cutoff: LocalDateTime,
    code_cutoff: LocalDateTime,
    enrollments: Int,
    results: SortedMap[Alias, RedactedStudentResults],
    phase1_tests: SortedSet[String],
    phase1_weight: Int,
    phase2_tests: SortedSet[String],
    phase2_weight: Int,
    weights: Seq[Weight],
    bad_tests: SortedSet[String],
    // highlighted rows: aliases that belong to staff
    staff_aliases: Set[Alias],
    // the course's project pages, as (tab label, file name), in tab order
    tabs: Seq[(String, String)] = Nil,
    // the page's heading; the title if empty
    project_name: String = "",
    // shown instead of the summary when the results can't be shown
    notice: Option[String] = None
)

class HtmlGen(p: Project) {

  // The page's file name, both in site_base and in the course's site repo.
  lazy val page_name: String = s"${p.course.course_name}_${p.project_name}.html"

  // Renders the page into this target's data directory, as `page_name`.
  lazy val page: Target[WithData[Unit]] =
    p.target(
      p.results,
      p.phase1_tests,
      p.phase1_weight,
      p.phase2_tests,
      p.phase2_weight,
      p.test_weights,
      p.bad_tests,
      p.test_cutoff,
      p.code_cutoff,
      p.course.enrollment,
      p.anti_aliases,
      p.course.staff,
      p.staff,
      p.course.active_projects
    ) {
      (
          results,
          phase1_tests,
          phase1_weight,
          phase2_tests,
          phase2_weight,
          weights,
          bad_tests,
          test_cutoff,
          code_cutoff,
          enrollment,
          anti_aliases,
          course_staff,
          project_staff,
          projects
      ) =>
        create_data(_ => false) { dir =>
          val tab_names = (projects.keys.toSeq :+ p.project_name).distinct
            .sortBy(n => (n.length, n))
          val data = ReportData(
            title = s"${p.course.course_name}_${p.project_name}",
            generated = LocalDateTime.now().nn,
            test_cutoff = test_cutoff,
            code_cutoff = code_cutoff,
            enrollments = enrollment.size,
            results = results.toOption.flatten.getOrElse(SortedMap()),
            notice = results.left.toOption.map { why =>
              say(
                s"can't show ${p.course.course_name}_${p.project_name}'s results: $why"
              )
              "The results for this project are being updated; they'll be back shortly."
            },
            phase1_tests = phase1_tests,
            phase1_weight = phase1_weight,
            phase2_tests = phase2_tests,
            phase2_weight = phase2_weight,
            weights = weights,
            bad_tests = bad_tests,
            staff_aliases = anti_aliases.collect {
              case (alias, csid)
                  if course_staff.contains(csid) || project_staff.contains(
                    csid
                  ) =>
                alias
            }.toSet,
            tabs = tab_names.map(n => n -> s"${p.course.course_name}_$n.html"),
            project_name = p.project_name
          )
          os.write.over(dir / page_name, HtmlGen.render(data))
        }
    }

  // Copies the page into site_base, if there is one.
  val gen_html: Target[Unit] =
    p.target(page, Config.site_base) { (page, site_base) =>
      site_base.foreach { site_base =>
        val f = os.Path(site_base) / page_name
        say(s"  ${f.toString}")
        os.copy.over(page.get_data_path / page_name, f, createFolders = true)
        os.perms.set(f, os.PermSet.fromString("rwxr--r--"))
      }
    }

}

object HtmlGen {
  // ScalaTags: typed tags and attributes; text and attribute values are
  // escaped, and only `raw` (the page's own CSS, JS and JSON) is not.
  import scalatags.Text.all.*
  import scalatags.Text.tags2

  private val display_format: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm (EEEE)").nn

  // commit and push times, both in the grader's time zone
  private val time_format: DateTimeFormatter =
    DateTimeFormatter
      .ofPattern("yyyy-MM-dd HH:mm:ss z")
      .nn
      .withZone(java.time.ZoneId.systemDefault())
      .nn

  // The fixed (non-test) columns at the start of every row; the page's
  // script relies on this count.
  private val fixed_columns = 5

  // ---- Correctness score
  //
  // Each chosen test is worth its weight from `test_weights`, scaled per
  // phase as in the "Selected test weights" table, and earns that weight
  // times the fraction of its tries that passed. The score is the earned
  // share of all the weight, times 0.9 per day (continuously: 0.9^(hours/24))
  // the commit is past the phase 2 (code) cutoff.

  final case class Score(
      earned: Double, // weighted, before the late penalty
      possible: Int,
      days_late: Double,
      correctness: Double // 0..1, after the late penalty
  )

  // e.g. "1 day 2 hours", "45 minutes"
  private def describe_days(days: Double): String = {
    val minutes = math.round(days * 24 * 60)
    def unit(n: Long, what: String) = s"$n $what${if (n == 1) "" else "s"}"
    if (minutes < 60) unit(minutes, "minute")
    else {
      val d = minutes / (24 * 60)
      val h = minutes / 60 % 24
      List(
        Option.when(d > 0)(unit(d, "day")),
        Option.when(h > 0)(unit(h, "hour"))
      ).flatten
        .mkString(" ")
    }
  }

  def score_of(d: ReportData, r: RedactedStudentResults): Option[Score] =
    score_with(compute_weights(d), r)

  private def score_with(
      weights: Weights,
      r: RedactedStudentResults
  ): Option[Score] = {
    val possible = weights.phase1_total + weights.phase2_total
    Option.when(possible > 0) {
      val earned = weights.rows.map { row =>
        val runs = r.outcomes
          .get(RedactedTestId(row.test))
          .map(_.outcomes)
          .getOrElse(Nil)
        val fraction =
          if (runs.isEmpty) 0.0
          else runs.count(_._1 == OutcomeStatus.pass).toDouble / runs.size
        (row.phase1 + row.phase2) * fraction
      }.sum
      val days = r.prepare_info.days_late
      Score(
        earned,
        possible,
        days,
        earned / possible * math.pow(0.9, days)
      )
    }
  }

  // How a test's last run is shown: css class, glyph, word, sort rank
  private def status_of(
      o: Option[OutcomeStatus]
  ): (String, String, String, Int) = o match {
    case Some(OutcomeStatus.pass)           => ("pass", "\u00b7", "pass", 3)
    case Some(OutcomeStatus.timeout)        => ("timeout", "T", "timeout", 2)
    case Some(OutcomeStatus.fail)           => ("fail", "✗", "fail", 1)
    case Some(OutcomeStatus.unknown) | None =>
      ("unknown", "?", "didn't run", 0)
  }

  private final case class WeightRow(
      test: String,
      weight: Option[Int],
      phase1: Int,
      phase2: Int
  )

  private final case class Weights(
      rows: Seq[WeightRow],
      phase1_factor: Int,
      phase2_factor: Int,
      phase1_total: Int,
      phase2_total: Int
  )

  // Scales the two phases' raw weights so they keep the configured
  // phase1:phase2 ratio, using the lcm of the raw sums.
  private def compute_weights(d: ReportData): Weights = {
    // chosen = (phase 1 + phase 2) - ignored, as in Project.chosen_test_ids
    val selected = (d.phase1_tests ++ d.phase2_tests).toList
      .filterNot(d.bad_tests.contains)
      .sortBy(c => (c.length, c))
    def weight_of(c: String): Option[Int] =
      d.weights
        .find(w => scala.util.matching.Regex(w.pattern).matches(c))
        .map(_.weight)

    val phase1_raw =
      selected.filter(d.phase1_tests.contains).flatMap(weight_of).sum
    val phase2_raw =
      selected.filter(d.phase2_tests.contains).flatMap(weight_of).sum
    val factor =
      if (phase1_raw == 0) phase2_raw
      else if (phase2_raw == 0) phase1_raw
      else (phase1_raw / BigInt(phase1_raw).gcd(phase2_raw).toInt) * phase2_raw
    val phase1_total = d.phase1_weight * factor
    val phase2_total = d.phase2_weight * factor
    val phase1_factor = if (phase1_raw != 0) phase1_total / phase1_raw else 0
    val phase2_factor = if (phase2_raw != 0) phase2_total / phase2_raw else 0

    val rows = selected.map { c =>
      val w = weight_of(c)
      WeightRow(
        test = c,
        weight = w,
        phase1 =
          if (d.phase1_tests.contains(c)) w.getOrElse(0) * phase1_factor
          else 0,
        phase2 =
          if (d.phase2_tests.contains(c)) w.getOrElse(0) * phase2_factor
          else 0
      )
    }
    Weights(rows, phase1_factor, phase2_factor, phase1_total, phase2_total)
  }

  // A page's version: a hash of everything it shows except the time it was
  // generated, so regenerating unchanged results doesn't look like a change.
  // The page polls for itself and swaps in a copy whose version differs.
  def render(d: ReportData): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256").nn
    val bytes = render_with(d.copy(generated = LocalDateTime.MIN.nn), "")
      .getBytes("UTF-8")
    val version =
      digest.digest(bytes).nn.take(8).map(b => f"${b & 0xff}%02x").mkString
    render_with(d, version)
  }

  private def render_with(d: ReportData, version: String): String = {
    val chosen: Set[String] = (d.phase1_tests ++ d.phase2_tests).toSet

    // chosen tests first, then by name length, then by name
    val tests: Seq[String] = d.results.values.toSeq
      .flatMap(_.outcomes.keys.map(_.external_name))
      .distinct
      .filterNot(d.bad_tests.contains)
      .sortBy(t => (!chosen.contains(t), t.length, t))
    val chosen_count = tests.count(chosen.contains)

    val rows: Seq[(Alias, RedactedStudentResults)] =
      d.results.toSeq.filter(_._2.alias.isDefined)

    // last status of every test, per row
    val statuses: Map[Alias, Map[String, Option[OutcomeStatus]]] =
      rows.map { (alias, r) =>
        alias -> r.outcomes.map((k, v) => k.external_name -> v.last).toMap
      }.toMap

    def passed(alias: Alias, among: Seq[String]): Int =
      among.count(t =>
        statuses(alias).get(t).flatten.contains(OutcomeStatus.pass)
      )

    val chosen_tests = tests.filter(chosen.contains)
    val all_chosen_passing =
      if (chosen_tests.isEmpty) 0
      else rows.count((alias, _) => passed(alias, chosen_tests) == chosen_count)

    val weights = compute_weights(d)
    val scores: Map[Alias, Score] =
      rows
        .flatMap((alias, r) => score_with(weights, r).map(alias -> _))
        .toMap

    // Per submission, per test (in `tests` order): [result rank, passing
    // tries, tries, median passing time or null]. The comparison view is
    // computed from this in the browser.
    def median(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else {
        val s = xs.sorted
        val m = s.size / 2
        Some(if (s.size % 2 == 1) s(m) else (s(m - 1) + s(m)) / 2)
      }
    val report_json: String = {
      val per_submission = rows.map { (alias, r) =>
        alias.toString -> ujson.Arr.from(tests.map { t =>
          val runs =
            r.outcomes.get(RedactedTestId(t)).map(_.outcomes).getOrElse(Nil)
          val time = median(runs.collect {
            case (OutcomeStatus.pass, Some(secs)) => secs
          })
          ujson.Arr(
            status_of(statuses(alias).get(t).flatten)._4,
            runs.count(_._1 == OutcomeStatus.pass),
            runs.size,
            time.fold(ujson.Null)(t => ujson.Num(math.round(t * 1000) / 1000.0))
          )
        })
      }
      ujson
        .write(
          ujson.Obj(
            "tests" -> ujson.Arr.from(tests.map(ujson.Str(_))),
            "chosen" -> ujson.Arr.from(
              tests.map(t => ujson.Bool(chosen.contains(t)))
            ),
            "rows" -> ujson.Obj.from(per_submission),
            "scores" -> ujson.Obj.from(rows.map { (alias, _) =>
              alias.toString -> scores
                .get(alias)
                .fold(ujson.Null)(s =>
                  ujson.Num(math.round(s.correctness * 1000) / 1000.0)
                )
            })
          )
        )
        // no raw "<" (e.g. "</script>" in a test name) inside the <script>;
        // "<" only ever appears inside JSON strings, where \u003c means the same
        .replace("<", "\\u003c")
        .nn
    }

    def stat(label: String, amount: String, note: String = ""): Frag =
      div(cls := "stat")(
        div(cls := "stat-value")(amount),
        div(cls := "stat-label")(
          label,
          Option.when(note.nonEmpty)(span(cls := "stat-note")(s" $note"))
        )
      )

    def results_table: Frag =
      table(cls := "results")(
        thead(
          tr(cls := "head")(
            th(cls := "pin-col", title := "pin rows to the top")("⚑"),
            th(cls := "alias")(
              button(cls := "sort", data("key") := "alias")("alias")
            ),
            th(cls := "num", title := "chosen tests passed")(
              button(cls := "sort", data("key") := "chosen")(
                "chosen",
                span(cls := "of")(s"/$chosen_count")
              )
            ),
            th(cls := "num", title := "all tests passed")(
              button(cls := "sort", data("key") := "all")(
                "all",
                span(cls := "of")(s"/${tests.size}")
              )
            ),
            th(
              cls := "num score",
              title := "experimental: correctness (0 to 1), after any late penalty; may change"
            )(
              button(cls := "sort", data("key") := "score")("correctness"),
              span(cls := "exp")("experimental")
            ),
            for ((t, i) <- tests.zipWithIndex)
              yield th(
                cls := (if (chosen.contains(t)) "test chosen" else "test opt"),
                data("col") := i.toString,
                data("id") := t
              )(
                button(
                  cls := "sort",
                  data("key") := s"test:$t",
                  title := s"sort by $t"
                )(span(cls := "vname")(t)),
                input(
                  cls := "colpin",
                  tpe := "checkbox",
                  data("id") := t,
                  title := s"pin $t to the left",
                  aria.label := s"pin $t"
                )
              )
          )
        ),
        tbody(
          for ((alias, r) <- rows) yield {
            val info = r.prepare_info
            val is_late = info.days_late > 0
            val n_chosen = passed(alias, chosen_tests)
            val n_all = passed(alias, tests)
            val score = scores.get(alias)

            tr(
              Option.when(d.staff_aliases.contains(alias))(cls := "staff"),
              data("alias") := alias.toString,
              data("chosen") := n_chosen.toString,
              data("all") := n_all.toString,
              data("score") := score.fold("-1")(s => f"${s.correctness}%.6f")
            )(
              td(cls := "pin-col")(
                input(
                  cls := "rowpin",
                  tpe := "checkbox",
                  title := "pin to the top",
                  aria.label := s"pin ${alias.toString}"
                )
              ),
              td(
                cls := (if (info.from_commit_file) "alias from-file"
                        else "alias"),
                title := List(
                  Option.when(is_late)("late commit"),
                  Option.when(info.from_commit_file)(
                    "commit chosen by a commit id file"
                  ),
                  Option.when(info.push_gap)(
                    "push and commit times differ by over 30 min"
                  )
                ).flatten.mkString(" · ")
              )(
                alias.toString,
                Option.when(is_late)(span(cls := "mark late")("*")),
                Option.when(info.from_commit_file)(span(cls := "mark")("†")),
                Option.when(info.push_gap)(span(cls := "mark warn")("!")),
                span(
                  cls := "sha",
                  title := (List(
                    info.sha,
                    s"committed ${time_format.format(info.commit_time)}"
                  ) ++ info.push_time.map(t =>
                    s"pushed ${time_format.format(t)}"
                  )).mkString("\n")
                )(info.sha.take(6))
              ),
              td(
                cls := (if (chosen_count > 0 && n_chosen == chosen_count)
                          "num full"
                        else "num")
              )(n_chosen.toString),
              td(cls := "num")(n_all.toString),
              td(
                cls := "num score",
                score.map { s =>
                  val late =
                    if (s.days_late == 0) ""
                    else
                      f" × 0.9^${s.days_late}%.2f (${describe_days(s.days_late)} late)"
                  title := f"${s.earned}%.1f of ${s.possible} points$late = ${s.correctness}%.3f"
                }
              )(score.fold("–")(s => f"${s.correctness}%.3f")),
              for ((t, i) <- tests.zipWithIndex) yield {
                val o = r.outcomes.get(RedactedTestId(t))
                val (status_cls, glyph, word, rank) =
                  status_of(statuses(alias).get(t).flatten)
                val tries = o.map(_.outcomes.size).filter(_ > 0)
                val times = o.flatMap(_.min_max).map { (lo, hi) =>
                  if (lo == hi) f"$lo%.2fs" else f"$lo%.2f–$hi%.2fs"
                }
                td(
                  cls := (s"cell $status_cls" + (if (chosen.contains(t)) ""
                                                 else " opt")),
                  data("col") := i.toString,
                  data("r") := rank.toString,
                  title := (List(s"$t: $word") ++
                    tries.map(n => s"$n ${if (n == 1) "try" else "tries"}") ++
                    times).mkString(" · ")
                )(glyph)
              }
            )
          }
        )
      )

    def legend_list: Frag =
      ul(cls := "legend")(
        li(span(cls := "cell pass")("·"), " pass"),
        li(span(cls := "cell fail")("✗"), " fail"),
        li(span(cls := "cell timeout")("T"), " timeout"),
        li(span(cls := "cell unknown")("?"), " didn't run"),
        li(span(cls := "swatch chosen")(""), " chosen test"),
        li(span(cls := "mark late")("*"), " late commit"),
        li(span(cls := "mark")("†"), " commit chosen by a commit id file"),
        li(
          span(cls := "mark warn")("!"),
          " push and commit times differ by over 30 min"
        ),
        li(span(cls := "swatch staff")(""), " staff")
      )

    def weights_card: Frag =
      tags2.section(cls := "card")(
        h2("Selected test weights"),
        table(cls := "weights")(
          thead(
            tr(
              th("test"),
              th(cls := "num")("weight"),
              th(cls := "num")(s"phase 1 (×${weights.phase1_factor})"),
              th(cls := "num")(s"phase 2 (×${weights.phase2_factor})")
            )
          ),
          tbody(
            for (w <- weights.rows)
              yield tr(
                td(w.test),
                td(cls := "num")(w.weight.fold("?")(_.toString)),
                td(cls := "num")(w.phase1.toString),
                td(cls := "num")(w.phase2.toString)
              )
          ),
          tfoot(
            tr(
              td("total"),
              td(""),
              td(cls := "num")(weights.phase1_total.toString),
              td(cls := "num")(weights.phase2_total.toString)
            )
          )
        ),
        div(cls := "note")(
          s"Overall: ${weights.phase1_total} + ${weights.phase2_total} = " +
            s"${weights.phase1_total + weights.phase2_total}; " +
            s"phase 1 : phase 2 = ${d.phase1_weight} : ${d.phase2_weight}"
        ),
        div(cls := "note")(
          span(cls := "exp")("experimental"),
          " Correctness (0 to 1) may change. Each test earns its weight times the fraction " +
            "of its tries that passed; correctness is the share of all the weight earned, " +
            "times 0.9 per day the commit is past the phase 2 cutoff, " +
            "pro-rated by the hour (0.9 ^ (hours late / 24))."
        )
      )

    val median_correctness: Option[Double] =
      Option.when(scores.nonEmpty) {
        val sorted = scores.values.map(_.correctness).toSeq.sorted
        val m = sorted.size / 2
        if (sorted.size % 2 == 1) sorted(m) else (sorted(m - 1) + sorted(m)) / 2
      }

    page(
      head(
        meta(charset := "utf-8"),
        meta(
          name := "viewport",
          content := "width=device-width,initial-scale=1"
        ),
        meta(name := "report-version", content := version),
        tags2.title(d.title),
        tags2.style(raw(css))
      ),
      body(data("assignment") := d.title)(
        header(cls := "top")(
          h1(if (d.project_name.nonEmpty) d.project_name else d.title),
          Option.when(d.tabs.nonEmpty)(
            tags2.nav(cls := "tabs", aria.label := "projects")(
              for ((tab, file) <- d.tabs) yield {
                val here = file == s"${d.title}.html"
                a(
                  Option.when(here)(cls := "active"),
                  href := file,
                  Option.when(here)(attr("aria-current") := "page")
                )(tab)
              }
            )
          ),
          dl(cls := "dates")(
            div(dt("generated"), dd(display_format.format(d.generated).nn)),
            div(
              dt("phase 1 cutoff"),
              dd(display_format.format(d.test_cutoff).nn)
            ),
            div(
              dt("phase 2 cutoff"),
              dd(display_format.format(d.code_cutoff).nn)
            )
          )
        ),
        tags2.main(
          d.notice.map(n => div(cls := "card notice", role := "status")(n)),
          Option.when(d.notice.isEmpty)(
            tags2.section(cls := "stats")(
              stat("enrollments", d.enrollments.toString),
              stat("submissions", rows.size.toString),
              stat("tests", tests.size.toString, s"($chosen_count chosen)"),
              Option.when(chosen_count > 0)(
                stat("pass every chosen test", all_chosen_passing.toString)
              ),
              median_correctness.map(m =>
                stat("median correctness", f"$m%.3f", "(experimental)")
              )
            )
          ),
          tags2.section(cls := "card grid-card")(
            div(cls := "toolbar")(
              input(
                id := "filter",
                tpe := "search",
                placeholder := "Filter aliases…",
                aria.label := "filter aliases"
              ),
              label(cls := "toggle")(
                input(id := "only-chosen", tpe := "checkbox"),
                " chosen tests only"
              ),
              button(id := "clear-pins", tpe := "button")("Clear pins"),
              legend_list
            ),
            div(cls := "table-wrap")(results_table),
            div(cls := "note")(
              "Click a column heading to sort; click again to reverse. " +
                "Click an alias to highlight its row and compare it below. " +
                "Pins are remembered in this browser. " +
                "The page checks for new results every minute and updates in place."
            )
          ),
          tags2.section(cls := "card", id := "compare")(
            div(cls := "compare-head")(
              h2("Compare a submission"),
              label(
                "submission ",
                select(id := "compare-pick")(
                  option(value := "")("—"),
                  for ((alias, _) <- rows)
                    yield option(value := alias.toString)(alias.toString)
                )
              ),
              label(cls := "toggle")(
                input(id := "notable-only", tpe := "checkbox"),
                " only tests that stand out"
              )
            ),
            div(id := "compare-body"),
            script(tpe := "application/json", id := "report-data")(
              raw(report_json)
            )
          ),
          div(cls := "cards")(
            Option.when(d.bad_tests.nonEmpty)(
              tags2.section(cls := "card")(
                h2("Ignored tests"),
                ul(cls := "plain")(d.bad_tests.toSeq.map(t => li(t)))
              )
            ),
            weights_card
          )
        ),
        script(raw(js.replace("FIXED_COLUMNS", fixed_columns.toString).nn))
      )
    )
  }

  // A whole document: ScalaTags doesn't write the doctype.
  private def page(content: Modifier*): String =
    "<!DOCTYPE html>\n" + html(lang := "en")(content*).render

  // A course site's front page: a link to each project's page.
  def render_index(course_name: String, pages: Seq[String]): String = {
    val sorted = pages.sortBy(f => (f.length, f))
    page(
      head(
        meta(charset := "utf-8"),
        meta(
          name := "viewport",
          content := "width=device-width,initial-scale=1"
        ),
        tags2.title(course_name),
        tags2.style(raw(css))
      ),
      body(
        header(cls := "top")(h1(course_name)),
        tags2.main(
          tags2.section(cls := "card")(
            h2("Projects"),
            if (sorted.isEmpty) div(cls := "note")("No projects yet.")
            else
              ul(cls := "projects")(
                for (f <- sorted) yield {
                  val project =
                    f.stripPrefix(s"${course_name}_").stripSuffix(".html")
                  li(a(href := f)(project))
                }
              )
          )
        )
      )
    )
  }

  private val css: String = """
    |:root {
    |  color-scheme: light dark;
    |  --bg: #f5f6f8;
    |  --surface: #ffffff;
    |  --surface-2: #f3f4f7;
    |  --text: #1b1f24;
    |  --muted: #5f6b7a;
    |  --border: #dde1e6;
    |  --accent: #3563d0;
    |  --pass-bg: #dcf2e2; --pass-fg: #17652f;
    |  --fail-bg: #fadcdc; --fail-fg: #a11d1d;
    |  --timeout-bg: #fcecc8; --timeout-fg: #7f4c00;
    |  --unknown-bg: #eceef1; --unknown-fg: #6b7280;
    |  --chosen: #e4ecff;
    |  --staff: #fff2b3;
    |  --marked: #ffe27a;
    |}
    |@media (prefers-color-scheme: dark) {
    |  :root {
    |    --bg: #131518;
    |    --surface: #1c1f23;
    |    --surface-2: #23272d;
    |    --text: #e6e8eb;
    |    --muted: #9aa4b1;
    |    --border: #343a42;
    |    --accent: #82a7ff;
    |    --pass-bg: #163824; --pass-fg: #7fe0a1;
    |    --fail-bg: #4a1c1c; --fail-fg: #ff9d9d;
    |    --timeout-bg: #44320d; --timeout-fg: #f3c36a;
    |    --unknown-bg: #2a2f35; --unknown-fg: #9aa4b1;
    |    --chosen: #1e2a45;
    |    --staff: #433b12;
    |    --marked: #5a4f14;
    |  }
    |}
    |* { box-sizing: border-box; }
    |body {
    |  margin: 0;
    |  background: var(--bg);
    |  color: var(--text);
    |  font: 14px/1.45 system-ui, -apple-system, "Segoe UI", Roboto, sans-serif;
    |}
    |header.top {
    |  display: flex; flex-wrap: wrap; align-items: baseline; gap: 0.5rem 2rem;
    |  padding: 1.25rem clamp(1rem, 3vw, 2rem) 0.75rem;
    |}
    |h1 { margin: 0; font-size: 1.6rem; font-weight: 650; letter-spacing: -0.01em; }
    |h2 { margin: 0 0 0.75rem; font-size: 1rem; font-weight: 600; }
    |.dates { display: flex; flex-wrap: wrap; gap: 0.25rem 1.5rem; margin: 0; color: var(--muted); }
    |nav.tabs { order: 3; flex-basis: 100%; display: flex; flex-wrap: wrap; gap: 0.25rem; margin-top: 0.5rem; border-bottom: 1px solid var(--border); }
    |nav.tabs a {
    |  padding: 0.4rem 0.95rem; margin-bottom: -1px; border: 1px solid transparent; border-bottom: none;
    |  border-radius: 8px 8px 0 0; color: var(--muted); text-decoration: none; font-weight: 600;
    |}
    |nav.tabs a:hover { color: var(--accent); }
    |nav.tabs a:focus-visible { outline: 2px solid var(--accent); outline-offset: -2px; }
    |nav.tabs a.active { color: var(--text); background: var(--surface); border-color: var(--border); }
    |.dates div { display: flex; gap: 0.4rem; }
    |.dates dt::after { content: ":"; }
    |.dates dd { margin: 0; color: var(--text); font-variant-numeric: tabular-nums; }
    |main { padding: 0 clamp(1rem, 3vw, 2rem) 2rem; display: grid; gap: 1rem; }
    |.stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(9rem, 1fr)); gap: 0.75rem; }
    |.stat, .card {
    |  background: var(--surface); border: 1px solid var(--border); border-radius: 10px;
    |}
    |.stat { padding: 0.75rem 1rem; }
    |.stat-value { font-size: 1.5rem; font-weight: 650; font-variant-numeric: tabular-nums; }
    |.stat-label { color: var(--muted); }
    |.card { padding: 1rem; min-width: 0; }
    |.cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(18rem, 1fr)); gap: 1rem; align-items: start; }
    |.note { color: var(--muted); font-size: 0.85rem; margin-top: 0.6rem; }
    |.card.notice { background: var(--timeout-bg); color: var(--timeout-fg); border-color: var(--timeout-fg); font-weight: 600; }
    |
    |.toolbar { display: flex; flex-wrap: wrap; align-items: center; gap: 0.5rem 1rem; margin-bottom: 0.75rem; }
    |.toolbar input[type=search] {
    |  font: inherit; padding: 0.35rem 0.6rem; min-width: 12rem;
    |  border: 1px solid var(--border); border-radius: 6px; background: var(--surface-2); color: var(--text);
    |}
    |.toolbar button {
    |  font: inherit; padding: 0.3rem 0.7rem; cursor: pointer;
    |  border: 1px solid var(--border); border-radius: 6px; background: var(--surface-2); color: var(--text);
    |}
    |.toggle { display: inline-flex; align-items: center; gap: 0.25rem; cursor: pointer; }
    |input[type=checkbox] { accent-color: var(--accent); }
    |.legend { display: flex; flex-wrap: wrap; gap: 0.25rem 0.9rem; list-style: none; margin: 0 0 0 auto; padding: 0; color: var(--muted); font-size: 0.85rem; }
    |.legend li { display: inline-flex; align-items: center; gap: 0.2rem; }
    |.legend .cell { display: inline-block; width: 1.4em; text-align: center; border-radius: 3px; }
    |.swatch { display: inline-block; width: 1em; height: 1em; border-radius: 3px; border: 1px solid var(--border); }
    |.swatch.chosen { background: var(--chosen); }
    |.swatch.staff { background: var(--staff); }
    |
    |.table-wrap { overflow: auto; max-height: 75vh; border: 1px solid var(--border); border-radius: 8px; }
    |.results { border-collapse: separate; border-spacing: 0; font: 12px/1.2 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
    |.results th, .results td { padding: 0; background: var(--surface); }
    |.results thead { position: sticky; top: 0; z-index: 3; }
    |.results thead tr.head th { border-bottom: 1px solid var(--border); vertical-align: bottom; }
    |.results thead tr:not(.head) td { border-bottom: 1px solid var(--border); }
    |.results .pin-col { position: sticky; left: 0; z-index: 1; width: 1.9rem; min-width: 1.9rem; text-align: center; color: var(--muted); }
    |.results .alias { position: sticky; left: 1.9rem; z-index: 1; padding: 0 0.6rem 0 0.2rem; white-space: nowrap; text-align: left; border-right: 1px solid var(--border); }
    |.results td.alias { cursor: pointer; font-weight: 600; }
    |.results td.from-file { font-style: italic; }
    |.results .sha { color: var(--muted); font-weight: 400; margin-left: 0.35em; }
    |.mark { color: var(--accent); font-weight: 700; margin-left: 0.1em; }
    |.exp { display: inline-block; margin-left: 0.4em; padding: 0 0.4em; border: 1px solid var(--timeout-fg); border-radius: 999px; color: var(--timeout-fg); font: 600 0.65rem/1.5 system-ui, sans-serif; letter-spacing: 0.03em; text-transform: uppercase; vertical-align: middle; }
    |.results th .exp { display: block; margin: 0.15em 0 0 auto; width: max-content; }
    |.mark.late { color: var(--fail-fg); }
    |.results .num { padding: 0 0.45rem; text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
    |.results td.num.full { color: var(--pass-fg); font-weight: 700; }
    |.results td.num.score { font-weight: 700; }
    |.mark.warn { color: var(--timeout-fg); }
    |.results th.num + th.test, .results td.num + td.cell { border-left: 1px solid var(--border); }
    |.results .of { color: var(--muted); font-weight: 400; }
    |
    |.results th.test { padding: 0.3rem 0 0.2rem; min-width: 1.7em; text-align: center; font-weight: 500; }
    |.results th.test.chosen { background: var(--chosen); }
    |.results th.test.pinned { box-shadow: inset 0 -2px 0 var(--accent); }
    |.results .vname {
    |  display: inline-block; writing-mode: vertical-rl; transform: rotate(180deg);
    |  white-space: nowrap; max-height: 9rem; overflow: hidden; text-overflow: ellipsis;
    |}
    |.results th input { display: block; margin: 0.25rem auto 0; }
    |button.sort {
    |  all: unset; cursor: pointer; border-radius: 4px; padding: 0.1rem 0.15rem;
    |}
    |button.sort:hover { color: var(--accent); }
    |button.sort:focus-visible { outline: 2px solid var(--accent); outline-offset: 1px; }
    |th[aria-sort=ascending] button.sort::after { content: " \25B2"; font-size: 0.7em; }
    |th[aria-sort=descending] button.sort::after { content: " \25BC"; font-size: 0.7em; }
    |th.test[aria-sort] button.sort::after { display: block; }
    |
    |.results td.cell { width: 1.7em; min-width: 1.7em; height: 1.55em; text-align: center; border: 1px solid var(--surface); border-width: 0 1px 1px 0; }
    |.results td.pass { background: var(--pass-bg); color: var(--pass-fg); }
    |.results td.fail { background: var(--fail-bg); color: var(--fail-fg); font-weight: 700; }
    |.results td.timeout { background: var(--timeout-bg); color: var(--timeout-fg); font-weight: 700; }
    |.results td.unknown { background: var(--unknown-bg); color: var(--unknown-fg); }
    |.legend .pass, .cmp .pass, .flag.good { background: var(--pass-bg); color: var(--pass-fg); }
    |.legend .fail, .cmp .fail, .flag.bad { background: var(--fail-bg); color: var(--fail-fg); }
    |.legend .timeout, .cmp .timeout, .flag.warn { background: var(--timeout-bg); color: var(--timeout-fg); }
    |.legend .unknown, .cmp .unknown { background: var(--unknown-bg); color: var(--unknown-fg); }
    |
    |.compare-head { display: flex; flex-wrap: wrap; align-items: center; gap: 0.5rem 1.25rem; margin-bottom: 0.75rem; }
    |.compare-head h2 { margin: 0; }
    |.compare-head select {
    |  font: inherit; padding: 0.3rem 0.5rem; border: 1px solid var(--border); border-radius: 6px;
    |  background: var(--surface-2); color: var(--text);
    |}
    |.summary { display: grid; grid-template-columns: repeat(auto-fit, minmax(13rem, 1fr)); gap: 0.75rem; margin-bottom: 1rem; }
    |.summary .stat { background: var(--surface-2); }
    |.summary .stat-value small { font-size: 0.9rem; font-weight: 400; color: var(--muted); }
    |.hist { display: block; width: 100%; height: 3.5rem; margin-top: 0.4rem; }
    |.hist rect { fill: var(--border); }
    |.hist rect.me { fill: var(--accent); }
    |.hist-axis { display: flex; justify-content: space-between; color: var(--muted); font-size: 0.75rem; border-top: 1px solid var(--border); }
    |.cmp-wrap { overflow-x: auto; }
    |.cmp { border-collapse: collapse; width: 100%; font-variant-numeric: tabular-nums; }
    |.cmp th, .cmp td { padding: 0.3rem 0.6rem; border-bottom: 1px solid var(--border); text-align: left; white-space: nowrap; }
    |.cmp th { color: var(--muted); font-weight: 600; }
    |.cmp .num { text-align: right; }
    |.cmp tr.is-chosen td:first-child { box-shadow: inset 3px 0 0 var(--accent); }
    |.cmp .cell { display: inline-block; width: 1.5em; text-align: center; border-radius: 3px; margin-right: 0.4em; font-family: ui-monospace, Menlo, Consolas, monospace; }
    |.meter { display: inline-block; width: 4.5rem; height: 0.45rem; margin-right: 0.45em; vertical-align: middle; background: var(--surface-2); border: 1px solid var(--border); border-radius: 999px; overflow: hidden; }
    |.meter > span { display: block; height: 100%; background: var(--pass-fg); opacity: 0.55; }
    |.rel.slow { color: var(--fail-fg); }
    |.rel.fast { color: var(--pass-fg); }
    |.flag { display: inline-block; padding: 0 0.5em; margin-right: 0.3em; border-radius: 999px; font-size: 0.8rem; }
    |
    |.results tbody tr:nth-child(even) td:not(.cell) { background: var(--surface-2); }
    |.results tr.staff td:not(.cell) { background: var(--staff) !important; }
    |.results tr.marked td:not(.cell) { background: var(--marked) !important; }
    |.results tr:not(.head):hover td { box-shadow: inset 0 1px 0 var(--accent), inset 0 -1px 0 var(--accent); }
    |.results.only-chosen .opt { display: none; }
    |
    |@keyframes changed { from { box-shadow: inset 0 0 0 2px var(--accent); } to { box-shadow: inset 0 0 0 2px transparent; } }
    |.results td.cell.changed { animation: changed 12s ease-in forwards; }
    |@media (prefers-reduced-motion: reduce) {
    |  .results td.cell.changed { animation: none; box-shadow: inset 0 0 0 2px var(--accent); }
    |}
    |.toast {
    |  position: fixed; right: 1rem; bottom: 1rem; z-index: 10;
    |  padding: 0.5rem 0.85rem; border-radius: 8px;
    |  background: var(--surface); color: var(--text); border: 1px solid var(--border);
    |  box-shadow: 0 4px 16px rgb(0 0 0 / 0.15);
    |  opacity: 0; transform: translateY(0.5rem); transition: opacity 0.3s, transform 0.3s;
    |  pointer-events: none;
    |}
    |.toast.show { opacity: 1; transform: none; }
    |
    |.weights { border-collapse: collapse; width: 100%; font-variant-numeric: tabular-nums; }
    |.weights th, .weights td { padding: 0.3rem 0.5rem; border-bottom: 1px solid var(--border); text-align: left; }
    |.weights .num { text-align: right; }
    |.weights th { color: var(--muted); font-weight: 600; }
    |.weights tfoot td { font-weight: 700; border-bottom: none; }
    |ul.plain { margin: 0; padding-left: 1.1rem; font-family: ui-monospace, Menlo, Consolas, monospace; }
    |ul.projects {
    |  list-style: none; margin: 0; padding: 0;
    |  display: grid; gap: 0.6rem; grid-template-columns: repeat(auto-fill, minmax(9rem, 1fr));
    |}
    |ul.projects a {
    |  display: block; padding: 0.8rem 1rem; border-radius: 8px;
    |  border: 1px solid var(--border); background: var(--surface-2);
    |  color: var(--text); text-decoration: none; font-weight: 600;
    |}
    |ul.projects a:hover, ul.projects a:focus-visible { border-color: var(--accent); color: var(--accent); }
    |""".stripMargin

  private val js: String = """
    |(() => {
    |  const FIXED = FIXED_COLUMNS;
    |  // Settings are per project (switching tabs loads the other project's).
    |  // Same key as earlier versions of this page, so saved pins carry over.
    |  let key, config, sort;
    |  function load_config() {
    |    key = `~gheith/${document.body.dataset.assignment}.html#config`;
    |    config = {};
    |    try { config = JSON.parse(localStorage.getItem(key) || "{}") || {}; } catch (e) {}
    |    config.pinned_tests = config.pinned_tests || {};
    |    config.pinned_aliases = config.pinned_aliases || {};
    |    sort = config.sort || { key: "alias", dir: 1 };
    |  }
    |  load_config();
    |  const save = () => { try { localStorage.setItem(key, JSON.stringify(config)); } catch (e) {} };
    |
    |  // Wires up the current <main>. Runs again whenever a newer version of
    |  // the page is swapped in.
    |  function setup() {
    |    const table = document.querySelector("table.results");
    |    const thead = table.tHead;
    |    const tbody = table.tBodies[0];
    |    const head = thead.rows[0];
    |    const test_headers = Array.from(head.cells).slice(FIXED);
    |    const col_of = new Map(test_headers.map((th) => [th.dataset.id, th.dataset.col]));
    |    const student_rows = () => Array.from(table.rows).filter((r) => r.dataset.alias);
    |
    |    // Columns: pinned tests first, each group in the page's default order.
    |    function order_columns() {
    |      const pinned = (th) => !!config.pinned_tests[th.dataset.id];
    |      const rank = new Map(test_headers
    |        .slice()
    |        .sort((a, b) => (+pinned(b) - +pinned(a)) || (a.dataset.col - b.dataset.col))
    |        .map((th, i) => [th.dataset.col, i]));
    |      for (const row of table.rows) {
    |        const cells = Array.from(row.cells);
    |        const tests = cells.slice(FIXED).sort((a, b) => rank.get(a.dataset.col) - rank.get(b.dataset.col));
    |        row.replaceChildren(...cells.slice(0, FIXED), ...tests);
    |      }
    |      for (const th of test_headers) {
    |        th.classList.toggle("pinned", pinned(th));
    |        th.querySelector("input.colpin").checked = pinned(th);
    |      }
    |    }
    |
    |    // Rows: pinned rows live in <thead> (so they stay visible while
    |    // scrolling); both groups are sorted the same way.
    |    function value_of(row) {
    |      if (sort.key === "alias") return row.dataset.alias;
    |      if (sort.key === "chosen" || sort.key === "all" || sort.key === "score") return +row.dataset[sort.key];
    |      const col = col_of.get(sort.key.slice("test:".length));
    |      const cell = col === undefined ? null : row.querySelector(`td[data-col="${col}"]`);
    |      return cell ? +cell.dataset.r : -1;
    |    }
    |    function sort_rows() {
    |      for (const section of [thead, tbody]) {
    |        const rows = Array.from(section.rows)
    |          .filter((r) => r.dataset.alias)
    |          .map((r) => [r, value_of(r)]);
    |        rows.sort(([ra, a], [rb, b]) => {
    |          const c = typeof a === "string" ? a.localeCompare(b) : a - b;
    |          return (c * sort.dir) || ra.dataset.alias.localeCompare(rb.dataset.alias);
    |        });
    |        section.append(...rows.map(([r]) => r));
    |      }
    |      for (const button of head.querySelectorAll("button.sort")) {
    |        const th = button.closest("th");
    |        if (button.dataset.key === sort.key) th.setAttribute("aria-sort", sort.dir > 0 ? "ascending" : "descending");
    |        else th.removeAttribute("aria-sort");
    |      }
    |    }
    |
    |    function pin_row(row, pinned) {
    |      (pinned ? thead : tbody).append(row);
    |      row.querySelector("input.rowpin").checked = pinned;
    |    }
    |
    |    head.addEventListener("click", (e) => {
    |      const button = e.target.closest("button.sort");
    |      if (!button) return;
    |      const k = button.dataset.key;
    |      // numbers and test results sort best-first; aliases ascending
    |      sort = sort.key === k ? { key: k, dir: -sort.dir } : { key: k, dir: k === "alias" ? 1 : -1 };
    |      config.sort = sort;
    |      save();
    |      sort_rows();
    |    });
    |
    |    table.addEventListener("change", (e) => {
    |      const box = e.target;
    |      if (box.matches("input.rowpin")) {
    |        const row = box.closest("tr");
    |        config.pinned_aliases[row.dataset.alias] = box.checked;
    |        pin_row(row, box.checked);
    |        sort_rows();
    |      } else if (box.matches("input.colpin")) {
    |        config.pinned_tests[box.dataset.id] = box.checked;
    |        order_columns();
    |      } else return;
    |      save();
    |    });
    |
    |    table.addEventListener("click", (e) => {
    |      const cell = e.target.closest("td.alias");
    |      if (!cell) return;
    |      cell.parentElement.classList.toggle("marked");
    |      show_compare(cell.parentElement.dataset.alias);
    |    });
    |
    |    document.getElementById("compare-pick").addEventListener("change", (e) => show_compare(e.target.value));
    |    const notable = document.getElementById("notable-only");
    |    notable.checked = !!config.notable_only;
    |    notable.addEventListener("change", () => {
    |      config.notable_only = notable.checked;
    |      save();
    |      show_compare(compare, false);
    |    });
    |
    |    const filter = document.getElementById("filter");
    |    filter.addEventListener("input", () => {
    |      const q = filter.value.trim();
    |      for (const row of student_rows()) row.hidden = q !== "" && !row.dataset.alias.includes(q);
    |    });
    |
    |    const only = document.getElementById("only-chosen");
    |    only.checked = !!config.only_chosen;
    |    const apply_only = () => table.classList.toggle("only-chosen", only.checked);
    |    only.addEventListener("change", () => {
    |      config.only_chosen = only.checked;
    |      save();
    |      apply_only();
    |    });
    |
    |    document.getElementById("clear-pins").addEventListener("click", () => {
    |      config.pinned_tests = {};
    |      config.pinned_aliases = {};
    |      save();
    |      for (const row of student_rows()) pin_row(row, false);
    |      order_columns();
    |      sort_rows();
    |    });
    |
    |    for (const row of Array.from(tbody.rows)) {
    |      if (config.pinned_aliases[row.dataset.alias]) pin_row(row, true);
    |    }
    |    order_columns();
    |    sort_rows();
    |    apply_only();
    |    show_compare(compare, false);
    |  }
    |
    |  // ---- Compare one submission with all the others, from the data the
    |  // page embeds (#report-data).
    |  const hash_compare = () => decodeURIComponent((location.hash.match(/compare=([^&]+)/) || [])[1] || "");
    |  let compare = hash_compare() || config.compare || "";
    |
    |  function el(tag, props, ...kids) {
    |    const e = document.createElement(tag);
    |    for (const [k, v] of Object.entries(props || {})) {
    |      if (k === "class") e.className = v; else e.setAttribute(k, v);
    |    }
    |    e.append(...kids.filter((k) => k !== null && k !== undefined));
    |    return e;
    |  }
    |  const pct = (x) => `${Math.round(100 * x)}%`;
    |  const secs = (t) => (t === null || t === undefined) ? "\u2013" : `${t < 10 ? t.toFixed(2) : t.toFixed(1)}s`;
    |  const median = (xs) => {
    |    if (!xs.length) return null;
    |    const s = xs.slice().sort((a, b) => a - b);
    |    const m = s.length >> 1;
    |    return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
    |  };
    |  const GLYPH = { 3: ["pass", "\u00b7"], 2: ["timeout", "T"], 1: ["fail", "\u2717"], 0: ["unknown", "?"] };
    |
    |  // Where `mine` stands among `all` (higher is better): rank 1 = best.
    |  function standing(mine, all) {
    |    const better = all.filter((x) => x > mine).length;
    |    const worse = all.filter((x) => x < mine).length;
    |    return { rank: better + 1, beats: all.length > 1 ? worse / (all.length - 1) : 0 };
    |  }
    |
    |  function histogram(values, mine, max) {
    |    const ns = "http://www.w3.org/2000/svg";
    |    const counts = Array(max + 1).fill(0);
    |    for (const v of values) counts[v]++;
    |    const top = Math.max(1, ...counts);
    |    const svg = document.createElementNS(ns, "svg");
    |    svg.setAttribute("class", "hist");
    |    svg.setAttribute("viewBox", `0 0 ${(max + 1) * 10} 50`);
    |    svg.setAttribute("preserveAspectRatio", "none");
    |    svg.setAttribute("role", "img");
    |    svg.setAttribute("aria-label", `distribution of tests passed; this submission passed ${mine}`);
    |    counts.forEach((c, v) => {
    |      if (!c) return;
    |      const h = Math.max(2, 50 * c / top);
    |      const r = document.createElementNS(ns, "rect");
    |      r.setAttribute("x", v * 10 + 1);
    |      r.setAttribute("width", 8);
    |      r.setAttribute("y", 50 - h);
    |      r.setAttribute("height", h);
    |      if (v === mine) r.setAttribute("class", "me");
    |      const t = document.createElementNS(ns, "title");
    |      t.textContent = `${c} submission${c === 1 ? "" : "s"} passed ${v}`;
    |      r.append(t);
    |      svg.append(r);
    |    });
    |    return svg;
    |  }
    |
    |  function show_compare(alias, remember = true) {
    |    const body = document.getElementById("compare-body");
    |    const pick = document.getElementById("compare-pick");
    |    const data = JSON.parse(document.getElementById("report-data").textContent);
    |    compare = data.rows[alias] ? alias : "";
    |    pick.value = compare;
    |    if (remember) {
    |      config.compare = compare;
    |      save();
    |      history.replaceState(null, "", compare ? `#compare=${encodeURIComponent(compare)}` : location.pathname + location.search);
    |    }
    |    body.replaceChildren();
    |    if (!compare) {
    |      body.append(el("p", { class: "note" }, "Pick a submission above, or click an alias in the grid, to see how it compares with all the others."));
    |      return;
    |    }
    |
    |    const aliases = Object.keys(data.rows);
    |    const mine = data.rows[compare];
    |    const n_chosen = data.chosen.filter((c) => c).length;
    |    const passed = (a, only_chosen) =>
    |      data.rows[a].filter((c, i) => c[0] === 3 && (!only_chosen || data.chosen[i])).length;
    |
    |    // correctness
    |    const summary = el("div", { class: "summary" });
    |    const score_card = (label, only_chosen, out_of) => {
    |      const all = aliases.map((a) => passed(a, only_chosen));
    |      const me = passed(compare, only_chosen);
    |      const s = standing(me, all);
    |      summary.append(el("div", { class: "stat" },
    |        el("div", { class: "stat-value" }, `${me}`, el("small", {}, ` / ${out_of} ${label}`)),
    |        el("div", { class: "stat-label" },
    |          `rank ${s.rank} of ${all.length} \u00b7 passes more than ${pct(s.beats)} of submissions`),
    |        histogram(all, me, out_of),
    |        el("div", { class: "hist-axis" }, el("span", {}, "0"), el("span", {}, `${out_of} passed`))));
    |    };
    |    const my_score = data.scores[compare];
    |    if (my_score !== null && my_score !== undefined) {
    |      const all = aliases.map((a) => data.scores[a]).filter((x) => x !== null);
    |      const s = standing(my_score, all);
    |      summary.append(el("div", { class: "stat" },
    |        el("div", { class: "stat-value" }, my_score.toFixed(3), el("small", {}, " correctness"),
    |          el("span", { class: "exp" }, "experimental")),
    |        el("div", { class: "stat-label" },
    |          `rank ${s.rank} of ${all.length} \u00b7 higher than ${pct(s.beats)} of submissions`)));
    |    }
    |    if (n_chosen > 0) score_card("chosen tests", true, n_chosen);
    |    score_card("tests", false, data.tests.length);
    |
    |    // performance, per test and overall
    |    const rows = data.tests.map((t, i) => {
    |      const [r, p, n, time] = mine[i];
    |      const rate = aliases.filter((a) => data.rows[a][i][0] === 3).length / aliases.length;
    |      const times = aliases.map((a) => data.rows[a][i][3]).filter((x) => x !== null);
    |      const med = median(times);
    |      const ratio = (time !== null && med) ? time / med : null;
    |      const flags = [];
    |      const what = { 2: "times out", 1: "fails", 0: "didn't run" }[r];
    |      if (r !== 3 && rate >= 0.75) flags.push(["bad", `${what}, but ${pct(rate)} pass`]);
    |      if (r === 3 && rate <= 0.25) flags.push(["good", `passes; only ${pct(rate)} do`]);
    |      if (p > 0 && p < n) flags.push(["warn", `flaky: ${p}/${n} tries passed`]);
    |      if (ratio !== null && ratio >= 2) flags.push(["warn", "much slower than typical"]);
    |      return { t, i, r, p, n, time, rate, med, ratio, flags };
    |    });
    |    const ratios = rows.filter((x) => x.ratio !== null && x.ratio > 0).map((x) => x.ratio);
    |    const typical = ratios.length ? Math.exp(ratios.reduce((s, x) => s + Math.log(x), 0) / ratios.length) : null;
    |    summary.append(el("div", { class: "stat" },
    |      el("div", { class: "stat-value" }, typical === null ? "\u2013" : `${typical.toFixed(2)}\u00d7`,
    |        el("small", {}, " median time")),
    |      el("div", { class: "stat-label" }, typical === null
    |        ? "no timing data for the tests it passes"
    |        : `typical speed over the ${ratios.length} tests it passes; ` +
    |          `faster than the median on ${ratios.filter((x) => x < 1).length}`)));
    |    body.append(summary);
    |
    |    const shown = document.getElementById("notable-only").checked ? rows.filter((x) => x.flags.length) : rows;
    |    if (!shown.length) {
    |      body.append(el("p", { class: "note" }, "Nothing stands out: this submission does about as well as the others on every test."));
    |      return;
    |    }
    |    const rel = (x) => {
    |      if (x.ratio === null) return el("span", { class: "rel" }, "\u2013");
    |      if (x.ratio >= 1.1) return el("span", { class: "rel slow" }, `${x.ratio.toFixed(1)}\u00d7 slower`);
    |      if (x.ratio <= 0.9) return el("span", { class: "rel fast" }, `${(1 / x.ratio).toFixed(1)}\u00d7 faster`);
    |      return el("span", { class: "rel" }, "about the median");
    |    };
    |    const table = el("table", { class: "cmp" },
    |      el("thead", {}, el("tr", {},
    |        el("th", {}, "test"), el("th", {}, "this submission"), el("th", {}, "submissions passing"),
    |        el("th", { class: "num" }, "time"), el("th", { class: "num" }, "median"), el("th", {}, "relative"),
    |        el("th", {}, ""))),
    |      el("tbody", {}, ...shown.map((x) => {
    |        const [cls, glyph] = GLYPH[x.r];
    |        return el("tr", { class: data.chosen[x.i] ? "is-chosen" : "" },
    |          el("td", {}, x.t),
    |          el("td", {}, el("span", { class: `cell ${cls}` }, glyph), x.n ? `${x.p}/${x.n} tries` : "not run"),
    |          el("td", {}, el("span", { class: "meter" }, el("span", { style: `width: ${pct(x.rate)}` })), pct(x.rate)),
    |          el("td", { class: "num" }, secs(x.time)),
    |          el("td", { class: "num" }, secs(x.med)),
    |          el("td", {}, rel(x)),
    |          el("td", {}, ...x.flags.map(([k, f]) => el("span", { class: `flag ${k}` }, f))));
    |      })));
    |    body.append(el("div", { class: "cmp-wrap" }, table));
    |  }
    |
    |  setup();
    |
    |  // ---- Live updates: poll for a newer version of this page and swap it
    |  // in place, keeping the viewer's scroll position, filter and highlights.
    |  const POLL_MS = 60 * 1000;
    |  const version_of = (doc) => doc.querySelector('meta[name="report-version"]')?.content;
    |
    |  // "alias|test" -> result rank, for flashing the cells that changed
    |  function results_of(root) {
    |    const id_of = new Map(Array.from(root.querySelectorAll("th.test")).map((th) => [th.dataset.col, th.dataset.id]));
    |    const out = new Map();
    |    for (const td of root.querySelectorAll("td.cell")) {
    |      out.set(`${td.parentElement.dataset.alias}|${id_of.get(td.dataset.col)}`, td.dataset.r);
    |    }
    |    return [out, id_of];
    |  }
    |
    |  let toast_timer;
    |  function toast(message) {
    |    let t = document.querySelector(".toast");
    |    if (!t) {
    |      t = document.createElement("div");
    |      t.className = "toast";
    |      t.setAttribute("role", "status");
    |      document.body.append(t);
    |    }
    |    t.textContent = message;
    |    t.classList.add("show");
    |    clearTimeout(toast_timer);
    |    toast_timer = setTimeout(() => t.classList.remove("show"), 5000);
    |  }
    |
    |  function swap_in(doc) {
    |    const old_main = document.querySelector("main");
    |    const new_main = doc.querySelector("main");
    |    const new_header = doc.querySelector("header.top");
    |    if (!new_main || !new_header) return;
    |
    |    const wrap = old_main.querySelector(".table-wrap");
    |    const scroll = [wrap.scrollTop, wrap.scrollLeft];
    |    const filter = document.getElementById("filter").value;
    |    const marked = new Set(Array.from(old_main.querySelectorAll("tr.marked")).map((r) => r.dataset.alias));
    |    const [before] = results_of(old_main);
    |
    |    document.querySelector('meta[name="report-version"]').content = version_of(doc);
    |    document.querySelector("header.top").replaceWith(document.adoptNode(new_header));
    |    old_main.replaceWith(document.adoptNode(new_main));
    |    setup();
    |
    |    const main = document.querySelector("main");
    |    const box = document.getElementById("filter");
    |    box.value = filter;
    |    box.dispatchEvent(new Event("input"));
    |    for (const r of main.querySelectorAll("tr[data-alias]")) {
    |      if (marked.has(r.dataset.alias)) r.classList.add("marked");
    |    }
    |    const new_wrap = main.querySelector(".table-wrap");
    |    new_wrap.scrollTop = scroll[0];
    |    new_wrap.scrollLeft = scroll[1];
    |
    |    const [after, id_of] = results_of(main);
    |    let changed = 0;
    |    for (const td of main.querySelectorAll("td.cell")) {
    |      const k = `${td.parentElement.dataset.alias}|${id_of.get(td.dataset.col)}`;
    |      if (before.has(k) && before.get(k) !== after.get(k)) {
    |        td.classList.add("changed");
    |        changed++;
    |      }
    |    }
    |    const time = new Date().toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
    |    toast(`Updated at ${time}` + (changed ? ` — ${changed} result${changed === 1 ? "" : "s"} changed` : ""));
    |  }
    |
    |  let checking = false;
    |  async function check() {
    |    // a page opened from disk can't fetch itself
    |    if (checking || location.protocol === "file:") return;
    |    checking = true;
    |    try {
    |      // "no-cache" revalidates with the server, which just says "not
    |      // modified" while the page is unchanged
    |      const res = await fetch(location.href, { cache: "no-cache" });
    |      if (!res.ok) return;
    |      const doc = new DOMParser().parseFromString(await res.text(), "text/html");
    |      const v = version_of(doc);
    |      // (a tab switch may have happened while this was in flight)
    |      const same = doc.body && doc.body.dataset.assignment === document.body.dataset.assignment;
    |      if (same && v && v !== version_of(document)) swap_in(doc);
    |    } catch (e) {
    |      // offline or briefly unavailable: try again next time
    |    } finally {
    |      checking = false;
    |    }
    |  }
    |
    |  let timer;
    |  function schedule() {
    |    clearTimeout(timer);
    |    if (!document.hidden) timer = setTimeout(async () => { await check(); schedule(); }, POLL_MS);
    |  }
    |  // stop polling in background tabs; check as soon as one comes back
    |  document.addEventListener("visibilitychange", () => {
    |    if (!document.hidden) check();
    |    schedule();
    |  });
    |  schedule();
    |
    |  // ---- Project tabs: switch to a sibling project's page in place, like
    |  // following its link but without reloading.
    |  let loaded_path = location.pathname;
    |  async function open_project(href, push) {
    |    let doc;
    |    try {
    |      const res = await fetch(href, { cache: "no-cache" });
    |      if (!res.ok) throw new Error(`HTTP ${res.status}`);
    |      doc = new DOMParser().parseFromString(await res.text(), "text/html");
    |    } catch (e) {
    |      location.href = href; // fall back to a plain page load
    |      return;
    |    }
    |    const main = doc.querySelector("main");
    |    const header = doc.querySelector("header.top");
    |    if (!main || !header || !doc.getElementById("report-data")) {
    |      location.href = href;
    |      return;
    |    }
    |    if (push) history.pushState(null, "", href);
    |    loaded_path = location.pathname;
    |    document.title = doc.title;
    |    document.body.dataset.assignment = doc.body.dataset.assignment;
    |    document.querySelector('meta[name="report-version"]').content = version_of(doc);
    |    document.querySelector("header.top").replaceWith(document.adoptNode(header));
    |    document.querySelector("main").replaceWith(document.adoptNode(main));
    |    load_config();
    |    compare = hash_compare() || config.compare || "";
    |    setup();
    |  }
    |
    |  document.addEventListener("click", (e) => {
    |    const link = e.target.closest("nav.tabs a");
    |    // let new-tab / new-window clicks, and pages opened from disk, behave normally
    |    if (!link || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    |    if (location.protocol === "file:") return;
    |    e.preventDefault();
    |    if (!link.classList.contains("active")) open_project(link.href, true);
    |  });
    |  // Back / Forward between projects; a hash-only change just re-compares
    |  window.addEventListener("popstate", () => {
    |    if (location.pathname !== loaded_path) open_project(location.href, false);
    |    else show_compare(hash_compare() || compare, false);
    |  });
    |})();
    |""".stripMargin
}
