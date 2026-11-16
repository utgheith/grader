package ag.grader.web

import ag.grader.Milestone

import ag.grader.name.{Alias, CSID}

import ag.grader.json.{
  PushGap,
  RawMilestone,
  Weight,
  RedactedStudentResults,
  OutcomeStatus,
  RedactedTestId
}

import ag.common.{given_ReadWriter_SortedMap, given_ReadWriter_SortedSet}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.{SortedMap, SortedSet}

// Everything a report page shows, as plain data (no targets), so pages can
// be rendered -- and tested -- without a grader workspace.
final case class ReportData(
    title: String,
    generated: LocalDateTime,
    // the project's milestones, in deadline order
    milestones: Seq[RawMilestone],
    enrollments: Int,
    results: SortedMap[Alias, RedactedStudentResults],
    weights: Seq[Weight],
    bad_tests: SortedSet[String],
    // highlighted rows: aliases that belong to staff
    staff_aliases: Set[Alias],
    // the course's projects, as (tab label, page title), in tab order; each
    // tab links to that project's main page
    tabs: Seq[(String, String)] = Nil,
    // the page's heading; the title if empty
    project_name: String = "",
    // shown instead of the summary when the results can't be shown
    notice: Option[String] = None,
    // which results these are
    milestone: Milestone = Milestone.Live,
    // the milestone the project's main page shows (see ProjectState.current)
    current: Milestone = Milestone.Live
)

// What a project's report pages show, but the time they're made: the
// inputs of HtmlGen.reports, kept by whatever makes the pages
@upickle.implicits.allowUnknownKeys(false)
final case class ReportInputs(
    // <course>_<project>
    title: String,
    project_name: String,
    milestones: Seq[RawMilestone],
    // the milestone the main page shows (see ProjectState.current)
    current: Milestone,
    enrollments: Int,
    // each milestone's published results (see Project.results), and whether
    // they're published yet
    results: Seq[
      (
          Milestone,
          Either[String, Option[SortedMap[Alias, RedactedStudentResults]]],
          Boolean
      )
    ],
    weights: Seq[Weight],
    bad_tests: SortedSet[String],
    staff_aliases: SortedSet[Alias],
    tabs: Seq[(String, String)]
) derives upickle.default.ReadWriter

object HtmlGen {

  // the aliases of `staff`, by `aliases` (csid -> alias)
  def staff_aliases(
      aliases: collection.Map[CSID, Alias],
      staff: collection.Set[CSID]
  ): SortedSet[Alias] =
    aliases
      .collect { case (csid, alias) if staff.contains(csid) => alias }
      .to(SortedSet)

  // the tabs of a course's pages: its active projects and this one, shortest
  // name first, as (label, page title)
  def tabs(
      course_name: String,
      project_name: String,
      active: Iterable[String]
  ): Seq[(String, String)] =
    (active.toSeq :+ project_name).distinct
      .sortBy(n => (n.length, n))
      .map(n => n -> s"${course_name}_$n")

  // A project's pages, one per milestone, made at `generated`; `log` hears why
  // results that should be shown can't be
  def reports(
      i: ReportInputs,
      generated: LocalDateTime,
      log: String => Unit
  ): Seq[ReportData] =
    for ((milestone, r, published) <- i.results) yield {
      ReportData(
        title = i.title,
        generated = generated,
        milestones = i.milestones,
        enrollments = i.enrollments,
        results =
          if (published) r.toOption.flatten.getOrElse(SortedMap())
          else SortedMap(),
        notice =
          if (!published)
            Some(
              s"${milestone_label(milestone).capitalize} results will be published after the final deadline, " +
                s"${display_format.format(i.milestones.last.deadline)}."
            )
          else
            r.left.toOption.map { why =>
              log(
                s"can't show ${i.title}'s ${milestone_label(milestone)} results: $why"
              )
              "The results for this project are being updated; they'll be back shortly."
            },
        weights = i.weights,
        bad_tests = i.bad_tests,
        staff_aliases = i.staff_aliases.toSet,
        tabs = i.tabs,
        project_name = i.project_name,
        milestone = milestone,
        current = i.current
      )
    }

  // Writes `data`'s page into `dir`, unless the page there shows the same
  // (the same version; see render): a page's bytes change only when what it
  // shows does. Its file name.
  def write_page(dir: os.Path, data: ReportData): String = {
    val f = dir / page_file(data.title, data.milestone, data.current)
    val v = version(data)
    if (!(os.isFile(f) && version_of(os.read(f)).contains(v)))
      os.write.over(f, render_with(data, v))
    f.last
  }
  // ScalaTags: typed tags and attributes; text and attribute values are
  // escaped, and only `raw` (the page's own CSS, JS and JSON) is not.
  import scalatags.Text.all.*
  import scalatags.Text.tags2

  // a project's page for `milestone`, by the project's title (<course>_<project>):
  // its main page, <title>.html, if `milestone` is the one it shows (`current`),
  // or else <title>--<milestone>.html
  def page_file(
      title: String,
      milestone: Milestone,
      current: Milestone
  ): String =
    if (milestone == current) s"$title.html"
    else s"$title--${milestone.slug}.html"

  // Whether `file` is a page for a milestone other than the current one (the
  // index lists only the main pages)
  def is_milestone_page(file: String): Boolean = file.contains("--")

  def milestone_label(milestone: Milestone): String = milestone.slug

  val display_format: DateTimeFormatter =
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
  // milestone as in the "Selected test weights" table, and earns that weight
  // times the fraction of its tries that passed. The score is the earned
  // share of all the weight, times 0.9 per day the commit is late, counting
  // any part of a day as a whole one (1 day 2 hours late: 0.9^2). A
  // milestone's results count only its weight, and are late from its
  // deadline; Live's count every milestone's, and are late from the final
  // deadline.

  final case class Score(
      earned: Double, // weighted, before the late penalty
      possible: Int,
      days_late: Double, // exact (1 day 2 hours = 1.083)
      correctness: Double // 0..1, after the late penalty
  )

  // the days the late penalty charges for: every day begun counts
  private def charged_days(days_late: Double): Int = math.ceil(days_late).toInt

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
    score_with(compute_weights(d), d.milestone, r)

  private def score_with(
      weights: Weights,
      milestone: Milestone,
      r: RedactedStudentResults
  ): Option[Score] = {
    // (Live counts every milestone)
    val counts: Int => Boolean = milestone match
      case Milestone.Named(name) => i => weights.names(i) == name
      case Milestone.Live        => _ => true
    def counted(row: WeightRow): Int =
      row.per_milestone.zipWithIndex.collect {
        case (w, i) if counts(i) => w
      }.sum
    val possible =
      weights.totals.zipWithIndex.collect { case (t, i) if counts(i) => t }.sum
    Option.when(possible > 0) {
      val earned = weights.rows.map { row =>
        val runs = r.outcomes
          .get(RedactedTestId(row.test))
          .map(_.outcomes)
          .getOrElse(Nil)
        val fraction =
          if (runs.isEmpty) 0.0
          else runs.count(_._1 == OutcomeStatus.pass).toDouble / runs.size
        counted(row) * fraction
      }.sum
      val days = r.prepare_info.days_late
      Score(
        earned,
        possible,
        days,
        earned / possible * math.pow(0.9, charged_days(days).toDouble)
      )
    }
  }

  // How a test's worst try is shown (see OutcomeApi.worst): css class,
  // glyph, word, sort rank
  private def status_of(
      o: Option[OutcomeStatus]
  ): (String, String, String, Int) = o match {
    case Some(OutcomeStatus.pass)           => ("pass", "\u00b7", "pass", 3)
    case Some(OutcomeStatus.timeout)        => ("timeout", "T", "timeout", 2)
    case Some(OutcomeStatus.fail)           => ("fail", "✗", "fail", 1)
    case Some(OutcomeStatus.unknown) | None =>
      ("unknown", "?", "didn't run", 0)
  }

  // a test's weight in each milestone, scaled (0 if it isn't one of the
  // milestone's tests)
  private final case class WeightRow(
      test: String,
      weight: Option[Int],
      per_milestone: Seq[Int]
  )

  // per milestone, in order: its name, the factor its tests' weights are
  // scaled by, and their total
  private final case class Weights(
      rows: Seq[WeightRow],
      names: Seq[String],
      factors: Seq[Int],
      totals: Seq[Int]
  )

  // Scales each milestone's raw weights (its tests', from test_weights) so
  // the milestones keep their configured ratio, using the lcm of the raw sums.
  private def compute_weights(d: ReportData): Weights = {
    // the chosen tests: some milestone's, less the ignored ones
    val selected = chosen_of(d).toList
      .filterNot(d.bad_tests.contains)
      .sortBy(c => (c.length, c))
    def weight_of(c: String): Option[Int] =
      d.weights
        .find(w => scala.util.matching.Regex(w.pattern).matches(c))
        .map(_.weight)

    val raws = d.milestones.map(m =>
      selected.filter(m.tests.contains).flatMap(weight_of).sum
    )
    val factor = raws
      .filter(_ != 0)
      .map(BigInt(_))
      .reduceOption((a, b) => a / a.gcd(b) * b)
      .fold(0)(_.toInt)
    val totals = d.milestones.map(_.weight * factor)
    val factors =
      raws.zip(totals).map((raw, total) => if (raw != 0) total / raw else 0)

    val rows = selected.map { c =>
      val w = weight_of(c)
      WeightRow(
        test = c,
        weight = w,
        per_milestone = d.milestones
          .zip(factors)
          .map((m, f) => if (m.tests.contains(c)) w.getOrElse(0) * f else 0)
      )
    }
    Weights(rows, d.milestones.map(_.name), factors, totals)
  }

  // the tests some milestone counts
  private def chosen_of(d: ReportData): Set[String] =
    d.milestones.flatMap(_.tests).toSet

  // A page's version: a hash of everything it shows except the time it was
  // generated, so regenerating unchanged results doesn't look like a change.
  // The page polls for itself and swaps in a copy whose version differs.
  def render(d: ReportData): String = render_with(d, version(d))

  def version(d: ReportData): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256").nn
    val bytes = render_with(d.copy(generated = LocalDateTime.MIN.nn), "")
      .getBytes("UTF-8")
    digest.digest(bytes).nn.take(8).map(b => f"${b & 0xff}%02x").mkString
  }

  // the version a rendered page carries, if it's one of these pages
  def version_of(page: String): Option[String] =
    """<meta name="report-version" content="([0-9a-f]+)"""".r
      .findFirstMatchIn(page)
      .map(_.group(1).nn)

  private def render_with(d: ReportData, version: String): String = {
    val chosen: Set[String] = chosen_of(d)

    // chosen tests first, then by name length, then by name
    val tests: Seq[String] = d.results.values.toSeq
      .flatMap(_.outcomes.keys.map(_.external_name))
      .distinct
      .filterNot(d.bad_tests.contains)
      .sortBy(t => (!chosen.contains(t), t.length, t))
    val chosen_count = tests.count(chosen.contains)

    val rows: Seq[(Alias, RedactedStudentResults)] =
      d.results.toSeq.filter(_._2.alias.isDefined)

    // worst status of every test, per row
    val statuses: Map[Alias, Map[String, Option[OutcomeStatus]]] =
      rows.map { (alias, r) =>
        alias -> r.outcomes.map((k, v) => k.external_name -> v.worst).toMap
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
        .flatMap((alias, r) =>
          score_with(weights, d.milestone, r).map(alias -> _)
        )
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
                  info.gap.map {
                    case PushGap.PushedLater(_) =>
                      "pushed over 30 min after it was committed"
                    case PushGap.CommittedLater(_) =>
                      "committed over 30 min after it was pushed (clock ahead)"
                  }
                ).flatten.mkString(" · ")
              )(
                alias.toString,
                Option.when(is_late)(span(cls := "mark late")("*")),
                Option.when(info.from_commit_file)(span(cls := "mark")("†")),
                info.gap.map {
                  case PushGap.PushedLater(_) =>
                    span(cls := "mark pushed-late")("!")
                  case PushGap.CommittedLater(_) =>
                    span(cls := "mark clock")("~")
                },
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
                      s" × 0.9^${charged_days(s.days_late)} (${describe_days(s.days_late)} late)"
                  title := f"${s.earned}%.1f of ${s.possible} points$late = ${s.correctness}%.3f"
                }
              )(score.fold("–")(s => f"${s.correctness}%.3f")),
              for ((t, i) <- tests.zipWithIndex) yield {
                val o = r.outcomes.get(RedactedTestId(t))
                val (status_cls, glyph, word, rank) =
                  status_of(statuses(alias).get(t).flatten)
                val runs = o.map(_.outcomes).getOrElse(Nil)
                val passed = runs.count(_._1 == OutcomeStatus.pass)
                val tries = Option.when(runs.nonEmpty) {
                  val n = runs.size
                  val rate = math.round(100.0 * passed / n)
                  s"$passed of $n ${
                      if (n == 1) "try" else "tries"
                    } passed ($rate%)"
                }
                // passed some tries but not all: a bar shows the pass rate
                val flaky = passed > 0 && passed < runs.size
                val times = o.flatMap(_.min_max).map { (lo, hi) =>
                  if (lo == hi) f"$lo%.2fs" else f"$lo%.2f–$hi%.2fs"
                }
                td(
                  cls := (s"cell $status_cls" +
                    (if (flaky) " flaky" else "") +
                    (if (chosen.contains(t)) "" else " opt")),
                  Option.when(flaky)(
                    style := f"--rate: ${passed.toDouble / runs.size}%.3f"
                  ),
                  data("col") := i.toString,
                  data("r") := rank.toString,
                  title := (List(s"$t: $word") ++
                    tries ++
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
        li(
          span(cls := "cell fail flaky", style := "--rate: 0.6")("✗"),
          " passed some tries (bar: pass rate)"
        ),
        li(span(cls := "swatch chosen")(""), " chosen test"),
        li(span(cls := "mark late")("*"), " late commit"),
        li(span(cls := "mark")("†"), " commit chosen by a commit id file"),
        li(
          span(cls := "mark pushed-late")("!"),
          " pushed over 30 min after it was committed"
        ),
        li(
          span(cls := "mark clock")("~"),
          " committed over 30 min after it was pushed (clock ahead)"
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
              for ((name, f) <- weights.names.zip(weights.factors))
                yield th(cls := "num")(s"$name (×$f)")
            )
          ),
          tbody(
            for (w <- weights.rows)
              yield tr(
                td(w.test),
                td(cls := "num")(w.weight.fold("?")(_.toString)),
                for (x <- w.per_milestone) yield td(cls := "num")(x.toString)
              )
          ),
          tfoot(
            tr(
              td("total"),
              td(""),
              for (t <- weights.totals) yield td(cls := "num")(t.toString)
            )
          )
        ),
        div(cls := "note")(
          s"Overall: ${weights.totals.mkString(" + ")} = ${weights.totals.sum}; " +
            s"${weights.names.mkString(" : ")} = ${d.milestones.map(_.weight).mkString(" : ")}"
        ),
        div(cls := "note")(
          span(cls := "exp")("experimental"),
          " Correctness (0 to 1) may change. Each test earns its weight times the fraction " +
            "of its tries that passed; correctness is the share of " +
            (d.milestone match
              case Milestone.Named(name) => s"$name's weight"
              case Milestone.Live        => "all the weight") +
            " earned, times 0.9 for each day (or part of a day) the commit is past " +
            (d.milestone match
              case Milestone.Named(name) => s"$name's deadline."
              case Milestone.Live        => "the final deadline.")
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
      body(
        data("assignment") := d.title,
        data("milestone") := milestone_label(d.milestone)
      )(
        header(cls := "top")(
          h1(if (d.project_name.nonEmpty) d.project_name else d.title),
          Option.when(d.tabs.nonEmpty)(
            tags2.nav(cls := "tabs", aria.label := "projects")(
              for ((tab, title) <- d.tabs) yield {
                val here = title == d.title
                a(
                  Option.when(here)(cls := "active"),
                  href := s"$title.html",
                  Option.when(here)(attr("aria-current") := "page")
                )(tab)
              }
            )
          ),
          tags2.nav(cls := "milestones", aria.label := "results")(
            for (
                milestone <- d.milestones
                  .map(m => Milestone.Named(m.name)) :+ Milestone.Live
              )
              yield {
                val here = milestone == d.milestone
                a(
                  Option.when(here)(cls := "active"),
                  href := page_file(d.title, milestone, d.current),
                  Option.when(here)(attr("aria-current") := "page")
                )(milestone_label(milestone))
              }
          ),
          dl(cls := "dates")(
            div(dt("generated"), dd(display_format.format(d.generated).nn)),
            for (m <- d.milestones)
              yield div(
                dt(s"${m.name} deadline"),
                dd(display_format.format(m.deadline).nn)
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

  // A course site's front page: a link to each project's page. Visitors go
  // straight on to the newest project's main page (its current milestone); the
  // list is for index.html?all, and for browsers without JavaScript.
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
        tags2.style(raw(css)),
        for (latest <- sorted.lastOption.toSeq)
          yield script(
            raw(
              // (replace: so that Back doesn't land here, to be sent on again)
              s"""if (!new URLSearchParams(location.search).has("all")) location.replace(${ujson
                  .write(ujson.Str(latest))});"""
            )
          )
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

  // The pages' style sheet: src/main/resources/ag/grader/web/report.css,
  // inlined into each page.
  private val css: String =
    os.read(os.resource / "ag" / "grader" / "web" / "report.css")

  // The report page's script: src/main/resources/ag/grader/web/report.js,
  // inlined into each page.
  private val js: String =
    os.read(os.resource / "ag" / "grader" / "web" / "report.js")
}
