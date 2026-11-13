package ag.grader

import ag.common.{
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.r2.{say, Target, update_data, WithData}

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
    // the course's projects, as (tab label, page title), in tab order; each
    // tab links to that project's page for this page's phase
    tabs: Seq[(String, String)] = Nil,
    // the page's heading; the title if empty
    project_name: String = "",
    // shown instead of the summary when the results can't be shown
    notice: Option[String] = None,
    // which results these are: the latest, or a phase's
    phase: Phase = Phase.Latest
)

// What a project's report pages show, but the time they're made: the
// inputs of HtmlGen.reports, kept by whatever makes the pages
@upickle.implicits.allowUnknownKeys(false)
final case class ReportInputs(
    // <course>_<project>
    title: String,
    project_name: String,
    test_cutoff: LocalDateTime,
    code_cutoff: LocalDateTime,
    enrollments: Int,
    // each phase's published results (see Project.results), and whether
    // they're published yet
    results: Seq[
      (
          Phase,
          Either[String, Option[SortedMap[Alias, RedactedStudentResults]]],
          Boolean
      )
    ],
    phase1_tests: SortedSet[String],
    phase1_weight: Int,
    phase2_tests: SortedSet[String],
    phase2_weight: Int,
    weights: Seq[Weight],
    bad_tests: SortedSet[String],
    staff_aliases: SortedSet[Alias],
    tabs: Seq[(String, String)]
) derives upickle.default.ReadWriter

class HtmlGen(p: Project) {

  private val title = s"${p.course.course_name}_${p.project_name}"

  // The latest results' page's file name, both in site_base and in the
  // course's site repo; each phase's page is next to it (see page_file).
  lazy val page_name: String = HtmlGen.page_file(title, Phase.Latest)

  // every phase's results, and whether they're published yet
  private lazy val results: Target[
    Seq[
      (
          Phase,
          Either[String, Option[SortedMap[Alias, RedactedStudentResults]]],
          Boolean
      )
    ]
  ] = p.target(
    (
      latest = p.results(Phase.Latest),
      one = p.results(Phase.One),
      two = p.results(Phase.Two),
      state = p.state
    )
  ) { (_, deps) =>
    Seq(
      Phase.Latest -> deps.latest,
      Phase.One -> deps.one,
      Phase.Two -> deps.two
    ).map((phase, r) => (phase, r, deps.state.publishes(phase)))
  }

  // Renders every phase's page into this target's data directory. A page
  // whose content is unchanged (the same version; see HtmlGen.render) is
  // left as it was, generated time and all: the pages' bytes change only
  // when what they show does, and so only then does the site (and its
  // deploy to Cloudflare) change.
  lazy val page: Target[WithData[Unit]] =
    p.target(
      (
        results = results,
        phase1_tests = p.phase1_tests,
        phase1_weight = p.phase1_weight,
        phase2_tests = p.phase2_tests,
        phase2_weight = p.phase2_weight,
        weights = p.test_weights,
        bad_tests = p.bad_tests,
        test_cutoff = p.test_cutoff,
        code_cutoff = p.code_cutoff,
        enrollment = p.course.enrollment,
        anti_aliases = p.anti_aliases,
        course_staff = p.course.staff,
        project_staff = p.staff,
        projects = p.course.active_projects
      )
    ) { (producer, deps) =>
      update_data(producer)(_ => false) { dir =>
        val inputs = ReportInputs(
          title = title,
          project_name = p.project_name,
          test_cutoff = deps.test_cutoff,
          code_cutoff = deps.code_cutoff,
          enrollments = deps.enrollment.size,
          results = deps.results,
          phase1_tests = deps.phase1_tests,
          phase1_weight = deps.phase1_weight,
          phase2_tests = deps.phase2_tests,
          phase2_weight = deps.phase2_weight,
          weights = deps.weights,
          bad_tests = deps.bad_tests,
          staff_aliases = HtmlGen.staff_aliases(
            deps.anti_aliases.map(_.swap),
            deps.course_staff ++ deps.project_staff
          ),
          tabs = HtmlGen.tabs(
            p.course.course_name,
            p.project_name,
            deps.projects.keys
          )
        )
        val reports = HtmlGen.reports(
          inputs,
          LocalDateTime.now().nn,
          why => say(producer, why)
        )
        val written = reports.map(HtmlGen.write_page(dir, _))
        // (pages this target no longer makes)
        for (f <- os.list(dir) if !written.contains(f.last)) os.remove.all(f)
      }
    }

  // Copies the pages into site_base, if there is one.
  val gen_html: Target[Unit] =
    p.target((page = page, site_base = Config.site_base)) { (producer, deps) =>
      val site_base = deps.site_base
      site_base.foreach { site_base =>
        for (page_file <- os.list(deps.page.get_data_path(producer))) {
          val f = os.Path(site_base) / page_file.last
          say(producer, s"  ${f.toString}")
          os.copy.over(page_file, f, createFolders = true)
          os.perms.set(f, os.PermSet.fromString("rwxr--r--"))
        }
      }
    }

}

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

  // A project's pages, one per phase, made at `generated`; `log` hears why
  // results that should be shown can't be
  def reports(
      i: ReportInputs,
      generated: LocalDateTime,
      log: String => Unit
  ): Seq[ReportData] =
    for ((phase, r, published) <- i.results) yield {
      val deadline = phase.deadline(i.test_cutoff, i.code_cutoff)
      ReportData(
        title = i.title,
        generated = generated,
        test_cutoff = i.test_cutoff,
        code_cutoff = i.code_cutoff,
        enrollments = i.enrollments,
        results =
          if (published) r.toOption.flatten.getOrElse(SortedMap())
          else SortedMap(),
        notice =
          if (!published)
            Some(
              s"${phase_label(phase).capitalize} results will be published after its deadline" +
                deadline.fold("")(d => s", ${display_format.format(d)}") + "."
            )
          else
            r.left.toOption.map { why =>
              log(
                s"can't show ${i.title}'s ${phase_label(phase)} results: $why"
              )
              "The results for this project are being updated; they'll be back shortly."
            },
        phase1_tests = i.phase1_tests,
        phase1_weight = i.phase1_weight,
        phase2_tests = i.phase2_tests,
        phase2_weight = i.phase2_weight,
        weights = i.weights,
        bad_tests = i.bad_tests,
        staff_aliases = i.staff_aliases.toSet,
        tabs = i.tabs,
        project_name = i.project_name,
        phase = phase
      )
    }

  // Writes `data`'s page into `dir`, unless the page there shows the same
  // (the same version; see render): a page's bytes change only when what it
  // shows does. Its file name.
  def write_page(dir: os.Path, data: ReportData): String = {
    val f = dir / page_file(data.title, data.phase)
    val v = version(data)
    if (!(os.isFile(f) && version_of(os.read(f)).contains(v)))
      os.write.over(f, render_with(data, v))
    f.last
  }
  // ScalaTags: typed tags and attributes; text and attribute values are
  // escaped, and only `raw` (the page's own CSS, JS and JSON) is not.
  import scalatags.Text.all.*
  import scalatags.Text.tags2

  // a project's page for `phase`, by the project's title (<course>_<project>)
  def page_file(title: String, phase: Phase): String = phase match
    case Phase.Latest => s"$title.html"
    case Phase.One    => s"${title}_phase1.html"
    case Phase.Two    => s"${title}_phase2.html"

  // Whether `file` is a phase's page (the index lists only the latest ones).
  def is_phase_page(file: String): Boolean =
    file.endsWith("_phase1.html") || file.endsWith("_phase2.html")

  def phase_label(phase: Phase): String = phase match
    case Phase.Latest => "latest"
    case Phase.One    => "phase 1"
    case Phase.Two    => "phase 2"

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
  // phase as in the "Selected test weights" table, and earns that weight
  // times the fraction of its tries that passed. The score is the earned
  // share of all the weight, times 0.9 per day the commit is late, counting
  // any part of a day as a whole one (1 day 2 hours late: 0.9^2). A phase's
  // results count only that phase's weight, and are late from that phase's
  // cutoff; the latest results count both, and are late from the phase 2
  // (code) cutoff.

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
    score_with(compute_weights(d), d.phase, r)

  private def score_with(
      weights: Weights,
      phase: Phase,
      r: RedactedStudentResults
  ): Option[Score] = {
    def counted(row: WeightRow): Int = phase match
      case Phase.Latest => row.phase1 + row.phase2
      case Phase.One    => row.phase1
      case Phase.Two    => row.phase2
    val possible = phase match
      case Phase.Latest => weights.phase1_total + weights.phase2_total
      case Phase.One    => weights.phase1_total
      case Phase.Two    => weights.phase2_total
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
        .flatMap((alias, r) => score_with(weights, d.phase, r).map(alias -> _))
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
            "of its tries that passed; correctness is the share of " +
            (d.phase match
              case Phase.Latest => "all the weight"
              case Phase.One    => "phase 1's weight"
              case Phase.Two    => "phase 2's weight") +
            " earned, times 0.9 for each day (or part of a day) the commit is past the " +
            (if (d.phase == Phase.One) "phase 1" else "phase 2") +
            " cutoff."
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
        data("phase") := phase_label(d.phase)
      )(
        header(cls := "top")(
          h1(if (d.project_name.nonEmpty) d.project_name else d.title),
          Option.when(d.tabs.nonEmpty)(
            tags2.nav(cls := "tabs", aria.label := "projects")(
              for ((tab, title) <- d.tabs) yield {
                val here = title == d.title
                a(
                  Option.when(here)(cls := "active"),
                  href := page_file(title, d.phase),
                  Option.when(here)(attr("aria-current") := "page")
                )(tab)
              }
            )
          ),
          tags2.nav(cls := "phases", aria.label := "results")(
            for (phase <- Seq(Phase.Latest, Phase.One, Phase.Two)) yield {
              val here = phase == d.phase
              a(
                Option.when(here)(cls := "active"),
                href := page_file(d.title, phase),
                Option.when(here)(attr("aria-current") := "page")
              )(phase_label(phase))
            }
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

  // A course site's front page: a link to each project's page. Visitors go
  // straight on to the latest project's live (latest results) page; the
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

  // The pages' style sheet: src/main/resources/ag/grader/report.css,
  // inlined into each page.
  private val css: String =
    os.read(os.resource / "ag" / "grader" / "report.css")

  // The report page's script: src/main/resources/ag/grader/report.js,
  // inlined into each page.
  private val js: String = os.read(os.resource / "ag" / "grader" / "report.js")
}
