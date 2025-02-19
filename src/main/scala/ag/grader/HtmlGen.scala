package ag.grader

import java.io.FileWriter
import ag.rules.{Maker, Rule, say}

import scala.collection.SortedMap
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.annotation.unused
import scala.collection.SortedSet

trait HtmlContext {
  def doctype(): Unit
  def start(tag: String, attributes: Seq[(String, String)]): Unit
  def start_self_closing(tag: String, attributes: Seq[(String, String)]): Unit
  def text(s: String): Unit
  def end(tag: String): Unit
}

class FileContext(path: os.Path) extends HtmlContext with AutoCloseable {

  private val w = new FileWriter(path.toIO).nn

  def doctype(): Unit = {
    w.write("<!DOCTYPE html>")
  }

  def start(tag: String, attributes: Seq[(String, String)]): Unit = {
    w.write(s"<$tag")
    attributes.foreach { (k, v) =>
      w.write(s" $k='$v'")
    }
    w.write(">")
  }

  def start_self_closing(
      tag: String,
      attributes: Seq[(String, String)]
  ): Unit = {
    w.write(s"<$tag")
    attributes.foreach { (k, v) =>
      w.write(s" $k='$v'")
    }
    w.write("/>")
  }

  def text(s: String): Unit = {
    w.write(s)
  }

  def end(tag: String): Unit = {
    w.write(s"</$tag>")
  }

  def close(): Unit = {
    w.close()
  }
}

val self_closing_tags = Set("meta")

@unused
case class Element(tag: String, attributes: Seq[(String, String)]) {
  def attr(name: String, value: String | Null): Element = value match {
    case s: String => this.copy(attributes = attributes :+ (name, s))
    case null      => this
  }
  @unused
  def bgcolor(c: String | Null): Element =
    attr("bgcolor", c)
  def colspan(c: String | Null): Element =
    attr("colspan", c)
  def href(c: String | Null): Element =
    attr("href", c)
  def style(c: String | Null): Element =
    attr("style", c)
  def css_class(c: String | Null): Element =
    attr("class", c)
  def css_class(c: List[String]): Element =
    attr("class", c.mkString(" "))
  @unused
  def textAlign(c: String | Null): Element =
    attr("textAlign", c)
  def title(c: String | Null): Element =
    attr("title", c)

  def apply(f: HtmlContext ?=> Any)(using c: HtmlContext): Unit = {
    if (self_closing_tags.contains(tag)) {
      c.start_self_closing(tag, attributes)
    } else {
      c.start(tag, attributes)
      f(using c)
      c.end(tag)
    }
  }
}

@unused
object Element {
  def apply(t: String): Element = Element(t, Seq())
  def apply()(using name: sourcecode.Name): Element = {
    Element(name.value, Seq())
  }
}

val a = Element()
val body = Element()
val h1 = Element()
val h3 = Element()
val p = Element()
val pre = Element()
val table = Element()
val td = Element()
val tr = Element()
val thead = Element()
val tbody = Element()
val tfoot = Element()
val script = Element()
val style = Element()
val head = Element()
val meta = Element()
val title = Element()

def html[A](c: HtmlContext)(f: HtmlContext ?=> A): A = {
  c.doctype()
  c.start("html", Seq())
  val a = f(using c)
  c.end("html")
  a
}

def text(s: String)(using c: HtmlContext): Unit = {
  c.text(s)
}

class HtmlGen(p: Project) {

  private val displayFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm (EEEE)").nn

  val gen_html: Maker[Unit] =
    Rule(
      p.results *:
        Config.site_base *:
        p.phase1_tests *:
        p.phase1_weight *:
        p.phase2_tests *:
        p.phase2_weight *:
        p.test_weights *:
        p.bad_tests *:
        p.test_extensions *:
        p.test_cutoff *:
        p.code_cutoff *:
        p.course.enrollment *:
        p.anti_aliases *:
        p.course.staff *:
        p.staff,
      p.scope
    ) {
      case (
            Some(results),
            Some(site_base),
            phase1_tests,
            phase1_weight,
            phase2_tests,
            phase2_weight,
            weights,
            bad_tests,
            test_extensions_,
            test_cutoff,
            code_cutoff,
            enrollment,
            anti_aliases,
            course_staff,
            project_staff
          ) =>
        val test_extensions = test_extensions_.to(IndexedSeq)

        val (chosenTestNames, otherTestNames) = results.values
          .to(List)
          .flatMap(
            _.outcomes.keys.filterNot(test =>
              bad_tests.contains(test.external_name)
            )
          )
          .sorted
          .distinct
          .groupBy(_.external_name.length)
          .toSeq
          .map { case (len, names) => (len, names.sorted) }
          .sortBy(_._1)
          .flatMap(_._2)
          .partition(test =>
            phase1_tests.contains(test.external_name)
              || phase2_tests.contains(test.external_name)
          )

        // All the test name properly ordered
        val testNames: Seq[RedactedTestId] = chosenTestNames ++ otherTestNames

        val submissions: List[(Alias, String)] = (for {
          s <- results.values
          a <- s.alias.to(Seq)
          sha = s.prepare_info.sha
        } yield (a, sha)).to(List).sortBy(_._1)

        def testTitle(t: String, rowNum: Int)(using HtmlContext): Unit = {
          val ch = if (rowNum >= t.length) "." else t(rowNum)
          val ext =
            if (rowNum >= test_extensions.size) test_extensions.last
            else test_extensions(rowNum)
          val fileName = s"$t.$ext"

          td.attr("data-id", t) {
            a.href(s"${p.tests_repo_name}/$t.$ext").title(fileName) {
              text(ch.toString)
            }
          }
        }

        // Display the different time stamps
        def times(using HtmlContext): Unit = table {
          tr {
            td { text("generated") }
            td { text(displayFormat.format(LocalDateTime.now().nn).nn) }
          }
          tr {
            td { text("phase 1 cutoff") }
            td { text(displayFormat.format(test_cutoff).nn) }
          }
          tr {
            td { text("phase 2 cutoff") }
            td { text(displayFormat.format(code_cutoff).nn) }
          }
        }

        // Display the results table
        def tbl(using HtmlContext): Unit = table.css_class("results") {
          thead {
            /* 3 headers */
            for (i <- 0 to 2) {
              tr {
                td.css_class("alias") { text("") }
                td { text("") }
                testNames.foreach { t =>
                  testTitle(t.external_name, i)
                }
              }
            }
          }

          tbody {
            /* the actual results, one row per submission */
            submissions.foreach { case (alias, sha) =>
              val short_name = s"${alias.toString}_$sha".take(8)
              val result = results(alias)
              val outcome: SortedMap[String, RedactedOutcome] =
                result.outcomes.map { case (k, v) => (k.external_name, v) }

              val is_mine = anti_aliases.get(alias) match {
                case Some(csid) =>
                  course_staff.contains(csid) || project_staff.contains(csid)
                case None => false
              }
              val is_late = result.prepare_info.commit_time.isAfter(
                ZonedDateTime.of(code_cutoff, ZoneId.systemDefault())
              )
              tr.css_class(if (is_mine) "mine" else null)
                .attr("data-alias", alias.toString) {

                  td.title(sha).css_class("alias") {
                    if (is_late) text(s"$short_name*")
                    else text(short_name)
                  }
                  td {
                    text("")
                  }
                  testNames.foreach { t =>
                    val o: Option[RedactedOutcome] =
                      outcome.get(t.external_name)
                    val (status_class, the_text) =
                      o.flatMap(_.outcome) match {
                        case Some(OutcomeStatus.pass) =>
                          ("pass", ".")
                        case Some(OutcomeStatus.fail) =>
                          ("fail", "X")
                        case Some(OutcomeStatus.timeout) =>
                          ("fail", "T")
                        case None | Some(OutcomeStatus.unknown) =>
                          ("compilefail", "?")
                      }
                    val chosen_class =
                      if (
                        phase1_tests.contains(t.external_name)
                        || phase2_tests.contains(t.external_name)
                      ) List("chosen")
                      else List.empty
                    val the_class = List(status_class) ::: chosen_class
                    val ttl = o match {
                      case Some(
                            RedactedOutcome(_, _, _, Some(time), tries)
                          ) =>
                        f"$tries%s tries, last took $time%.2fs"
                      case Some(RedactedOutcome(_, _, _, None, tries)) =>
                        s"$tries tries"
                      case _ =>
                        null
                    }
                    td.css_class(the_class).title(ttl) {
                      text(the_text)
                    }
                  }
                }
            }
          }
        }

        def ignored(using HtmlContext): Unit = table {
          tr { td { h3 { pre { text("Ignored tests") } } } }
          tr {
            td {
              table {
                bad_tests.foreach { test_name =>
                  tr {
                    td {
                      pre {
                        text(test_name)
                      }
                    }
                  }
                }
              }
            }
          }
        }

        def weights_table(using HtmlContext): Unit = table {
          // Match the sorting of test cases elsewhere (t0-4 before user test cases)
          val selected_tests = phase1_tests ++ phase2_tests
          val sorted_tests = selected_tests.toList.sortBy(c => (c.length, c))

          // Get the summed raw test weights for each phase
          var phase1_raw_weight = 0
          var phase2_raw_weight = 0
          sorted_tests.foreach { c =>
            val weight = weights.find { w =>
              scala.util.matching.Regex(w.pattern).matches(c)
            }
            weight match {
              case Some(w) => {
                if (phase1_tests.contains(c)) {
                  phase1_raw_weight += w.weight
                }
                if (phase2_tests.contains(c)) {
                  phase2_raw_weight += w.weight
                }
              }
              case _ => {}
            }
          }
          // get the lcm of the raw phase1 and phase2 summed weights
          val factor =
            if (phase1_raw_weight == 0) phase2_raw_weight
            else if (phase2_raw_weight == 0) phase1_raw_weight
            else
              (phase1_raw_weight / BigInt(phase1_raw_weight)
                .gcd(phase2_raw_weight)
                .toInt) * phase2_raw_weight
          val phase1_total_weight = phase1_weight * factor
          val phase2_total_weight = phase2_weight * factor
          val phase1_factor =
            if (phase1_raw_weight != 0) phase1_total_weight / phase1_raw_weight
            else 0
          val phase2_factor =
            if (phase2_raw_weight != 0) phase2_total_weight / phase2_raw_weight
            else 0
          val total_weight = phase1_total_weight + phase2_total_weight

          tr { td { h3 { pre { text("Selected test weights") } } } }
          tr {
            td {
              table.css_class("weights") {
                thead.style("font-weight: bold;") {
                  tr {
                    td { text("test") }
                    td { text("weight") }
                    td { text(s"phase 1 weight (x$phase1_factor)") }
                    td { text(s"phase 2 weight (x$phase2_factor)") }
                  }
                }
                tbody {
                  sorted_tests.foreach { c =>
                    val weight = weights.find { w =>
                      scala.util.matching.Regex(w.pattern).matches(c)
                    }
                    val weight_str = weight match {
                      case Some(w) => w.weight.toString
                      case None    => "?"
                    }
                    val phase1_weight_str = weight match {
                      case Some(w) if phase1_tests.contains(c) =>
                        (w.weight * phase1_factor).toString
                      case _ => "0"
                    }
                    val phase2_weight_str = weight match {
                      case Some(w) if phase2_tests.contains(c) =>
                        (w.weight * phase2_factor).toString
                      case _ => "0"
                    }
                    tr {
                      td { text(c) }
                      td { text(weight_str) }
                      td { text(phase1_weight_str) }
                      td { text(phase2_weight_str) }
                    }
                  }
                }
                tfoot {
                  tr {
                    td { text("total") }
                    td {}
                    td { text(phase1_total_weight.toString) }
                    td { text(phase2_total_weight.toString) }
                  }
                }
              }
            }
          }

          tr.style("font-weight: bold;") {
            td {
              pre {
                text(
                  s"Overall total weight: $phase1_total_weight + $phase2_total_weight = $total_weight; Phase1:Phase2 = $phase1_weight:$phase2_weight"
                )
              }
            }
          }
        }

        val f =
          os.Path(site_base) / s"${p.course.course_name}_${p.project_name}.html"
        val perms = os.PermSet.fromString("rwxr--r--")

        say(s"  $f")

        os.write.over(f, "")
        os.perms.set(f, perms)

        val _ = scala.util.Using(FileContext(f)) { file =>
          html(file) {
            head {
              meta.attr("charset", "utf8") {}
              meta
                .attr("name", "viewport")
                .attr("content", "width=device-width,initial-scale=1") {}
              title(text(s"${p.course.course_name}_${p.project_name}"))
              style(text("""
                  |:root {
                  |  color-scheme: light dark;
                  |  --bg-page: #FFF;
                  |  --fg-text: #000;
                  |
                  |  --fg-pass: green;
                  |  --fg-fail: red;
                  |  --fg-compilefail: #933;
                  |  --bg-highlight: hsl(53.5, 100%, 71.2%);
                  |
                  |  --bg-chosen: hsl(120, 0%, 95%);
                  |  --bg-chosen-alt: hsl(120, 0%, 88%);
                  |  --bg-row: #FFF;
                  |  --bg-row-alt: #F5F5F5;
                  |
                  |  --border-table: #CCC;
                  |}
                  |
                  |@media (prefers-color-scheme: dark) {
                  |  :root {
                  |    --bg-page: #222;
                  |    --fg-text: #EEE;
                  |
                  |    --fg-pass: hsl(150, 80%, 60%);
                  |    --fg-fail: hsl(0, 100%, 55%);
                  |    --fg-compilefail: hsl(30, 100%, 55%);
                  |    --bg-highlight: hsl(53.5, 70%, 25%);
                  |
                  |    --bg-chosen: hsl(120, 0%, 30%);
                  |    --bg-chosen-alt: hsl(120, 0%, 26%);
                  |    --bg-row: hsl(120, 0%, 20%);
                  |    --bg-row-alt: hsl(120, 0%, 16%);
                  |
                  |    --border-table: #888;
                  |  }
                  |  .pass { font-weight: bold; }
                  |}
                  |
                  |body {
                  |  background: var(--bg-page);
                  |  color: var(--fg-text);
                  |}
                  |
                  |.pass { color: var(--fg-pass); }
                  |.fail { color: var(--fg-fail); }
                  |.compilefail { color: var(--fg-compilefail); }
                  |.chosen { background-color: var(--bg-chosen); }
                  |.mine, .mine td { background-color: var(--bg-highlight) !important; }
                  |
                  |h3, pre { margin: 0; }
                  |h1 { margin: 0 0 0.67em 0; }
                  |
                  |.results { font-family: monospace; }
                  |.weights { font-family: monospace; }
                  |.weights td { padding-right: 1.0em; }
                  |.weights tfoot td { font-weight: bold; }
                  |
                  |.results td.alias + td { border-right: 1px solid var(--border-table); }
                  |.results thead { position: sticky; top: 0; z-index: 1; }
                  |.results thead tr:last-child td { border-bottom: 1px solid var(--border-table); }
                  |
                  |.results thead td { background: var(--bg-page); }
                  |.results td.alias { position: sticky; left: 0; }
                  |.results td.alias::after {
                  |  --width: 0.6em;
                  |  content: "";
                  |  position: absolute;
                  |  top: 0; bottom: 0;
                  |  right: calc(-1 * var(--width) - 1px);
                  |  width: var(--width);
                  |  background: inherit;
                  |  border-right: 1px solid var(--border-table);
                  |}
                  |
                  |/* Alternating row styles: */
                  |.results { border-collapse: separate; border-spacing: 0; }
                  |.results td { padding: 2px; }
                  |.results tbody tr:nth-child(2n + 1) td { background: var(--bg-row); }
                  |.results tbody tr:nth-child(2n) td { background: var(--bg-row-alt); }
                  |.results tbody tr:nth-child(2n + 1) .chosen { background: var(--bg-chosen); }
                  |.results tbody tr:nth-child(2n) .chosen { background: var(--bg-chosen-alt); }
                  |
                  |/* Original (unintentional) spacing: */
                  |/* .results td { padding-bottom: calc(1em + 1px); } */
                  |
                  |/* More relaxed spacing: */
                  |.results td { padding: 0.15em 0.3em; }
                  |""".stripMargin))
              // script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js" )
            }
            body {
              table {
                tr(td(h1(text(s"${p.course.course_name}_${p.project_name}"))))
                tr(td(times))
                tr(td(tbl))
                tr(td(text("")))
                tr(td(text(s"${enrollment.keySet.size} enrollments")))
                tr(td(text(s"${submissions.size} submissions")))
                tr(td(text(s"${testNames.size} tests")))
                tr {
                  td.colspan("3") {
                    ignored
                  }
                }
                tr {
                  td.colspan("3") {
                    weights_table
                  }
                }
              }
              // script.src("highlight_on_click.js")
              script(text("""
                  |// Highlight row when its alias/hash is clicked
                  |document.querySelectorAll(".results td[title]")
                  |  .forEach((e) => {
                  |    e.addEventListener("click", () => {
                  |      e.parentElement.classList.toggle("mine");
                  |    })
                  |  });
                  |""".stripMargin))

              // Script to allow pinning certain cases to the left side of the matrix
              style(text("""
                  |.selectors td { padding: 0; text-align: center; }
                  |.selectors input { margin: 0; }
                  |.row-pin { padding: 0; }
                  |.row-pin input { margin: 0; }
                  """.stripMargin))
              script(text("""
                  |let assignment = document.querySelector("h1").textContent;
                  |let config_key = `~gheith/${assignment}.html#config`;
                  |let config = JSON.parse(window.localStorage.getItem(config_key) || "{}");
                  |config.pinned_tests = config.pinned_tests || {};
                  |config.pinned_aliases = config.pinned_aliases || {};
                  |
                  |function saveConfig() {
                  |  window.localStorage.setItem(config_key, JSON.stringify(config));
                  |}
                  |
                  |let table = document.querySelector(".results");
                  |let head = table.querySelector("thead");
                  |let body = table.querySelector("tbody");
                  |
                  |let first_header_row = head.querySelector("tr:first-child");
                  |let first_body_row = body.querySelector("tr:first-child");
                  |let selectors;
                  |let base_col_idx = 2;
                  |
                  |// Sort the columns of a table; if initial_index is specified,
                  |// assumes that only that column has moved for performance.
                  |function sortTableColumns(initial_index) {
                  |  let selector_cells = selectors.children;
                  |  let first_row_cells = first_body_row.children;
                  |  let columns = Array.from(first_header_row.children)
                  |    .map((cell, i) => [
                  |      i,
                  |      cell.dataset.id || "",
                  |      first_row_cells[i].classList.contains("chosen"),
                  |      selector_cells[i].firstElementChild?.checked || false,
                  |    ])
                  |    .sort(([ai, aid, achosen, aselected], [bi, bid, bchosen, bselected]) => {
                  |      return (+(bi < base_col_idx) - +(ai < base_col_idx)) || (+bselected - +aselected)
                  |        || (+bchosen - +achosen) || (aid.length - bid.length)
                  |        || aid.localeCompare(bid);
                  |    });
                  |
                  |  table.style.display = "none";
                  |  if (initial_index != undefined) {
                  |    let dest_index;
                  |    columns.forEach(([i,], j) => {
                  |      if (initial_index == i) dest_index = j;
                  |    });
                  |    if (dest_index >= initial_index) dest_index += 1;
                  |
                  |    table.querySelectorAll("tr").forEach((row) => {
                  |      row.insertBefore(row.children[initial_index], row.children[dest_index]);
                  |    });
                  |  } else {
                  |    table.querySelectorAll("tr").forEach((row) => {
                  |      let children = Array.from(row.children);
                  |      let reordered_children = columns.map(([i,]) => children[i]);
                  |      row.replaceChildren(...reordered_children);
                  |    });
                  |  }
                  |  table.style.display = "initial";
                  |}
                  |
                  |// Create a row of checkboxes to set whether each column is pinned
                  |selectors = document.createElement("tr");
                  |selectors.classList.add("selectors");
                  |for (let i = 0; i < first_header_row.childElementCount; i++) {
                  |  let cell = document.createElement("td");
                  |  if (i >= base_col_idx) {
                  |    let id = first_header_row.children[i].dataset.id;
                  |    let input = document.createElement("input");
                  |    input.type = "checkbox";
                  |    input.checked = config.pinned_tests[id] || false;
                  |    input.addEventListener("change", () => {
                  |      config.pinned_tests[id] = input.checked;
                  |      sortTableColumns(Array.prototype.indexOf.call(selectors.children, cell));
                  |      saveConfig();
                  |    });
                  |    cell.appendChild(input);
                  |  }
                  |  if (i == 0) {
                  |    cell.classList.add("alias");
                  |    cell.textContent = "pinned:";
                  |  }
                  |  selectors.appendChild(cell);
                  |}
                  |head.appendChild(selectors);
                  |sortTableColumns();
                  |
                  |function sortRows(element) {
                  |  let sorted = Array.from(element.children)
                  |    .sort((a, b) => (a.dataset.alias || "").localeCompare(b.dataset.alias || ""));
                  |  element.replaceChildren(...sorted);
                  |}
                  |// Create a column of checkboxes to set whether each row is pinned
                  |table.querySelectorAll("tr").forEach((row) => {
                  |  let cell = document.createElement("td");
                  |  cell.classList.add("row-pin");
                  |  if (row.parentElement == body) {
                  |    let input = document.createElement("input");
                  |    input.type = "checkbox";
                  |    input.checked = config.pinned_aliases[row.dataset.alias] || false;
                  |    if (input.checked) head.appendChild(row);
                  |
                  |    input.addEventListener("change", () => {
                  |      let target = input.checked ? head : body;
                  |      target.appendChild(row);
                  |      sortRows(target);
                  |      config.pinned_aliases[row.dataset.alias] = input.checked;
                  |      saveConfig();
                  |    });
                  |    cell.appendChild(input);
                  |  }
                  |  row.insertBefore(cell, row.firstChild);
                  |});
                  |base_col_idx = 3;
                  |sortRows(head);
                  |""".stripMargin))
            }
          }
        }

        // sorted submissions

        ()
      case _ =>
        say(
          s"---> not generating results for ${p.course.course_name}:${p.project_name}"
        )
    }

}
