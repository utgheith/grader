package ag.grader

import java.io.FileWriter
import ag.rules.{say, Rule}
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import scala.collection.SortedMap
import java.time.ZonedDateTime
import java.time.ZoneId

trait HtmlContext {
  def doctype(): Unit
  def start(tag: String, attributes: Seq[(String, String)]): Unit
  def start_self_closing(tag: String, attributes: Seq[(String, String)]): Unit
  def text(s: String): Unit
  def end(tag: String): Unit
}

class FileContext(path: os.Path) extends HtmlContext with AutoCloseable {

  val w = new FileWriter(path.toIO).nn

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

  def start_self_closing(tag: String, attributes: Seq[(String, String)]): Unit = {
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

  def close() = {
    w.close()
  }
}

val self_closing_tags = Set("meta")

case class Element(tag: String, attributes: Seq[(String, String)]) {
  def attr(name: String, value: String | Null): Element = value match {
    case s: String => this.copy(attributes = attributes :+ (name, s))
    case null         => this
  }
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

  val displayFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm (EEEE)").nn


  val gen_html =
    Rule(
      p.results *: 
        Config.site_base *: 
        p.chosen *:
        p.weights *:
        p.bad_tests *:
        p.test_extensions *:
        p.test_cutoff *:
        p.code_cutoff *:
        p.course.enrollment,
      p.scope
    ) {
      case (Some(results), Some(site_base), chosen, weights, bad_tests, test_extensions_, test_cutoff, code_cutoff, enrollment) =>

        val test_extensions = test_extensions_.to(IndexedSeq)

        val (chosenTestNames, otherTestNames) = results.values.to(List)
            .flatMap(
              _.outcomes.keys.filterNot(test => bad_tests.contains(test.external_name))
            )
            .sorted
            .distinct
            .groupBy(_.external_name.length)
            .toSeq
            .map { case (len, names) => (len, names.sorted) }
            .sortBy(_._1)
            .flatMap(_._2)
            .partition(test => chosen.contains(test.external_name))

        // All the test name properly ordered
        val testNames: Seq[RedactedTestId] = chosenTestNames ++ otherTestNames

        val submissions: List[(Alias, String)] = (for {
          s <- results.values
          a <- s.alias.to(Seq)
          sha = s.prepare_info.sha
        } yield (a, sha)).to(List).sortBy(_._1)

        def testTitle(t: String, rowNum: Int)(using HtmlContext) = {
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
        def times(using HtmlContext) = table {
            tr {
              td { text("generated") }
              td { text(displayFormat.format(LocalDateTime.now().nn).nn) }
            }
            tr {
              td { text("test cutoff") }
              td { text(displayFormat.format(test_cutoff).nn) }
            }
            tr {
              td { text("code cutoff") }
              td { text(displayFormat.format(code_cutoff).nn) }
            }
        }

        // Display the results table
        def tbl(using HtmlContext) = table.css_class("results") {
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
                val short_name = s"${alias.toString}_$sha".toString.take(8)
                val result = results(alias)
                val outcome: SortedMap[String, RedactedOutcome] = result.outcomes.map { case (k,v) => (k.external_name, v) }

                val is_mine = false // outcome.is_mine
                val is_late = result.prepare_info.commit_time.isAfter(ZonedDateTime.of(code_cutoff, ZoneId.systemDefault()))
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
                    val o: Option[RedactedOutcome] = outcome.get(t.external_name)
                    val (status_class, the_text) =
                      o.flatMap(_.outcome) match {
                        case Some("pass") =>
                          ("pass", ".")
                        case Some(_) =>
                          ("fail", "X")
                        case None =>
                          ("compilefail", "?")
                      }
                    val chosen_class = if (chosen.contains(t.external_name)) List("chosen") else List.empty
                    val the_class = List(status_class) ::: chosen_class
                    val ttl = o match {
                      case Some(RedactedOutcome(_, _, _, _, Some(time), tries)) =>
                        s"$tries tries, last took ${time.round}s"
                      case Some(RedactedOutcome(_, _, _, _, None, tries)) =>
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
        
        def ignored(using HtmlContext) = table {
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

        def weights_table(using HtmlContext) = table {
            tr { td { h3 { pre { text("Selected test weights") } } } }
            tr {
              td {
                table.css_class("weights") {
                  tr {
                    td.style("font-weight: bold;") { text("test") }
                    td.style("font-weight: bold;") { text("weight") }
                  }
                  var total_weight = 0

                  // Match the sorting of test cases elsewhere (t0-4 before user test cases)
                  val sorted = chosen.toList.sortBy(c => (c.length, c))
                  sorted.foreach { c =>
                    val weight = weights.find { w =>
                      scala.util.matching.Regex(w.pattern).matches(c)
                    }
                    total_weight += (weight match {
                      case Some(w) => w.weight
                      case None => 0
                    })

                    val weight_str = weight match {
                      case Some(w) => w.weight.toString
                      case None => "?"
                    }
                    tr {
                      td { text(c) }
                      td { text(weight_str) }
                    }
                  }

                  tr {
                    td.style("font-weight: bold;") { text("total") }
                    td { text(total_weight.toString) }
                  }
                }
              }
            }
          }

        val f = os.Path(site_base) / s"${p.course.course_name}_${p.project_name}.html"
          val perms = os.PermSet.fromString("rwxr--r--")

          say(s"  $f")

          os.write.over(f, "")
          os.perms.set(f, perms)

          scala.util.Using(FileContext(f)) { file =>

            html(file) {
              head {
                meta.attr("charset", "utf8") { }
                meta.attr("name", "viewport").attr("content", "width=device-width,initial-scale=1") { }
                title(text(s"${p.course.course_name}_${p.project_name}"))
                style(text("""
:root {
  color-scheme: light dark;
  --bg-page: #FFF;
  --fg-text: #000;

  --fg-pass: green;
  --fg-fail: red;
  --fg-compilefail: #933;
  --bg-highlight: hsl(53.5, 100%, 71.2%);

  --bg-chosen: hsl(120, 0%, 95%);
  --bg-chosen-alt: hsl(120, 0%, 88%);
  --bg-row: #FFF;
  --bg-row-alt: #F5F5F5;

  --border-table: #CCC;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bg-page: #222;
    --fg-text: #EEE;

    --fg-pass: hsl(150, 80%, 60%);
    --fg-fail: hsl(0, 100%, 55%);
    --fg-compilefail: hsl(30, 100%, 55%);
    --bg-highlight: hsl(53.5, 70%, 25%);

    --bg-chosen: hsl(120, 0%, 30%);
    --bg-chosen-alt: hsl(120, 0%, 26%);
    --bg-row: hsl(120, 0%, 20%);
    --bg-row-alt: hsl(120, 0%, 16%);

    --border-table: #888;
  }
  .pass { font-weight: bold; }
}

body {
  background: var(--bg-page);
  color: var(--fg-text);
}

.pass { color: var(--fg-pass); }
.fail { color: var(--fg-fail); }
.compilefail { color: var(--fg-compilefail); }
.chosen { background-color: var(--bg-chosen); }
.mine, .mine td { background-color: var(--bg-highlight) !important; }

h3, pre { margin: 0; }
h1 { margin: 0 0 0.67em 0; }

.results { font-family: monospace; }
.weights { font-family: monospace; }
.weights td:first-child { padding-right: 0.5em; }
.weights td:nth-child(2) { text-align: right; }

.results td.alias + td { border-right: 1px solid var(--border-table); }
.results thead { position: sticky; top: 0; z-index: 1; }
.results thead tr:last-child td { border-bottom: 1px solid var(--border-table); }

.results thead td { background: var(--bg-page); }
.results td.alias { position: sticky; left: 0; }
.results td.alias::after {
  --width: 0.6em;
  content: "";
  position: absolute;
  top: 0; bottom: 0;
  right: calc(-1 * var(--width) - 1px);
  width: var(--width);
  background: inherit;
  border-right: 1px solid var(--border-table);
}

/* Alternating row styles: */
.results { border-collapse: separate; border-spacing: 0; }
.results td { padding: 2px; }
.results tbody tr:nth-child(2n + 1) td { background: var(--bg-row); }
.results tbody tr:nth-child(2n) td { background: var(--bg-row-alt); }
.results tbody tr:nth-child(2n + 1) .chosen { background: var(--bg-chosen); }
.results tbody tr:nth-child(2n) .chosen { background: var(--bg-chosen-alt); }

/* Original (unintentional) spacing: */
/* .results td { padding-bottom: calc(1em + 1px); } */

/* More relaxed spacing: */
.results td { padding: 0.15em 0.3em; }
"""))
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
                //script.src("highlight_on_click.js")
                script(text("""
// Highlight row when its alias/hash is clicked
document.querySelectorAll(".results td[title]")
  .forEach((e) => {
    e.addEventListener("click", () => {
      e.parentElement.classList.toggle("mine");
    })
  });
"""))
              }
            }
          }
            

        // sorted submissions
        
        ()
      case _ =>
        say(s"---> not generating results for ${p.course.course_name}:${p.project_name}")
    }




}
