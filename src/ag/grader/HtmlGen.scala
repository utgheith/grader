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

            td {
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
            /* 3 headers */
            for (i <- 0 to 2) {
              tr {
                td { text("") }
                td { text("") }
                testNames.foreach { t =>
                  testTitle(t.external_name, i)
                }
              }
            }

            /* the actual results, one row per submission */
            submissions.foreach { case (alias, sha) =>
              val short_name = s"${alias.toString}_$sha".toString.take(8)
              val result = results(alias)
              val outcome: SortedMap[String, RedactedOutcome] = result.outcomes.map { case (k,v) => (k.external_name, v) }

              val is_mine = false // outcome.is_mine
              val is_late = result.prepare_info.commit_time.isAfter(ZonedDateTime.of(code_cutoff, ZoneId.systemDefault()))
              tr.css_class(if (is_mine) "mine" else null) {
                td.title(sha) {
                  if (is_late) text(s"$short_name*")
                  else text(short_name)
                }
                td {
                  text("")
                }
                testNames.foreach { t =>
                  val (status_class, the_text) =
                    outcome.get(t.external_name).flatMap(_.outcome) match {
                      case Some("pass") =>
                        ("pass", ".")
                      case Some(_) =>
                        ("fail", "X")
                      case None =>
                        ("compilefail", "?")
                    }
                  val chosen_class = if (chosen.contains(t.external_name)) List("chosen") else List.empty
                  val the_class = List(status_class) ::: chosen_class
                  td.css_class(the_class) {
                    text(the_text)
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
.pass { color: green; }
.fail { color: red; }
.compilefail { color: #933; }
.chosen { background-color: #DDD; }
.mine, .mine td { background-color: hsl(53.5, 100%, 71.2%) !important; }

h3, pre { margin: 0; }
h1 { margin: 0 0 0.67em 0; }

.results { font-family: monospace; }
.weights { font-family: monospace; }
.weights td:first-child { padding-right: 0.5em; }
.weights td:nth-child(2) { text-align: right; }

.results td:nth-child(2) { border-right: 1px solid #CCC; }
.results tr:nth-child(3) { border-bottom: 1px solid #CCC; }

/* Alternating row styles: */
.results { border-collapse: collapse; }
.results td { padding: 2px; }
.results tr:nth-child(2n) { background: #FFF; }
.results tr:nth-child(2n + 5) { background: #F5F5F5; }
.results tr:nth-child(2n) .chosen { background: hsl(120, 0%, 95%); }
.results tr:nth-child(2n + 5) .chosen { background: hsl(120, 0%, 88%); }

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