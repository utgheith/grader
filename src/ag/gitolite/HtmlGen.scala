package ag.gitolite

import java.io.FileWriter
import ag.rules.{say, Rule}
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import scala.collection.SortedMap
import java.time.ZonedDateTime
import java.time.ZoneId

trait HtmlContext {
  def start(tag: String, attributes: Seq[(String, String)]): Unit
  def text(s: String): Unit
  def end(tag: String): Unit
}

class FileContext(path: os.Path) extends HtmlContext with AutoCloseable {

  val w = new FileWriter(path.toIO).nn

  def start(tag: String, attributes: Seq[(String, String)]): Unit = {
    w.write(s"<$tag")
    attributes.foreach { (k, v) =>
      w.write(s" $k='$v'")
    }
    w.write(">")
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
  def textAlign(c: String | Null): Element =
    attr("textAlign", c)
  def title(c: String | Null): Element =
    attr("title", c)

  def apply(f: HtmlContext ?=> Any)(using c: HtmlContext): Unit = {
    c.start(tag, attributes)
    f(using c)
    c.end(tag)
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

def html[A](c: HtmlContext)(f: HtmlContext ?=> A): A = {
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
    DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm").nn


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
              pre {
                a.href(s"${p.tests_repo_name}/$t.$ext").title(fileName) {
                  text(ch.toString)
                }
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
        def tbl(using HtmlContext) = table {
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
              tr.bgcolor(if (is_mine) "yellow" else null) {
                td {
                  pre.title(sha) {
                    if (is_late) text(s"$short_name*")
                    else text(short_name)
                  }
                }
                td {
                  pre.textAlign("right") {
                    text("")
                  }
                }
                testNames.foreach { t =>
                  td.bgcolor(if (chosen.contains(t.external_name)) "LightGray" else null) {
                    val (the_style, the_text) =
                      outcome.get(t.external_name).flatMap(_.outcome) match {
                        case Some("pass") =>
                          ("color:green", ".")
                        case Some(_) =>
                          ("color:red", "X")
                        case _ =>
                          (null, "?")
                      }
                    pre.textAlign("center").style(the_style) {
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
            tr { td { h3 { pre { text("Weights") } } } }
            tr {
              td {
                table {
                  chosen.foreach { c =>
                    val weight = weights.find { w =>
                      scala.util.matching.Regex(w.pattern).matches(c)
                    } match {
                      case Some(w) => w.weight.toString
                      case None => "?"
                    }
                    tr {
                      td {
                        pre {
                          text(c)
                        }
                      }
                      td {
                        text(weight)
                      }
                    }
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
              //head(
              //    script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js" )
              //),
              body {
                table {
                  tr(td(h1(text(s"${p.course.course_name}_${p.project_name}"))))
                  tr(td(times))
                  tr(td(tbl))
                  tr(td(text("")))
                  tr(td(text(s"${enrollment.keySet.size} enrollements")))
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
              }
            }
          }
            

        // sorted submissions
        
        ()
      case _ =>
        say(s"---> not generating results for ${p.course.course_name}:${p.project_name}")
    }




}
