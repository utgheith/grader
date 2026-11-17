package ag.grader.json

import ag.grader.name.Alias
import upickle.default.ReadWriter

import scala.collection.SortedMap
import scala.util.Try

// What a published results file (results_<milestone>.json, by alias) says
// about one student's runs, read leniently: the commit graded, and each
// test's outcomes by its external name. Only what every version of the
// grader has published (r2's included) is read, so a file this version
// can't read as RedactedStudentResults still gives its runs: they seed a
// test's runs when it has none saved (see TestRun.runs).
@upickle.implicits.allowUnknownKeys(false)
final case class PublishedRuns(
    sha: String,
    runs: SortedMap[String, Seq[(OutcomeStatus, Option[Double])]]
) derives ReadWriter

object PublishedRuns {
  // Each student's in a results file: a list of [alias, results] pairs.
  // A student whose entry can't be read is left out, and a test whose
  // outcomes can't be.
  def parse(json: String): SortedMap[Alias, PublishedRuns] = {
    val pairs = Try(ujson.read(json).arr.toSeq).getOrElse(Seq())
    (for {
      pair <- pairs
      (a, r) <- Try((pair.arr(0), pair.arr(1))).toOption.toSeq
      alias <- Try(upickle.default.read[Alias](a)).toOption.toSeq
      sha <- Try(r("prepare_info")("sha").str).toOption.toSeq
    } yield {
      val runs = for {
        o <- Try(r("outcomes").arr.toSeq).getOrElse(Seq())
        name <- Try(o.arr(0)("external_name").str).toOption.toSeq
        outcomes <- Try(
          upickle.default.read[Seq[(OutcomeStatus, Option[Double])]](
            o.arr(1)("outcomes")
          )
        ).toOption.toSeq
      } yield name -> outcomes
      alias -> PublishedRuns(sha, runs.to(SortedMap))
    }).to(SortedMap)
  }
}
