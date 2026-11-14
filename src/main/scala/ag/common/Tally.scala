package ag.common

import scala.collection.mutable

// Counts what a command's search goes through, stage by stage: wrap each
// generator (`apply`) and filter (`keep`) of its for comprehension, then
// `report`. Counts are summed across nested loops; a stage shows once it's
// first reached, so the stages after one that let nothing through don't
// show. Only counts are shown, never names. An r3 State keeps one, which
// Target.tally adds to (see ag.r3.Measure).
//
//   val f = Tally()
//   for {
//     c <- f("active courses", courses)
//     if f.keep("matching -c", re.matches(c))
//   } ...
//   f.report("bad_tests", found = n, verbose = v)
final class Tally {
  private val counts = mutable.LinkedHashMap[String, Long]()

  // adds `n` to the stage's count
  def add(label: String, n: Long): Unit = counts.synchronized {
    counts.update(label, counts.getOrElse(label, 0L) + n)
  }

  // the stage's items, counted
  def apply[A](label: String, xs: Seq[A]): Seq[A] = {
    add(label, xs.size.toLong)
    xs
  }

  // whether the item passes the stage; counted if it does
  def keep(label: String, pass: Boolean): Boolean = {
    add(label, if (pass) 1L else 0L)
    pass
  }

  // what `report` shows: a header, then each stage's count, marking the
  // first that let nothing through
  def lines(what: String, found: Int): Seq[String] = {
    val stages = counts.synchronized(counts.toSeq)
    val width = (stages.map(_._1.length) :+ 0).max
    val first_empty = stages.indexWhere(_._2 == 0L)
    val header =
      if (found == 0) s"$what: none found" else s"$what: $found found"
    header +: stages.zipWithIndex.map { case ((label, n), i) =>
      val mark = if (i == first_empty) "   <- nothing left here" else ""
      s"  ${label.padTo(width, ' ')}  ${n.toString.reverse.padTo(6, ' ').reverse}$mark"
    }
  }

  // shown if asked for, or if nothing was found (when it's most wanted)
  def report(
      what: String,
      found: Int,
      verbose: Boolean,
      out: String => Unit = println
  ): Unit =
    if (verbose || found == 0) lines(what, found).foreach(out)
}
