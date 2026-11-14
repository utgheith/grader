package ag.r3

// How much of a value there is, as Target.tally counts it: a collection's
// size, 1 for Some and 0 for None, 1 for true and 0 for false.
trait Measure[A] {
  def measure(a: A): Long
}

object Measure {
  given iterable: [C <: Iterable[?]] => Measure[C] = (c: C) => c.size.toLong
  given option: [X] => Measure[Option[X]] = (o: Option[X]) =>
    if (o.isDefined) 1L else 0L
  given boolean: Measure[Boolean] = (b: Boolean) => if (b) 1L else 0L

  // a target's kind as shown: its definition's last two names
  def label(kind: os.RelPath): String =
    kind.segments.takeRight(2).mkString(".")
}
