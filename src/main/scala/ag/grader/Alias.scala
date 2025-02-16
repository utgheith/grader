package ag.grader

import upickle.default.{readwriter, ReadWriter}
import scala.math.Ordering

case class Alias(value: Int) extends Ordered[Alias] {
  override def toString: String = {
    if ((value < 0) || (value >= 1000)) throw new IllegalArgumentException(s"$value")
    f"$value%03d"
  }

  override def compare(that: Alias): Int = Ordering[Alias].compare(this, that)
}

object Alias {
  def apply(s: String): Alias = new Alias(s.toInt)

  given ReadWriter[Alias] = readwriter[Int].bimap(
    alias => alias.value,
    value => Alias(value)
  )
  given Ordering[Alias] = Ordering.by(_.value)
}
