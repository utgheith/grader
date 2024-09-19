package ag.gitolite

import upickle.default.{readwriter, ReadWriter}
import scala.math.Ordering

case class Alias(value: Int) {
  override def toString: String = {
    if (value >= 1000) throw new Exception(s"alias is too large: $value")
    f"$value%03d"
  }
}

object Alias {
  def apply(s: String): Alias = new Alias(s.toInt)

  given ReadWriter[Alias] = readwriter[Int].bimap(
    alias => alias.value,
    value => Alias(value)
  )
  given Ordering[Alias] = Ordering.by(_.value)
}