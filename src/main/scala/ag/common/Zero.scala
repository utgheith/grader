package ag.common

trait Zero[+A] {
  def zero: A
}

object Zero {

  def apply[A](using za: Zero[A]): A = za.zero

  given Zero[Int] = new Zero[Int] {
    override val zero: Int = 0
  }
  given Zero[String] = new Zero[String] {
    override val zero: String = ""
  }

  def apply[A](an: A | Null)(using za: Zero[A]): A =
    if an == null then za.zero else an
}
