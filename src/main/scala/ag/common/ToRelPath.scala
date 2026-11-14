package ag.common

import scala.annotation.unused
import scala.compiletime.{summonAll, summonFrom}
import scala.deriving.Mirror
import scala.util.matching.Regex

trait ToRelPath[A] {
  def apply(a: A): os.RelPath
}

object ToRelPath {
  def apply[A](a: A)(using ev: ToRelPath[A]): os.RelPath = ev(a)

  // One path component for `p`, whatever it is: its segments joined (with
  // their own %s escaped), or % for an empty path. Different paths give
  // different components, so fields can't run into each other.
  def component(p: os.RelPath): String = {
    if (p.ups != 0)
      throw IllegalArgumentException(s"a key can't go up: ${p.toString}")
    if (p.segments.isEmpty) "%"
    else p.segments.map(_.replace("%", "%25")).mkString("%2F")
  }

  // What a type that's never part of a key gives (say, what targets work
  // from, as opposed to which they are): `derived` leaves its fields out.
  final class Omitted[A] extends ToRelPath[A] {
    def apply(a: A): os.RelPath = os.rel
  }

  // `derives ToRelPath` for a case class: a component for each of its
  // fields, in order, but those whose type is Omitted (each field's type
  // needs a ToRelPath)
  inline def derived[A <: Product](using
      m: Mirror.ProductOf[A]
  ): ToRelPath[A] = {
    val fields = summonAll[Tuple.Map[m.MirroredElemTypes, ToRelPath]].toList
      .asInstanceOf[List[ToRelPath[Any]]]
    (a: A) =>
      a.productIterator
        .zip(fields)
        .collect {
          case (v, t) if !t.isInstanceOf[Omitted[?]] => component(t(v))
        }
        .foldLeft(os.rel)(_ / _)
  }

  inline def opt[A](a: A): os.RelPath = summonFrom {
    case ev: ToRelPath[A] => ev(a)
    case _                => ???
  }

  given ToRelPath[Boolean] = b => os.RelPath(if (b) "yes" else "no")

  given ToRelPath[sourcecode.FullName] = fn =>
    os.RelPath(fn.value.split('.').mkString("/"))

  given ToRelPath[os.RelPath] = p => p

  // one component, never empty, "." or "..", whatever the pattern
  given ToRelPath[Regex] = r =>
    os.RelPath(
      "re=" + r.toString.replace("%", "%25").replace("/", "%2F")
    )

  given [T: ToRelPath] => ToRelPath[Option[T]] = {
    case Some(t) => ToRelPath(t)
    case None    => os.RelPath(".")
  }

  given [T: ToRelPath] => ToRelPath[Seq[T]] = { st =>
    st.map(t => ToRelPath(t)).foldLeft(os.RelPath("."))((a, v) => a / v)
  }

  given ToRelPath[String] = s => os.RelPath(s)

  @unused
  given [N: Numeric] => ToRelPath[N] = n => ToRelPath(n.toString)

  given [A: ToRelPath, B: ToRelPath] => ToRelPath[(A, B)] = { case (a, b) =>
    ToRelPath(a) / ToRelPath(b)
  }

}
