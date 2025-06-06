package ag.r2

import scala.annotation.unused
import scala.compiletime.summonFrom
import scala.util.matching.Regex

trait ToRelPath[A] {
  def apply(a: A): os.RelPath
}

object ToRelPath {
  def apply[A](a: A)(using ev: ToRelPath[A]): os.RelPath = ev(a)

  inline def opt[A](a: A): os.RelPath = summonFrom {
    case ev: ToRelPath[A] => ev(a)
    case _                => ???
  }

  given ToRelPath[Boolean] = b => os.RelPath(if (b) "yes" else "no")

  given ToRelPath[sourcecode.FullName] = fn =>
    os.RelPath(fn.value.split('.').mkString("/"))

  given ToRelPath[os.RelPath] = p => p

  given ToRelPath[Regex] = r => os.RelPath(r.toString)

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
