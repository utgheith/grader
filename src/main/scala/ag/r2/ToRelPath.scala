package ag.r2

import scala.compiletime.{summonAll, summonFrom, summonInline}

trait ToRelPath[A] {
  def apply(a: A): os.RelPath
}

object ToRelPath {
  def apply[A: ToRelPath](a: A): os.RelPath = summon[ToRelPath[A]](a)

  inline def opt[A](a: A): os.RelPath = summonFrom {
    case given ToRelPath[A] => summonInline[ToRelPath[A]](a)
    case _                  => ???
  }

  given ToRelPath[sourcecode.FullName] = fn =>
    os.RelPath(fn.value.split('.').mkString("/"))

  given ToRelPath[os.RelPath] = p => p

  given [T: ToRelPath] => ToRelPath[Option[T]] = {
    case Some(t) => ToRelPath(t)
    case None    => os.RelPath(".")
  }

  given [T: ToRelPath] => ToRelPath[Seq[T]] = { st =>
    st.map(t => ToRelPath(t)).foldLeft(os.RelPath("."))((a, v) => a / v)
  }

  given ToRelPath[String] = s => os.RelPath(s)

  given [N: Numeric] => ToRelPath[N] = n => ToRelPath(n.toString)

  given [A: ToRelPath, B: ToRelPath] => ToRelPath[(A, B)] = { case (a, b) =>
    ToRelPath(a) / ToRelPath(b)
  }

}
