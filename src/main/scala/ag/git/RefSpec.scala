package ag.git

case class RefSpec(
    force: Boolean,
    src: Option[os.RelPath],
    dest: Option[os.RelPath]
) {
  private def show_part(p: Option[os.RelPath]): String =
    p.map(_.toString).getOrElse("")
  override def toString: String =
    s"${(if (force) "+" else "").toString}${show_part(src)}:${show_part(dest)}"
}

object RefSpec {
  private def partAsRelPath(x: String | os.RelPath | Null): Option[os.RelPath] =
    x match {
      case s: String     => Some(os.RelPath(s))
      case p: os.RelPath => Some(p)
      case null          => None
    }
  def apply(
      force: Boolean = false,
      src: String | os.RelPath | Null = null,
      dest: String | os.RelPath | Null = null
  ): RefSpec =
    RefSpec(force = force, src = partAsRelPath(src), dest = partAsRelPath(dest))

  given Conversion[RefSpec, os.Shellable] = rs => os.Shellable(Seq(rs.toString))
}
