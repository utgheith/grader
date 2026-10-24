package ag.common

// upickle reads a JSON null as a Java null for anything but Option (a case
// class, an Either, a String, ...), so data written in an older format -- say,
// a None, saved as null, where the type is now an Either -- can look readable.
// Saved and published data never legitimately contains a null, so finding one
// means the data has to be produced again.
object NoNulls {

  // Where the first null is (e.g. "$.prepare_info.sha"), if there is one.
  def find(x: Any, path: String = "$"): Option[String] = x match {
    case null            => Some(path)
    case _: String       => None
    case o: Option[?]    => o.flatMap(find(_, path))
    case e: Either[?, ?] =>
      e.fold(find(_, s"$path.left"), find(_, s"$path.right"))
    case m: scala.collection.Map[?, ?] =>
      m.iterator
        .flatMap { (k, v) =>
          find(k, s"$path[key]").orElse(find(v, s"$path[${String.valueOf(k)}]"))
        }
        .nextOption()
    case it: Iterable[?] =>
      it.iterator.zipWithIndex
        .flatMap((v, i) => find(v, s"$path[$i]"))
        .nextOption()
    case p: Product =>
      p.productIterator
        .zip(p.productElementNames)
        .flatMap((v, name) => find(v, s"$path.$name"))
        .nextOption()
    case _ => None
  }

  // `a`, or an exception if it holds a null anywhere.
  def check[A](a: A, what: => String): A = find(a) match {
    case Some(where) =>
      throw new IllegalStateException(
        s"$what has a null at $where (written in an older format?)"
      )
    case None => a
  }
}
