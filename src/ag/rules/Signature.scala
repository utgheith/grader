package ag.rules

import upickle.default.{ReadWriter, readwriter}

import java.security.MessageDigest
import scala.collection.SortedSet
case class Signature(it: String)

object Signature {
  given ReadWriter[Signature] = readwriter[String].bimap[Signature](
    _.it,
    Signature.apply
  )

  def apply(bytes: Array[Byte]): Signature =
    val it = bytes.map(b => f"$b%02x").mkString("")
    Signature(it)

  def of(
      paths: SortedSet[os.Path],
      ignore_last: SortedSet[String]
  ): Signature = {
    val md = MessageDigest.getInstance("sha1").nn

    def one(base: os.Path, path: os.Path): Unit = {
      if (!ignore_last.contains(path.last)) {
        md.update(path.relativeTo(base).toString)
        if (os.isLink(path)) {
          md.update("L")
          os.followLink(path)
            .foreach(p => md.update(p.relativeTo(base).toString))
          md.update("l")
        } else if (os.isFile(path)) {
          // chuncks, stream, ...
          md.update("F")
          md.update(os.read.bytes(path))
          md.update("f")
        } else if (os.isDir(path)) {
          for {
            p <- os.list(path, sort = true)
          } one(base, p)
        } else if (os.exists(path)) {
          throw Exception(s"$path")
        }
      }
    }

    for {
      p <- paths
    } one(p, p)

    Signature(md.digest.nn)
  }

}
