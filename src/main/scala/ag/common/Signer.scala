package ag.common

import java.io.OutputStream
import java.security.{DigestOutputStream, MessageDigest}
import scala.util.Using
import upickle.default.{ReadWriter, writeBinaryTo}

// type-class for things that could be digitally signed
trait Signer[A] {
  def signWith(md: MessageDigest, a: A): Unit
  def sign(a: A): Signature = {
    val md = MessageDigest.getInstance("SHA1")
    signWith(md, a)
    Signature(md.digest())
  }
}

object Signer {

  def apply[A: Signer]: Signer[A] = summon[Signer[A]]

  def signWith[A: Signer](md: MessageDigest, a: A): Unit =
    apply[A].signWith(md, a)
  def sign[A: Signer](a: A): Signature = apply[A].sign(a)

  given [A: ReadWriter] => Signer[A] = { (md, a) =>
    Using.resource(DigestOutputStream(OutputStream.nullOutputStream(), md)) {
      dos =>
        writeBinaryTo(a, dos, sortKeys = true)
    }
  }

  // Sign a path with an exclusion list
  given Signer[(os.Path, os.RelPath => Boolean)] = {
    case (md, (base, exclude)) =>
      def one(path: os.Path): Unit = {
        if (!exclude(path.relativeTo(base))) {
          md.update(path.relativeTo(base).toString)
          if (os.isLink(path)) {
            md.update("L")
            os.followLink(path)
              .foreach(p => md.update(p.relativeTo(base).toString))
            md.update("l")
          } else if (os.isFile(path)) {
            md.update("F")
            Using.resource(
              DigestOutputStream(OutputStream.nullOutputStream(), md)
            ) { dos =>
              os.read.stream(path).writeBytesTo(dos)
            }
            md.update("f")
          } else if (os.isDir(path)) {
            for {
              p <- os.list(path, sort = true)
            } one(p)
          } else if (os.exists(path)) {
            throw Exception(s"$path")
          }
        }
      }
      one(base)
  }
  given Signer[os.Path] = { (md, base) =>
    signWith(md, (base, { (_: os.RelPath) => false }))
  }
}
