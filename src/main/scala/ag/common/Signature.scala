package ag.common

import upickle.default.{ReadWriter, readwriter}

case class Signature(it: String)

object Signature {
  given ReadWriter[Signature] = readwriter[String].bimap[Signature](
    _.it,
    Signature.apply
  )

  def apply(bytes: Array[Byte]): Signature =
    val it = bytes.map(b => f"$b%02x").mkString("")
    Signature(it)

}
