package ag.r2

import ag.common.{
  Signature,
  given_ReadWriter_RelPath,
  given_ReadWriter_SortedMap
}
import upickle.default.{macroRW, ReadWriter}

import scala.collection.SortedMap

// The persistent state of a computed value
case class Saved[+A](
    result: Result[A],
    depends_on: SortedMap[os.RelPath, Signature]
)
object Saved {
  given [A: ReadWriter] => ReadWriter[Saved[A]] = macroRW
}
