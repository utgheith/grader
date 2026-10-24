package ag.rules

import ag.common.{
  Signature,
  given_ReadWriter_RelPath,
  given_ReadWriter_SortedMap
}

import scala.collection.SortedMap
import upickle.default.ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class Saved[+T](
    value: T,
    signature: Signature,
    dependsOn: SortedMap[os.RelPath, Signature]
) derives ReadWriter
