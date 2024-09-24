package ag.rules

import scala.collection.SortedMap
import upickle.default.ReadWriter

case class Saved[+T](
    value: T,
    signature: Signature,
    dependsOn: SortedMap[os.RelPath, Signature]
) derives ReadWriter
