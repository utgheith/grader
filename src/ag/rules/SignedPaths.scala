package ag.rules

import scala.collection.SortedSet
import upickle.default.ReadWriter

case class SignedPaths(paths: SortedSet[os.Path]) derives ReadWriter {}
