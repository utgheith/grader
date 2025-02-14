package ag.rules

import ag.common.{given_ReadWriter_Path, given_ReadWriter_SortedSet}
import scala.collection.SortedSet
import upickle.default.ReadWriter

case class SignedPaths(paths: SortedSet[os.Path]) derives ReadWriter {}
