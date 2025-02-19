package ag.r2

import ag.common.Lazy
import ag.rules.Signature

import scala.collection.SortedMap
import scala.concurrent.Future

//
// Make runs in 2 phases:
//    (1) collect the dependencies
//    (2) produce the results
//
// We need to defer phase2 because we might already have a cached value.
// Scala futures are eager so we force the result to be lazy.
//
case class Made[+A](dependencies: SortedMap[os.RelPath, Signature], result: Lazy[Future[Result[A]]])
