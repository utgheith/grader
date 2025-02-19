package ag.r2

import ag.rules.Signature
import upickle.default.{macroRW, ReadWriter}

// The result of a computations: the value and its signature
case class Result[+A](value: A, signature: Signature)
object Result {
  given [A: ReadWriter] => ReadWriter[Result[A]] = macroRW
}
