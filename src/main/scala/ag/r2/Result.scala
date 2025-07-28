package ag.r2

import ag.common.Signature
import upickle.default.ReadWriter

// The result of a computation: the value and its signature
case class Result[+A](value: A, signature: Signature) derives ReadWriter
