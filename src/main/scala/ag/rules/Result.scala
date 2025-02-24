package ag.rules

import ag.common.Signature
import upickle.default.ReadWriter

case class Result[+T](value: T, signature: Signature) derives ReadWriter
