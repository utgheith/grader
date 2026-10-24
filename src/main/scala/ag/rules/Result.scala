package ag.rules

import ag.common.Signature
import upickle.default.ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class Result[+T](value: T, signature: Signature) derives ReadWriter
