package ag.rules

import upickle.default.ReadWriter

case class Result[+T](value: T, signature: Signature) derives ReadWriter
