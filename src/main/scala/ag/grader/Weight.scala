package ag.grader

import upickle.default.ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class Weight(
    pattern: String,
    weight: Int
) derives ReadWriter
