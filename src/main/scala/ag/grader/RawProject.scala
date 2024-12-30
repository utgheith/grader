package ag.grader

import ag.rules.given_ReadWriter_LocalDateTime
import upickle.default.ReadWriter
import java.time.LocalDateTime

@upickle.implicits.allowUnknownKeys(false)
case class RawProject(
    active: Boolean,
    cores: Int = 4,
    score_iterations: Int = 100,
    code_cutoff: LocalDateTime,
    test_cutoff: LocalDateTime,
    ignore_tests: Boolean,
    chosen: Seq[String],
    bad_tests: Seq[String],
    docker_file: Option[String],
    test_extensions: Seq[String],
    weights: Seq[Weight],
    staff: Seq[CSID]
) derives ReadWriter
