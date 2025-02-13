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
    phase1_tests: Seq[String],
    phase1_weight: Int,
    phase2_tests: Seq[String],
    phase2_weight: Int,
    test_weights: Seq[Weight],
    bad_tests: Seq[String],
    docker_file: Option[String],
    test_extensions: Seq[String],
    staff: Seq[CSID]
) derives ReadWriter
