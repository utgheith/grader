package ag.grader

import ag.common.given_ReadWriter_LocalDateTime
import upickle.default.ReadWriter
import java.time.LocalDateTime

@upickle.implicits.allowUnknownKeys(false)
case class RawProject(
    active: Boolean,
    cores: Int,
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
    staff: Seq[CSID],
    extensions: Extensions,
    // how many times each test runs on a submission (see Priority)
    iterations: Iterations
) derives ReadWriter

// How many tries each test gets on a submission. In courses_config, inside a
// project:
//   "iterations": { "min": 10, "max": 100 }
// A live project gets its first `min` tries before any other work, and the
// rest (up to `max`) after all other work; other projects get up to `max`.
@upickle.implicits.allowUnknownKeys(false)
case class Iterations(min: Int, max: Int) derives ReadWriter {
  require(
    min >= 1 && max >= min,
    s"iterations: need 1 <= min <= max, got min $min and max $max"
  )
}

// Deadline extensions: per phase, the whole days a student gets past that
// phase's deadline. In courses_config, inside a project:
//   "extensions": { "phase1": { "<csid>": 2, ... }, "phase2": { ... } }
@upickle.implicits.allowUnknownKeys(false)
case class Extensions(
    phase1: Map[CSID, Int],
    phase2: Map[CSID, Int]
) derives ReadWriter {
  // the days `csid` gets for `phase`; the latest results are late from the
  // code cutoff, so they take phase 2's
  def days(csid: CSID, phase: Phase): Int =
    (phase match
      case Phase.One                => phase1
      case Phase.Two | Phase.Latest => phase2
    ).getOrElse(csid, 0)
}
