package ag.grader

import ag.common.given_ReadWriter_LocalDateTime
import upickle.default.ReadWriter
import java.time.LocalDateTime

@upickle.implicits.allowUnknownKeys(false)
case class RawProject(
    active: Boolean,
    cores: Int,
    // TODO: drop this default once every project in courses.json lists it
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
    staff: Seq[CSID],
    // optional in courses_config, so (unlike saved values) it has a default
    extensions: Extensions = Extensions()
) derives ReadWriter

// Deadline extensions: per phase, the whole days a student gets past that
// phase's deadline. In courses_config, inside a project:
//   "extensions": { "phase1": { "<csid>": 2, ... }, "phase2": { ... } }
// Either phase may be left out.
@upickle.implicits.allowUnknownKeys(false)
case class Extensions(
    phase1: Map[CSID, Int] = Map(),
    phase2: Map[CSID, Int] = Map()
) derives ReadWriter {
  // the days `csid` gets for `phase`; the latest results are late from the
  // code cutoff, so they take phase 2's
  def days(csid: CSID, phase: Phase): Int =
    (phase match
      case Phase.One                => phase1
      case Phase.Two | Phase.Latest => phase2
    ).getOrElse(csid, 0)
}
