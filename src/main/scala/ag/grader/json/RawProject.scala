package ag.grader.json

import ag.grader.name.CSID

import ag.grader.Milestone

import ag.common.given_ReadWriter_LocalDateTime
import upickle.default.ReadWriter
import java.time.LocalDateTime

// A project's settings, in courses_config, inside a course
@upickle.implicits.allowUnknownKeys(false)
case class RawProject(
    active: Boolean,
    cores: Int,
    ignore_tests: Boolean,
    // in deadline order; the last is the final deliverable
    milestones: Seq[RawMilestone],
    test_weights: Seq[Weight],
    bad_tests: Seq[String],
    docker_file: Option[String],
    test_extensions: Seq[String],
    staff: Seq[CSID],
    // how many times each test runs on a submission (see Priority)
    iterations: Iterations
) {
  require(milestones.nonEmpty, "a project needs a milestone")
  require(
    milestones.map(_.name).distinct.size == milestones.size,
    s"milestone names repeat: ${milestones.map(_.name).mkString(", ")}"
  )
  require(
    milestones
      .zip(milestones.drop(1))
      .forall((a, b) => !b.deadline.isBefore(a.deadline)),
    "milestones go in deadline order"
  )

  def all_milestones: Seq[Milestone] =
    milestones.map(m => Milestone.Named(m.name)) :+ Milestone.Live

  def named(name: String): RawMilestone =
    milestones
      .find(_.name == name)
      .getOrElse(throw IllegalArgumentException(s"no milestone $name"))

  // the milestone `milestone` is graded by: Live's is the final one
  private def of(milestone: Milestone): RawMilestone = milestone match
    case Milestone.Named(name) => named(name)
    case Milestone.Live        => milestones.last

  // A milestone's tests (by name) and weight: Live counts every milestone's
  def tests(milestone: Milestone): Seq[String] = milestone match
    case Milestone.Named(name) => named(name).tests
    case Milestone.Live        => milestones.flatMap(_.tests).distinct
  def weight(milestone: Milestone): Int = milestone match
    case Milestone.Named(name) => named(name).weight
    case Milestone.Live        => milestones.map(_.weight).sum

  // A milestone's deadline, and a student's, with their extension: Live is late
  // from the final one
  def deadline(milestone: Milestone): LocalDateTime = of(milestone).deadline
  def extension_days(csid: CSID, milestone: Milestone): Int =
    of(milestone).extensions.getOrElse(csid, 0)
  def deadline(csid: CSID, milestone: Milestone): LocalDateTime =
    deadline(milestone).plusDays(extension_days(csid, milestone)).nn
}

object RawProject {
  private val current: ReadWriter[RawProject] = upickle.default.macroRW

  // (also reads the old format, two phases, as two milestones: see Legacy)
  given ReadWriter[RawProject] =
    upickle.default
      .readwriter[ujson.Value]
      .bimap[RawProject](
        rp => upickle.default.writeJs(rp)(using current),
        js =>
          if (js.obj.contains("milestones"))
            upickle.default.read[RawProject](js)(using current)
          else upickle.default.read[Legacy](js).milestones
      )

  // The format before milestones, with exactly two: phase 1 (due at the test
  // cutoff) and phase 2 (at the code cutoff). Read
  // as milestones named phase1 and phase2, so that what's there now still
  // works; to go once no courses_config has it.
  @upickle.implicits.allowUnknownKeys(false)
  private case class Legacy(
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
      extensions: LegacyExtensions,
      iterations: Iterations
  ) derives ReadWriter {
    def milestones: RawProject = RawProject(
      active = active,
      cores = cores,
      ignore_tests = ignore_tests,
      milestones = Seq(
        RawMilestone(
          "phase1",
          test_cutoff,
          phase1_tests,
          phase1_weight,
          extensions.phase1
        ),
        RawMilestone(
          "phase2",
          code_cutoff,
          phase2_tests,
          phase2_weight,
          extensions.phase2
        )
      ),
      test_weights = test_weights,
      bad_tests = bad_tests,
      docker_file = docker_file,
      test_extensions = test_extensions,
      staff = staff,
      iterations = iterations
    )
  }

  @upickle.implicits.allowUnknownKeys(false)
  private case class LegacyExtensions(
      phase1: Map[CSID, Int],
      phase2: Map[CSID, Int]
  ) derives ReadWriter
}

// One of a project's milestones, in courses_config, inside a project:
//   { "name": "checkpoint", "deadline": "...", "tests": ["t0", ...],
//     "weight": 30, "extensions": { "<csid>": 2, ... } }
// `name` is in pages, file names and URLs. A submission is graded for it at
// the last commit by `deadline` (plus the student's `extensions`, whole
// days), on `tests`, which count `weight` (see HtmlGen.score), and on the
// students' own tests as of `deadline` (extensions are for code, not tests).
@upickle.implicits.allowUnknownKeys(false)
case class RawMilestone(
    name: String,
    deadline: LocalDateTime,
    tests: Seq[String],
    weight: Int,
    extensions: Map[CSID, Int] = Map()
) derives ReadWriter {
  require(
    name.matches("[A-Za-z0-9_]+") && name != Milestone.live,
    s"a milestone's name is letters, digits and _, and not ${Milestone.live}: $name"
  )
}

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
