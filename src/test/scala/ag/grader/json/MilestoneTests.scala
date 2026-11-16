package ag.grader.json

import ag.grader.name.CSID

import ag.grader.Milestone

import ag.grader.json.{RawProject, RawMilestone}

import munit.FunSuite

import java.time.LocalDateTime
import scala.util.Try

// A project's milestones, as courses_config has them (RawProject,
// RawMilestone), and the old two-phase form read as two milestones
class MilestoneTests extends FunSuite {
  private val ada = CSID("ada123")
  private val bob = CSID("bob456")
  private val first = LocalDateTime.of(2026, 10, 3, 23, 59).nn
  private val last = LocalDateTime.of(2026, 10, 10, 23, 59).nn

  private def project(milestones: String): Try[RawProject] = Try(
    upickle.default.read[RawProject](
      s"""{"active": true, "cores": 1, "ignore_tests": false,
         |"milestones": [$milestones],
         |"test_weights": [], "bad_tests": [], "docker_file": null,
         |"test_extensions": ["cc", "ok"], "staff": [],
         |"iterations": {"min": 1, "max": 2}}""".stripMargin
    )
  )

  private val two =
    """{"name": "checkpoint", "deadline": "2026:10:03:23:59:00", "tests": ["t0"], "weight": 30,
      | "extensions": {"ada123": 2, "bob456": 1}},
      |{"name": "final", "deadline": "2026:10:10:23:59:00", "tests": ["t0", "t1"], "weight": 70,
      | "extensions": {"ada123": 3}}""".stripMargin

  test("milestones read from courses_config's form, extensions and all") {
    val rp = project(two).get
    val (cp, fin) = (Milestone.Named("checkpoint"), Milestone.Named("final"))
    assertEquals(rp.all_milestones, Seq(cp, fin, Milestone.Live))
    assertEquals(rp.extension_days(ada, cp), 2)
    assertEquals(rp.extension_days(bob, cp), 1)
    assertEquals(rp.extension_days(bob, fin), 0)
    assertEquals(rp.deadline(ada, cp), first.plusDays(2).nn)
    // Live: every milestone's tests and weight, late from the final deadline
    assertEquals(rp.tests(Milestone.Live), Seq("t0", "t1"))
    assertEquals(rp.weight(Milestone.Live), 100)
    assertEquals(rp.deadline(ada, Milestone.Live), last.plusDays(3).nn)
    // it reads back as it was written
    assertEquals(upickle.default.read[RawProject](upickle.default.write(rp)), rp)
  }

  test("what courses_config can't have") {
    // none, two by one name, out of order, named live
    assert(project("").isFailure)
    val final_ = """{"name": "final", "deadline": "2026:10:10:23:59:00", "tests": [], "weight": 1}"""
    assert(project(s"$final_, $final_").isFailure)
    assert(project(two.replace("2026:10:03", "2026:10:11")).isFailure)
    assert(project(final_.replace("final", "live")).isFailure)
    // a misspelt key is an error, not ignored
    assert(project(final_.replace("weight", "wieght")).isFailure)
  }

  test("the old two-phase form is read as milestones phase1 and phase2") {
    val rp = upickle.default.read[RawProject](
      """{"active": true, "cores": 1, "code_cutoff": "2026:10:10:23:59:00",
        |"test_cutoff": "2026:10:03:23:59:00", "ignore_tests": false,
        |"phase1_tests": ["t0"], "phase1_weight": 30, "phase2_tests": ["t1"],
        |"phase2_weight": 70, "test_weights": [], "bad_tests": [],
        |"docker_file": null, "test_extensions": ["ok"], "staff": [],
        |"extensions": {"phase1": {"ada123": 2}, "phase2": {}},
        |"iterations": {"min": 1, "max": 2}}""".stripMargin
    )
    assertEquals(
      rp.milestones,
      Seq(
        RawMilestone("phase1", first, Seq("t0"), 30, Map(ada -> 2)),
        RawMilestone("phase2", last, Seq("t1"), 70)
      )
    )
  }
}
