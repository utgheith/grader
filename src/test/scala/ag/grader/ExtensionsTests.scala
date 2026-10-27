package ag.grader

import munit.FunSuite

class ExtensionsTests extends FunSuite {
  test("extensions read from courses_config's form, either phase optional") {
    val e = upickle.default.read[Extensions](
      """{"phase1": {"ada123": 2, "bob456": 1}}"""
    )
    assertEquals(e.days(CSID("ada123"), Phase.One), 2)
    assertEquals(e.days(CSID("bob456"), Phase.One), 1)
    assertEquals(e.days(CSID("ada123"), Phase.Two), 0)
    assertEquals(e.days(CSID("cat789"), Phase.One), 0)
    assertEquals(upickle.default.read[Extensions]("{}"), Extensions())
  }

  test("the latest results take phase 2's extension") {
    val e = Extensions(phase2 = Map(CSID("ada123") -> 3))
    assertEquals(e.days(CSID("ada123"), Phase.Latest), 3)
  }

  test("a misspelt phase is an error, not ignored") {
    assert(
      scala.util
        .Try(upickle.default.read[Extensions]("""{"phase3": {"a": 1}}"""))
        .isFailure
    )
  }
}
