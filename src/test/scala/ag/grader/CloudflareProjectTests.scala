package ag.grader

import munit.FunSuite

class CloudflareProjectTests extends FunSuite {
  private def cf(prefix: Option[String]) =
    CloudflareConfig(true, "id", "key", Some("classprojects.net"), prefix)

  test("a course's Pages project name uses only lowercase, digits and dashes") {
    assertEquals(cf(None).project_of("cs439t_f26"), "cs439t-f26")
    assertEquals(cf(None).project_of("CS429H_S22"), "cs429h-s22")
  }

  test("a prefix makes a test setup's projects its own") {
    assertEquals(cf(Some("test-")).project_of("cs439t_f26"), "test-cs439t-f26")
  }

  test("the prefix is optional in config.json") {
    val c = upickle.default.read[CloudflareConfig](
      """{"enabled": false, "id": "i", "api_key": "k", "domain": null}"""
    )
    assertEquals(c.project_prefix, None)
  }
}
