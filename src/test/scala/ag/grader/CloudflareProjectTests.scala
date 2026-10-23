package ag.grader

import munit.FunSuite

class CloudflareProjectTests extends FunSuite {
  test("a course's Pages project name uses only lowercase, digits and dashes") {
    assertEquals(Course("cs439t_f26").cloudflare_project, "cs439t-f26")
    assertEquals(Course("CS429H_S22").cloudflare_project, "cs429h-s22")
  }
}
