package ag.grader

// A target's path names its files in the workspace: if one changes, the target
// loses its saved value and its checkout.
class GitoliteTargetPathTests extends munit.FunSuite {
  private val course = Course("c")
  private val project = Project(course, "p")

  private def check_path(actual: ag.r2.TargetBase, expected: String): Unit =
    assertEquals(actual.path.toString, expected)

  test("course repo targets are named after their vals") {
    check_path(course.publish_keys, "ag/grader/Course/publish_keys/c")
    check_path(
      course.publish_enrollment,
      "ag/grader/Course/publish_enrollment/c"
    )
  }

  test("project repo targets are named after their vals") {
    check_path(
      project.publish_override_repo,
      "ag/grader/Project/publish_override_repo/c/p"
    )
    check_path(project.publish_tests, "ag/grader/Project/publish_tests/c/p")
  }
}
