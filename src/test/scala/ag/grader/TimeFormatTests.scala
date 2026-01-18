package ag.grader

class TimeFormatTests extends munit.FunSuite {

  test("t1") {
    assert(Project.parseTime(Some("1:20:30.40")) == Some(3600 + 20 * 60 + 30.4))
    assert(Project.parseTime(Some("20:30.40")) == Some(20 * 60 + 30.4))
    assert(Project.parseTime(Some("30.40")) == Some(30.4))
    assert(Project.parseTime(None) == None)
    try {
      val _ = Project.parseTime(Some("foo"))
      fail("should have failed")
    } catch {
      case e: Exception =>
        assert(e.getMessage.contains("foo"))
    }

  }

}
