package ag.grader

class TimeFormatTests extends munit.FunSuite {

  test("t1") {
    assert(TimeFormat.parse(Some("1:20:30.40")) == Some(3600 + 20 * 60 + 30.4))
    assert(TimeFormat.parse(Some("20:30.40")) == Some(20 * 60 + 30.4))
    assert(TimeFormat.parse(Some("30.40")) == Some(30.4))
    assert(TimeFormat.parse(None) == None)
    try {
      val _ = TimeFormat.parse(Some("foo"))
      fail("should have failed")
    } catch {
      case e: Exception =>
        assert(e.getMessage.contains("foo"))
    }

  }

}
