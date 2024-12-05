package ag.grader

import Project.TimeFormat

class TimeFormatTests extends munit.FunSuite {

  test("t1") {
    "10:20:30.40" match {
        case TimeFormat(h,m,s) =>
            assert(clue(h) == "10")
            assert(clue(m) == "20")
            assert(clue(s) == "30.40")
    }
    "20:30.40" match {
        case TimeFormat(h,m,s) =>
            println(s"***$h***")
            assert(clue(m) == "20")
            assert(clue(s) == "30.40")
    }
  }

}
