package ag.r2

import munit.FunSuite

import upickle.default.ReadWriter

class ScopeTests extends FunSuite {
  test("scope base path") {
    val s1 = Scope()
    assert(clue(s1.base) == clue(os.RelPath(".")))

    val s2 = Scope("foo")
    assert(clue(s2.base) == clue(os.RelPath("foo")))

    val s3 = s2 / "bar"
    assert(clue(s3.base) == clue(os.RelPath("foo/bar")))
  }

  test("target creation") {
    given State = State(os.temp.dir(deleteOnExit = true))
    val s = Scope("test")

    case class Data(i: Int) derives ReadWriter

    val t1 = s.target[Data]() {
      Data(1)
    }

    assert(clue(t1.path) == clue(os.RelPath("ag/r2/ScopeTests/t1/test")))
    assert(clue(t1.guilty) == clue(Data(1)))
  }

  test("fun method") {
    val s = Scope("fun")

    case class Data(i: Int) derives ReadWriter

    val f = s.fun { (i: Int) =>
      s.target[Data]() {
        Data(i)
      }
    }

    val t = f(10)
    // Path should contain "10" at the end
    assert(clue(t.path.last) == clue("10"))
  }
}
