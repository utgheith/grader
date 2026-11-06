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
    val root = State(os.temp.dir(deleteOnExit = true))
    val s = Scope("test")

    case class Data(i: Int) derives ReadWriter

    val t1 = s.target(EmptyTuple) {(_, _) => 
      Data(1)
    }

    assert(clue(t1.path) == clue(os.RelPath("ag/r2/ScopeTests/t1/test")))
    assertEquals(root.guilty(t1), Data(1))
  }

  test("fun method") {
    val s = Scope("fun")

    case class Data(i: Int) derives ReadWriter

    val f = s.fun { (i: Int) =>
      s.target(EmptyTuple) {(_, _) => 
        Data(i)
      }
    }

    val t = f(10)
    // Path should contain "10" at the end
    assert(clue(t.path.last) == clue("10"))
  }

  test("target takes a named tuple of targets") {
    val root = State(os.temp.dir(deleteOnExit = true))
    val s = Scope("named")
    val none: Target[Int] = s.target(EmptyTuple) { (_, _) => 1 }
    val one: Target[Int] = s.target((a = none)) { (_, deps) => deps.a + 1 }
    val two: Target[String] = s.target((a = none, b = one)) { (_, deps) =>
      s"${deps.a},${deps.b}"
    }
    val pattern: Target[Int] = s.target((a = none, b = one)) {
      case (_, (1, b)) => b
      case (_, (a, _)) => a
    }
    assertEquals(root.guilty(two), "1,2")
    assertEquals(root.guilty(pattern), 2)
  }

  test("target refuses a tuple with anything but targets") {
    assert(
      clue(compileErrors("""
        Scope("named").target((a = 1)) { (_, _) => 1 }
      """)).contains("Cannot prove")
    )
  }
}
