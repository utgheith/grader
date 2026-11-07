package ag.r2

object Things extends Scope(".") {
  val a: Target[WithData[Int]] = target(EmptyTuple) {(producer, _) => 
    create_data(producer)(_ => false) { path =>
      os.write(path / "xyz", "hello\n")
      10
    }
  }

  val b: (String, Int) => Target[Int] = fun { (s, i) =>
    target((a = a)) { (_, deps) =>
      println(s"running $s $i, a:${deps.a.toString}")
      deps.a.value + i
    }
  }
}

class TargetTests extends munit.FunSuite {
  test("basic tracking") {
    def doit(d: Int): Unit = {
      val root = State(config.get_test_dir)

      val a = root.track(Things.a)
      assertEquals(a.value, 10)
      val b = root.track(Things.b("thing", d))
      assertEquals(b, 10 + d)

      assertEquals(a.value + b, 20 + d)
    }
    doit(6)
    doit(4)
    doit(6)
  }
}
