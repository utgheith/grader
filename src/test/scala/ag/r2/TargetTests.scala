package ag.r2

import ag.common.block
import scala.concurrent.Future

object Things extends Scope(".") {
  val a: Target[WithData[Int]] = target() {
        create_data(_ => false) { path =>
          os.write(path / "xyz", "hello\n")
          10
        }
  }

  val b: (String, Int) => Target[Int] = fun { (s, i) =>
    target(a) { a =>
      println(s"running $s $i, a:${a.toString}")
      a.value + i
    }
  }
}

class TargetTests extends munit.FunSuite {
  test("basic tracking") {
    def doit(d: Int): Unit = {
      given State = State(config.get_test_dir)

      val tb: Future[Int] =
        for {
          a <- Things.a.track
          _ = assertEquals(a.value, 10)
          b <- Things.b("thing", d).track
          _ = assertEquals(b, 10 + d)
        } yield (a.value + b)

      assertEquals(tb.block, 20 + d)
    }
    doit(6)
    doit(4)
    doit(6)
  }
}
