package ag.r2

import ag.common.{block, given_VirtualExecutionContext}
import scala.concurrent.Future


val a = Target(os.RelPath("a")) {
  run_if_needed {
    Future.successful(10)
  }
}

val b = Target(os.RelPath("b")) {
  val ta = a.track
  run_if_needed {
    ta.map(_ + 1)
  }
}

class TargetTests extends munit.FunSuite {
  test("basic tracking") {
    val dir = os.temp.dir(deleteOnExit=true)
    def doit(): Unit = {
      val tb = State(dir) {
        for {
          a <- a.track
          _ = assertEquals(a, 10)
          b <- b.track
          _ = assertEquals(b, 11)
        } yield (a + b)
      }
      assertEquals(tb.block, 21)
    }
    doit()
    doit()
  }
}
