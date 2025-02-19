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
  test("a") {
    val state = State(os.temp.dir())
    val tb = state {
      b.track
    }
    assertEquals(tb.block, 11)
    println(state.targets)
  }
}
