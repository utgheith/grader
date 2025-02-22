package ag.r2

import ag.common.{block, given_VirtualExecutionContext}
import scala.concurrent.Future


val a: Target[Int] = target[Int] {
  run_if_needed {
    Future.successful(10)
  }
}

val b: (String, Int) => Target[Int] = target[String, Int, Int] { (s, i) =>
    println(s"tracking $s $i")
    val ta = a.track
    run_if_needed {
      println(s"running $s $i")
      ta.map(_ + i)
    }
}

class TargetTests extends munit.FunSuite {
  test("basic tracking") {
    val dir = os.temp.dir(deleteOnExit=true)
    def doit(d: Int): Unit = {
      val tb: Future[Int] = State(dir) {
        for {
          a <- a.track
          _ = assertEquals(a, 10)
          b <- b("thing", d).track
          _ = assertEquals(b, 10 + d)
        } yield (a + b)
      }
      assertEquals(tb.block, 20 + d)
    }
    doit(6)
    doit(4)
    doit(6)
  }
}
