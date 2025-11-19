package ag.common

import munit.FunSuite
import java.util.concurrent.CountDownLatch
import scala.concurrent.Future

class VirtualExecutionContextTests extends FunSuite {
  test("execute on virtual thread") {
    val vec = new VirtualExecutionContext("test-vt")
    val latch = new CountDownLatch(1)
    var threadName = ""
    var isVirtual = false

    vec.execute(() => {
      val t = Thread.currentThread()
      threadName = t.getName
      isVirtual = t.isVirtual
      latch.countDown()
    })

    latch.await()
    assert(clue(threadName).startsWith("test-vt"))
    assert(clue(isVirtual))
  }

  test("works with futures") {
    given VirtualExecutionContext = VirtualExecutionContext("test-vt")

    val f = Future {
      val t = Thread.currentThread()
      (t.getName, t.isVirtual)
    }

    val (threadName, isVirtual) = f.block

    assert(clue(threadName).startsWith("test-vt"))
    assert(clue(isVirtual))
  }
}
