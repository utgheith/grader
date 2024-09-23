package ag.rules

import java.util.concurrent.CountDownLatch

class TaskTests extends munit.FunSuite {

  test("tuple sequence") {
    val a = Task.success(10)
    val b = Task.success("20")
    val c = Task.sequence((a, b))
    assert(c.ready)
    assert(clue(c.block) == (10, "20"))

    val latch = new CountDownLatch(1)
    val p = Task {
      latch.await()
      10
    }

    val q = Task.success(42)
    val r = Task.sequence((p, q))
    assert(clue(!p.ready))
    assert(clue(q.ready))
    assert(clue(!r.ready))
    latch.countDown()
    assert(clue(r.block) == (10, 42))
    assert(p.ready)
    assert(q.ready)
    assert(r.ready)
  }

}
