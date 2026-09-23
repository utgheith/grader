package ag.task

import munit.FunSuite

class TaskTests extends FunSuite {
  test("block returns the computed value") {
    val t = Task { 21 + 21 }
    assertEquals(t.block, 42)
  }

  test("runs eagerly, on its own thread") {
    val callerThread = Thread.currentThread()
    val t = Task { Thread.currentThread() }
    assertNotEquals(t.block, callerThread)
  }

  test("successful/failed construct already-completed tasks") {
    assertEquals(Task.successful(1).block, 1)
    intercept[RuntimeException] {
      Task.failed(new RuntimeException("boom")).block
    }
  }

  test("map transforms the value") {
    val t = Task { 10 }.map(_ * 2)
    assertEquals(t.block, 20)
  }

  test("flatMap chains to another Task") {
    val t = Task { 10 }.flatMap(a => Task { a + 5 })
    assertEquals(t.block, 15)
  }

  test("a failure propagates through map/flatMap") {
    val t = Task[Int] { throw new RuntimeException("boom") }
    val _ = intercept[RuntimeException] { t.block }
    val _ = intercept[RuntimeException] { t.map(_ + 1).block }
    val _ = intercept[RuntimeException] {
      t.flatMap(a => Task.successful(a)).block
    }
  }

  test("many tasks run concurrently, not serially") {
    val n = 50
    val tasks = (1 to n).map(_ => Task { Thread.sleep(50); 1 })
    val start = System.nanoTime()
    val total = tasks.map(_.block).sum
    val elapsedMs = (System.nanoTime() - start) / 1000000
    assertEquals(total, n)
    assert(
      clue(elapsedMs) < 500,
      "tasks should have run concurrently, not one after another"
    )
  }
}
