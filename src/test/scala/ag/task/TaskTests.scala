package ag.task

import munit.FunSuite

import java.time.Duration
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.util.{Failure, Try}

class TaskTests extends FunSuite {
  // `t.block_try`, or None if it doesn't finish within `ms` -- so a Task
  // that never completes fails the test instead of hanging the suite.
  private def block_try_within[A](
      t: Task[A],
      ms: Long = 1000
  ): Option[Try[A]] = {
    val out = new AtomicReference[Option[Try[A]]](None)
    val th = Thread.ofVirtual().start(() => out.set(Some(t.block_try)))
    val _ = th.join(Duration.ofMillis(ms))
    out.get().nn
  }

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

  test("runs on a named virtual thread") {
    val th = Task { Thread.currentThread() }.block
    assert(th.isVirtual)
    assert(clue(th.getName).startsWith("task-"))
  }

  test("the body runs without anyone blocking on the Task") {
    val started = new CountDownLatch(1)
    val _ = Task { started.countDown() }
    assert(started.await(1, TimeUnit.SECONDS))
  }

  test("the body runs exactly once, however the Task is used") {
    val runs = new AtomicInteger(0)
    val t = Task { runs.incrementAndGet(); 1 }
    val _ = t.block
    val _ = t.block
    val _ = t.map(_ + 1).block
    val _ = t.flatMap(a => Task.successful(a)).block
    assertEquals(runs.get, 1)
  }

  test("many concurrent blockers all see the same value") {
    val gate = new CountDownLatch(1)
    val t = Task { gate.await(); "done" }
    val blockers = (1 to 20).map(_ => Task { t.block })
    gate.countDown()
    assertEquals(blockers.map(_.block).toSet, Set("done"))
  }

  test("block_try returns the body's failure instead of throwing") {
    val boom = new RuntimeException("boom")
    assertEquals(Task[Int] { throw boom }.block_try, Failure(boom))
    assertEquals(Task.failed[Int](boom).block_try, Failure(boom))
  }

  test("block rethrows the body's exception itself, not a wrapper") {
    val boom = new IllegalArgumentException("boom")
    val thrown = intercept[IllegalArgumentException] {
      Task[Int] { throw boom }.block
    }
    assert(thrown eq boom)
  }

  test("an exception thrown by map's f fails the result") {
    val boom = new RuntimeException("boom")
    val t = Task { 1 }.map[Int](_ => throw boom)
    assertEquals(t.block_try, Failure(boom))
  }

  test("an exception thrown by flatMap's f fails the result") {
    val boom = new RuntimeException("boom")
    val t = Task { 1 }.flatMap[Int](_ => throw boom)
    assertEquals(t.block_try, Failure(boom))
  }

  test("flatMap propagates the inner Task's failure") {
    val boom = new RuntimeException("boom")
    val t = Task { 1 }.flatMap(_ => Task[Int] { throw boom })
    assertEquals(t.block_try, Failure(boom))
  }

  test("Task is covariant") {
    val t: Task[Any] = Task.successful("x")
    assertEquals(t.block, "x")
  }

  test("sequence keeps input order, even when later tasks finish first") {
    val ts = (1 to 5).map(i => Task { Thread.sleep((6 - i) * 20L); i })
    assertEquals(Task.sequence(ts).block, Seq(1, 2, 3, 4, 5))
  }

  test("sequence of nothing is an empty Seq") {
    assertEquals(Task.sequence(Seq.empty[Task[Int]]).block, Seq.empty[Int])
  }

  test("sequence fails if any element fails") {
    val boom = new RuntimeException("boom")
    val ts = Seq(Task { 1 }, Task[Int] { throw boom }, Task { 3 })
    assertEquals(Task.sequence(ts).block_try, Failure(boom))
  }

  // KNOWN BUG: Task.apply only catches NonFatal, so a fatal throwable
  // (InterruptedException, StackOverflowError, LinkageError, ...) escapes
  // the body without completing the Task, and every `block` on it hangs
  // forever. Marked `.fail` so the suite passes while the bug exists and
  // munit flags it once it's fixed.
  test(
    "KNOWN BUG: a fatal throwable in the body still completes the Task".fail
  ) {
    val t = Task[Int] { throw new StackOverflowError("deep") }
    assert(clue(block_try_within(t)).exists(_.isFailure))
  }
}
