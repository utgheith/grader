package ag.common

import munit.FunSuite

import java.time.Duration
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.util.{Failure, Success, Try}

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

  test("runs on a named virtual thread, not the caller's") {
    val th = Task { Thread.currentThread() }.block
    assertNotEquals(th, Thread.currentThread())
    assert(th.isVirtual)
    assert(clue(th.getName).startsWith("task-"))
  }

  test("the body runs without anyone blocking on the Task") {
    val started = new CountDownLatch(1)
    val _ = Task { started.countDown() }
    assert(started.await(1, TimeUnit.SECONDS))
  }

  test("the body runs exactly once, however often the Task is blocked on") {
    val runs = new AtomicInteger(0)
    val t = Task { runs.incrementAndGet() }
    assertEquals(t.block, 1)
    assertEquals(t.block, 1)
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
  }

  test("block rethrows the body's exception itself, not a wrapper") {
    val boom = new IllegalArgumentException("boom")
    val thrown = intercept[IllegalArgumentException] {
      Task[Int] { throw boom }.block
    }
    assert(thrown eq boom)
  }

  test("a promise is waited for until its holder completes it") {
    val (t, complete) = Task.promise[Int]()
    assertEquals(block_try_within(t, 50), None)
    val blocker = Task { t.block }
    complete(Success(42))
    assertEquals(blocker.block, 42)
    assertEquals(t.block, 42)
  }

  test("a promise can complete with a failure") {
    val boom = new RuntimeException("boom")
    val (t, complete) = Task.promise[Int]()
    complete(Failure(boom))
    assertEquals(t.block_try, Failure(boom))
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
