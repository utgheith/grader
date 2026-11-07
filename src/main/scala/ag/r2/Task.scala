package ag.r2

import java.util.concurrent.CountDownLatch
import scala.util.{Failure, Success, Try}

// The engine's in-flight computations: a value that some thread is working on,
// which any number of readers can wait for. Nothing outside ag.r2 sees one:
// targets read each other with Tracker.track and Tracker.track_all.
private[r2] final class Task[A] private () {
  private val latch = new CountDownLatch(1)
  @volatile private var outcome: Try[A] | Null = null

  // `outcome` is written before the latch opens, so by the time `block_try`
  // observes the latch as counted down, the write is guaranteed visible.
  private def complete(t: Try[A]): Unit = {
    outcome = t
    latch.countDown()
  }

  def block_try: Try[A] = {
    latch.await()
    outcome.nn
  }

  def block: A = block_try.get
}

private[r2] object Task {
  private val builder: Thread.Builder.OfVirtual =
    Thread.ofVirtual().name("task-", 0)

  // Starts running `body` now, on its own virtual thread.
  def apply[A](body: => A): Task[A] = {
    val t = new Task[A]()
    val _ = builder.start(() => {
      val result =
        try Success(body)
        catch { case scala.util.control.NonFatal(e) => Failure(e) }
      t.complete(result)
    })
    t
  }

  // A Task that the caller completes, on whatever thread it likes.
  def promise[A](): (Task[A], Try[A] => Unit) = {
    val t = new Task[A]()
    (t, outcome => t.complete(outcome))
  }
}
