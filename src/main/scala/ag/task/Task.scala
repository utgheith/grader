package ag.task

import language.experimental.captureChecking

import java.util.concurrent.CountDownLatch
import scala.util.{Failure, Success, Try}

// A covariant, eager, future-like value: `body` starts running immediately,
// on its own virtual thread, as soon as the Task is created -- there's no
// ExecutionContext to thread through call sites.
//
// `map`/`flatMap` are implemented by blocking a (fresh, cheap) virtual
// thread on the source Task rather than registering a callback: the
// continuation always runs synchronously within a single thread's body, so
// there's no hidden asynchronous escape hatch the way there is with
// scala.concurrent.Future's flatMap.
//
// map/flatMap are capture-polymorphic (`Task[B]^{f}`): the result is
// declared to capture whatever `f` captures, so a capability smuggled into
// `f` and used inside the continuation it returns is flagged by capture
// checking as escaping this Task's scope, instead of silently compiling.
final class Task[+A] private () {
  private val latch = new CountDownLatch(1)
  @volatile private var outcome: Try[A] | Null = null

  // `outcome` is written before the latch opens, so by the time `block_try`
  // observes the latch as counted down, the write is guaranteed visible.
  private def complete(t: Try[Any]): Unit = {
    outcome = t.asInstanceOf[Try[A]]
    latch.countDown()
  }

  def block_try: Try[A] = {
    latch.await()
    outcome.nn
  }

  def block: A = block_try.get

  def map[B](f: A => B): Task[B]^{f} = Task(f(block))

  def flatMap[B](f: A => Task[B]^): Task[B]^{f} = Task(f(block).block)
}

object Task {
  private val builder: Thread.Builder.OfVirtual =
    Thread.ofVirtual().name("task-", 0)

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

  def successful[A](value: A): Task[A] = {
    val t = new Task[A]()
    t.complete(Success(value))
    t
  }

  def failed[A](cause: Throwable): Task[A] = {
    val t = new Task[A]()
    t.complete(Failure(cause))
    t
  }

  // `ts`'s elements are already running (Tasks are eager) by the time this
  // is called, so blocking on them in order here doesn't serialize the
  // underlying work -- it only serializes collecting results that, in the
  // common case, are already there or arriving concurrently.
  def sequence[A](ts: Seq[Task[A]]): Task[Seq[A]] = Task(ts.map(_.block))
}
