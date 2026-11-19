package ag.grader.targets

import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicLong

// How many test runs go at once: a core each (a test may ask for more), all
// but one of this machine's
val limit = Runtime.getRuntime.nn.availableProcessors() - 1
val governor = new Semaphore(limit)

private[grader] val started_runs = AtomicLong(0)
// how many test runs this process has started
def runs_started(): Long = started_runs.get
private[grader] val finished_runs = AtomicLong(0)

// Set when the round is to end early (a push failed: see main.misc.Publishing):
// test runs not yet started don't start (their targets fail, and nothing is
// saved for them), and the ones going finish. The first reason is kept.
private val stopping =
  java.util.concurrent.atomic.AtomicReference[Option[Throwable]](None)

def stop(why: Throwable): Unit = {
  val _ = stopping.compareAndSet(None, Some(why))
}

final class Stopped(why: Throwable)
    extends Exception(s"not started: the round is ending (${why.toString})")

// throws if the round is ending (see stop)
def stop_check(): Unit = stopping.get.foreach(why => throw Stopped(why))
