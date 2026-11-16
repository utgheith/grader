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
