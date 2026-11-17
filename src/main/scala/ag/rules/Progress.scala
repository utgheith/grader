package ag.rules

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

// What a State's targets are doing, for the status screen (see
// StatusScreen): the ones in flight, each in its phase since when, and how
// the finished ones went.
//
//   Checking  started: reading what it depends on, or deciding whether its
//             saved result is still good
//   Queued    computing, but waiting its turn (see Producer.waiting)
//   Running   computing
//
// A finished target was reused (it never ran), ran, or failed.
object Progress {
  enum Phase {
    case Checking, Queued, Running
  }

  final case class InFlight(
      path: os.RelPath,
      display: String,
      phase: Phase,
      since: Long
  )

  final case class Snapshot(
      started: Long,
      reused: Long,
      ran: Long,
      failed: Long,
      in_flight: Seq[InFlight]
  ) {
    def count(phase: Phase): Int = in_flight.count(_.phase == phase)
  }
}

final class Progress {
  import Progress.{InFlight, Phase, Snapshot}

  private val active = ConcurrentHashMap[os.RelPath, InFlight]()
  private val started_ = AtomicLong(0)
  private val reused_ = AtomicLong(0)
  private val ran_ = AtomicLong(0)
  private val failed_ = AtomicLong(0)

  // (keeping what it's shown as, if it said; see describe)
  private def set(t: TargetBase, phase: Phase): Unit = {
    val shown = Option(active.get(t.path)).fold(t.display)(_.display)
    val _ = active.put(
      t.path,
      InFlight(t.path, shown, phase, System.currentTimeMillis())
    )
  }

  // what it's shown as from now on, while it's in flight
  private[rules] def describe(t: TargetBase, display: String): Unit = {
    val _ = active.computeIfPresent(t.path, (_, f) => f.copy(display = display))
  }

  private[rules] def started(t: TargetBase): Unit = {
    val _ = started_.incrementAndGet()
    set(t, Phase.Checking)
  }

  private[rules] def running(t: TargetBase): Unit = set(t, Phase.Running)
  private[rules] def queued(t: TargetBase): Unit = set(t, Phase.Queued)

  // it ran if it got as far as running, else it was reused
  private[rules] def finished(t: TargetBase): Unit =
    Option(active.remove(t.path)) match {
      case Some(f) if f.phase != Phase.Checking =>
        val _ = ran_.incrementAndGet()
      case _ =>
        val _ = reused_.incrementAndGet()
    }

  private[rules] def failed(t: TargetBase): Unit = {
    val _ = active.remove(t.path)
    val _ = failed_.incrementAndGet()
  }

  def snapshot: Snapshot = Snapshot(
    started_.get,
    reused_.get,
    ran_.get,
    failed_.get,
    active.values.asScala.toSeq
  )
}
