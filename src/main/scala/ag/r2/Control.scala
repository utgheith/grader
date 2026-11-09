package ag.r2

import upickle.default.ReadWriter

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

// Lets someone watching a run (see WebServer) hold targets at gates: when a
// target starts (Start), when its function is about to run (Run), and when it
// fails (Failed). Held targets wait, and so does whatever needs them; the rest
// of the run goes on until it needs them too.
//
// The run itself can be held at its end (End), before it exits.
//
// A target is held at any gate while the run is paused, and at Run if it (or
// its kind) has a breakpoint, or at Failed if failures break. A breakpoint or
// failure also pauses the run, so that other targets stop at their next gate.
// Until `enable` is called, gates are free.
private[r2] final class Control(changed: () => Unit) {
  import Control.*

  @volatile private var enabled = false
  private var paused = false
  private var break_on_failure = false
  private val breakpoints = mutable.SortedSet[String]()
  private val waiting = mutable.LinkedHashMap[Long, Waiter]()
  private val ids = new AtomicLong(0)
  // failures that already broke: one passes through every target that
  // depends on the target that failed, and it breaks only the first time
  private val broke_on = java.util.Collections.newSetFromMap(
    new java.util.IdentityHashMap[Throwable, java.lang.Boolean]()
  )

  def enable(): Unit = enabled = true

  def checkpoint(
      target: TargetBase,
      gate: Gate,
      error: Option[Throwable] = None
  ): Unit = if (enabled) {
    val w = synchronized {
      val hit = gate match {
        case Gate.Start => false
        case Gate.Run =>
          breakpoints.contains(target.path.toString) || breakpoints
            .contains(target.kind.toString)
        case Gate.Failed =>
          break_on_failure && error.exists(e => broke_on.add(e))
        case Gate.End => true
      }
      if (hit) paused = true
      Option.when(paused)(waiter(target.path.toString, gate, hit))
    }
    w.foreach(wait_for)
  }

  // Holds the run before it exits, until it's released or resumed.
  def hold_at_end(): Unit = if (enabled) {
    val w = synchronized {
      paused = true
      waiter(RunGraph.root, Gate.End, hit = true)
    }
    wait_for(w)
  }

  // call holding the lock
  private def waiter(path: String, gate: Gate, hit: Boolean): Waiter = {
    val w = Waiter(ids.incrementAndGet(), path, gate.toString, hit)
    waiting.update(w.id, w)
    w
  }

  private def wait_for(w: Waiter): Unit = {
    changed()
    synchronized {
      while (!w.released) wait()
      val _ = waiting.remove(w.id)
    }
    changed()
  }

  private def update(f: => Unit): Unit = {
    synchronized {
      f
      notifyAll()
    }
    changed()
  }

  def pause(): Unit = update { paused = true }

  // releases every held target, and lets the rest run freely
  def resume(): Unit = update {
    paused = false
    waiting.values.foreach(_.released = true)
  }

  // releases one held target; the run stays paused
  def release(id: Long): Unit = update {
    waiting.get(id).foreach(_.released = true)
  }

  // `at` is a target's path or a kind (every target it makes)
  def set_breakpoint(at: String, on: Boolean): Unit = update {
    val _ = if (on) breakpoints.add(at) else breakpoints.remove(at)
  }

  def set_break_on_failure(on: Boolean): Unit = update {
    break_on_failure = on
  }

  def status: Status = synchronized {
    Status(
      paused = paused,
      break_on_failure = break_on_failure,
      breakpoints = breakpoints.toSeq,
      waiting = waiting.values.toSeq.map(w =>
        Held(w.id, w.path, w.gate, w.hit)
      )
    )
  }
}

private[r2] object Control {
  enum Gate {
    case Start, Run, Failed, End
  }

  // a held target; `hit`: held by a breakpoint or failure, not just a pause
  final case class Waiter(id: Long, path: String, gate: String, hit: Boolean) {
    @volatile var released: Boolean = false
  }

  case class Held(id: Long, path: String, gate: String, hit: Boolean)
      derives ReadWriter

  case class Status(
      paused: Boolean,
      break_on_failure: Boolean,
      breakpoints: Seq[String],
      waiting: Seq[Held]
  ) derives ReadWriter
}
