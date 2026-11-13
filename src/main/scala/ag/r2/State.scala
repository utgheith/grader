package ag.r2

import ag.common.Task

import os.Path

import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal

// `force_roots`: rebuild every target the program tracks directly (its
// "roots", tracked at depth 0), even if its saved result is current. Their
// inputs are reused as usual.
//
// `read_only`: its targets may update the workspace and read from servers,
// but nothing else: whatever would act outside it (see
// ag.rules.read_only) throws instead, failing the target that tried.
class State(
    val workspace: os.Path,
    val force_roots: Boolean = false,
    val read_only: Boolean = false
) extends Tracker {

  // Context[Nothing] methods
  override val depth: Int = 0
  override val state: State = this
  override def producing_opt: Option[Target[Nothing]] = None

  override val route = Seq()

  def target_path(target: TargetBase | os.RelPath): os.Path =
    workspace / "targets" / (target match {
      case t: TargetBase => t.path
      case p: os.RelPath => p
    })
  def data_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "data"
  def saved_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "saved.json"
  def dirty_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "dirty"

  // Makes `target` rebuild the next time it's used, as if its last run had
  // failed: its saved result and data are discarded.
  def force(target: TargetBase): Unit =
    os.write.over(dirty_path(target), "", createFolders = true)
  def log_path(target: TargetBase | os.RelPath): os.Path =
    target_path(target) / "log.txt"

  private val cache = mutable.Map[os.RelPath, Task[Result[?]]]()

  // counts changes to `graph` and `control`, for a WebServer to notice them
  private[r2] val version = new java.util.concurrent.atomic.AtomicLong(0)

  // what this run did, for write_graph
  private[r2] val graph: RunGraph = RunGraph(() => {
    val _ = version.incrementAndGet()
  })

  // where a WebServer can hold targets
  private[r2] val control: Control = Control(() => {
    val _ = version.incrementAndGet()
  })

  // Where write_graph leaves the graph's pages: open index.html.
  lazy val graph_dir: os.Path = workspace / "graph"

  // Writes the graph of the targets this run used, their dependencies and the
  // commands that computed them, replacing the last run's.
  def write_graph(): Unit = graph.write(this)

  // Starts computing `target`, unless it already has been, and makes it a
  // dependency of `tracker`'s target. Returns the computation's result and
  // what the caller must run: with `here`, the target is computed by calling
  // that on the caller's thread; otherwise on a thread of its own.
  private[r2] def start[A](
      target: Target[A],
      tracker: Tracker[?],
      here: Boolean
  ): (Task[Result[?]], () => Unit) = {
    tracker.check_phase(tracker.Phase.Open)
    if (tracker.route.map(_.path).contains(target.path)) {
      throw new Exception(
        s"Circular dependency detected: ${(tracker.route.map(_.path) :+ target.path).map(_.toString).mkString(" -> ")}"
      )
    }
    graph.used(target, tracker.producing_opt)
    var run: () => Unit = () => ()
    val result: Task[Result[?]] = cache.synchronized {
      cache.getOrElseUpdate(
        target.path, {
          // first use of this target in this run
          if (force_roots && tracker.depth == 0) force(target)
          def compute(): Result[?] = {
            Context.say(
              Some(tracker),
              s"miss for ${target.path.toString} in ${this.toString}"
            )
            control.checkpoint(target, Control.Gate.Start)
            graph.making(target)
            try {
              // the commands that run on this thread now are the target's,
              // until it tracks another, which computes on this thread
              val r = ag.rules.on_command.set(c =>
                graph.command(Some(target.path), c)
              ) {
                ag.rules.read_only.set(read_only)(
                  target.make(
                    new Tracker {
                      override val depth: Int = tracker.depth + 1

                      override val route = tracker.route :+ target
                      override val state: State = tracker.state

                      override def producing_opt: Option[Target[A]] =
                        Some(target)
                    }
                  )
                )
              }
              graph.made(target, None)
              r
            } catch {
              case NonFatal(e) =>
                graph.made(target, Some(e))
                control.checkpoint(target, Control.Gate.Failed, Some(e))
                throw e
            }
          }
          if (here) {
            val (task, complete) = Task.promise[Result[?]]()
            run = () => complete(Try(compute()))
            task
          } else {
            Task(compute())
          }
        }
      )
    }

    tracker.add_dependency(target, result)
    (result, run)
  }

}

object State {
  // Runs `f` with a new state in workspace `path`, then writes the run's graph
  // (see State.write_graph), whether or not it fails.
  def run[A](path: os.Path, force_roots: Boolean = false)(
      f: State => A
  ): A = {
    val state = new State(path, force_roots)
    try
      ag.rules.on_command.set(c => state.graph.command(None, c))(f(state))
    finally {
      try {
        state.write_graph()
        println(s"graph: ${(state.graph_dir / "index.html").toString}")
      } catch {
        case NonFatal(e) => println(s"writing the graph failed: ${e.toString}")
      }
    }
  }
}
