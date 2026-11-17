package ag.rules

import ag.common.Task
import org.eclipse.jgit.lib.ObjectId

import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal

// One run of targets in `workspace`, which keeps them in
// a git repo (see Store):
//
//   r3.git/       the targets: their values, data and history
//   checkouts/    read-only checkouts of data trees (see Context.checkout)
//   scratch/      where create_data and update_data's targets write
//   forced/       targets to run again even if they're current (see force)
//   logs/         what each target logged, last run
//
// With `force_roots`, the targets it tracks itself run again even if
// they're current (their inputs are reused). With `read_only`, its targets
// may update the workspace but refuse to act beyond it (see
// refuse_if_read_only).
class State(
    val workspace: os.Path,
    val force_roots: Boolean = false,
    val read_only: Boolean = false
) extends Tracker[Nothing] {

  override val depth: Int = 0
  override val state: State = this
  override def producing_opt: Option[Target[Nothing]] = None
  override val route: Seq[Target[?]] = Seq()

  lazy val store: Store = Store(workspace / "r3.git")

  // what its targets are doing (see StatusScreen)
  val progress: Progress = Progress()

  // what this run's commands went through (see Target.tally)
  val tally: ag.common.Tally = ag.common.Tally()

  def forced_path(t: TargetBase): os.Path =
    workspace / "forced" / Store.rel_path(t.stored)
  def log_path(t: TargetBase): os.Path =
    workspace / "logs" / Store.rel_path(t.stored) / "log.txt"
  lazy val scratch_root: os.Path = workspace / "scratch"
  lazy val checkouts: os.Path = workspace / "checkouts"

  // Makes `target` run the next time it's used, even if it's current.
  def force(target: TargetBase): Unit =
    os.write.over(forced_path(target), "", createFolders = true)

  private[rules] def checkout_tree(tree: Tree): os.Path = {
    os.makeDir.all(checkouts)
    store_at(tree.at).checkout(ObjectId.fromString(tree.id).nn, checkouts)
  }

  // the store a Tree or Commit is in, to read: the workspace's, or a Source
  // repo, opened once
  private val sources =
    scala.collection.concurrent.TrieMap[String, Store.Reads]()
  private[rules] def store_at(at: Option[String]): Store.Reads = at match {
    case None     => store
    case Some(at) =>
      sources.getOrElseUpdate(at, Store(os.Path(at), create = false))
  }

  private val cache = mutable.Map[os.RelPath, Task[Result[?]]]()
  // where each target used is saved (see TargetBase.stored)
  private val stored_at =
    scala.collection.concurrent.TrieMap[os.RelPath, os.RelPath]()

  // Runs `f` as the only one working on what's saved at `stored`: the
  // targets saved there take turns (see TargetBase.stored).
  private val turns =
    scala.collection.concurrent
      .TrieMap[os.RelPath, java.util.concurrent.Semaphore]()
  private[rules] def in_turn[B](stored: os.RelPath)(f: => B): B = {
    val s = turns.getOrElseUpdate(stored, java.util.concurrent.Semaphore(1))
    s.acquire()
    try f
    finally s.release()
  }

  // For a person to look at: every target this run used, by its path, as a
  // link to a checkout of what's stored for it (value.json, and data/ if
  // it has data): <dir>/<path>/@, as the store names it (so a target and
  // those under its path can both be there). What was there goes. Says
  // how many.
  def link_used(dir: os.Path): Int = {
    os.remove.all(dir)
    val paths = cache
      .synchronized(cache.keys.toSeq)
      .map(p => stored_at.getOrElse(p, p))
      .distinct
    (for {
      path <- paths
      loaded <- store.load(path)
    } yield {
      os.makeDir.all(checkouts)
      val target = store.checkout(loaded.result, checkouts)
      val link = dir / Store.rel_path(path)
      os.makeDir.all(link / os.up)
      os.symlink(link, target.relativeTo(link / os.up))
    }).size
  }

  private[rules] def start[A](
      target: Target[A],
      tracker: Tracker[?],
      here: Boolean
  ): (Task[Result[?]], () => Unit) = {
    tracker.check_open()
    if (tracker.route.map(_.path).contains(target.path))
      throw new Exception(
        s"Circular dependency detected: ${(tracker.route.map(_.path) :+ target.path).map(_.toString).mkString(" -> ")}"
      )
    val _ = stored_at.put(target.path, target.stored)
    var run: () => Unit = () => ()
    val result: Task[Result[?]] = cache.synchronized {
      cache.getOrElseUpdate(
        target.path, {
          if (force_roots && tracker.depth == 0) force(target)
          def compute(): Result[?] = {
            progress.started(target)
            try {
              val r = ag.rules.read_only.set(read_only)(
                target.make(new Tracker[A] {
                  override val depth: Int = tracker.depth + 1
                  override val route: Seq[Target[?]] = tracker.route :+ target
                  override val state: State = tracker.state
                  override def producing_opt: Option[Target[A]] = Some(target)
                })
              )
              progress.finished(target)
              r
            } catch {
              case e: Throwable =>
                progress.failed(target)
                throw e
            }
          }
          if (here) {
            val (task, complete) = Task.promise[Result[?]]()
            run = () => complete(Try(compute()))
            task
          } else Task(compute())
        }
      )
    }
    tracker.add_dependency(target, result)
    (result, run)
  }
}

object State {
  // Runs `f` with a new State in `path`; with `browse`, links what it used
  // under path/browse when it's done, however it ends (see link_used).
  def run[A](
      path: os.Path,
      force_roots: Boolean = false,
      browse: Boolean = false
  )(
      f: State => A
  ): A = {
    val state = new State(path, force_roots)
    try f(state)
    finally
      if (browse) {
        val dir = path / "browse"
        println(
          s"linked ${state.link_used(dir).toString} targets under ${dir.toString}"
        )
      }
  }
}

// a scratch directory, removed afterwards whatever happens
private[rules] def with_scratch[A](state: State)(f: os.Path => A): A = {
  os.makeDir.all(state.scratch_root)
  val dir = os.temp.dir(dir = state.scratch_root, deleteOnExit = false)
  try f(dir)
  finally
    try os.remove.all(dir)
    catch { case NonFatal(_) => () }
}
