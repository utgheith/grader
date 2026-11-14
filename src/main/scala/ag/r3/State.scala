package ag.r3

import ag.common.Task
import org.eclipse.jgit.lib.ObjectId

import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal

// One run of targets in `workspace` (as an ag.r2.State), which keeps them in
// a git repo (see Store):
//
//   r3.git/       the targets: their values, data and history
//   checkouts/    read-only checkouts of data trees (see Context.checkout)
//   scratch/      where create_data and update_data's targets write
//   forced/       targets to run again even if they're current (see force)
//   logs/         what each target logged, last run
//
// `force_roots` and `read_only` are as in ag.r2.State.
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

  // what this run's commands went through (see Target.tally)
  val tally: ag.common.Tally = ag.common.Tally()

  def forced_path(t: TargetBase): os.Path =
    workspace / "forced" / Store.rel_path(t.path)
  def log_path(t: TargetBase): os.Path =
    workspace / "logs" / Store.rel_path(t.path) / "log.txt"
  lazy val scratch_root: os.Path = workspace / "scratch"
  lazy val checkouts: os.Path = workspace / "checkouts"

  // Makes `target` run the next time it's used, even if it's current.
  def force(target: TargetBase): Unit =
    os.write.over(forced_path(target), "", createFolders = true)

  private[r3] def checkout_tree(tree: Tree): os.Path = {
    os.makeDir.all(checkouts)
    store_at(tree.at).checkout(ObjectId.fromString(tree.id).nn, checkouts)
  }

  // the store a Tree or Commit is in: the workspace's, or a Source repo,
  // opened once and only read
  private val sources = scala.collection.concurrent.TrieMap[String, Store]()
  private[r3] def store_at(at: Option[String]): Store = at match {
    case None     => store
    case Some(at) =>
      sources.getOrElseUpdate(at, Store(os.Path(at), create = false))
  }

  private val cache = mutable.Map[os.RelPath, Task[Result[?]]]()

  // For a person to look at: every target this run used, by its path, as a
  // link to a checkout of what's stored for it (value.json, and data/ if
  // it has data): <dir>/<path>/@, as the store names it (so a target and
  // those under its path can both be there). What was there goes. Says
  // how many.
  def link_used(dir: os.Path): Int = {
    os.remove.all(dir)
    val paths = cache.synchronized(cache.keys.toSeq)
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

  private[r3] def start[A](
      target: Target[A],
      tracker: Tracker[?],
      here: Boolean
  ): (Task[Result[?]], () => Unit) = {
    tracker.check_open()
    if (tracker.route.map(_.path).contains(target.path))
      throw new Exception(
        s"Circular dependency detected: ${(tracker.route.map(_.path) :+ target.path).map(_.toString).mkString(" -> ")}"
      )
    var run: () => Unit = () => ()
    val result: Task[Result[?]] = cache.synchronized {
      cache.getOrElseUpdate(
        target.path, {
          if (force_roots && tracker.depth == 0) force(target)
          def compute(): Result[?] =
            ag.rules.read_only.set(read_only)(
              target.make(new Tracker[A] {
                override val depth: Int = tracker.depth + 1
                override val route: Seq[Target[?]] = tracker.route :+ target
                override val state: State = tracker.state
                override def producing_opt: Option[Target[A]] = Some(target)
              })
            )
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
private[r3] def with_scratch[A](state: State)(f: os.Path => A): A = {
  os.makeDir.all(state.scratch_root)
  val dir = os.temp.dir(dir = state.scratch_root, deleteOnExit = false)
  try f(dir)
  finally
    try os.remove.all(dir)
    catch { case NonFatal(_) => () }
}
