package ag.r2

import ag.rules.CommandRun
import upickle.default.ReadWriter

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

// What one run did, kept for the dependency graph it leaves in its workspace
// (see `write`): every target it used, how it got its value, what it depends
// on, and the commands that ran to compute it. A WebServer shows it as it
// changes: `changed` is called after each change, and `changes` tells which.
private[r2] final class RunGraph(changed: () => Unit) {
  import RunGraph.*

  private val started_at = System.currentTimeMillis()
  private val nodes = TrieMap[os.RelPath, Node]()
  // nodes changed since `changes` or `snapshot` last looked
  private val dirty = java.util.concurrent.ConcurrentHashMap.newKeySet[Node]()

  // runs `f` on `n`, then records the change
  private def change(n: Node)(f: => Unit): Unit = {
    n.synchronized(f)
    val _ = dirty.add(n)
    changed()
  }

  private def now(): Long = System.currentTimeMillis() - started_at

  private def node(target: TargetBase): Node =
    nodes.get(target.path).getOrElse {
      val n = Node(target.path, target.kind)
      nodes.putIfAbsent(target.path, n) match {
        case Some(first) => first
        case None =>
          val _ = dirty.add(n)
          changed()
          n
      }
    }

  private def node_at(path: os.RelPath): Option[Node] = nodes.get(path)

  // `by` (the run itself, for None) depends on `target`
  def used(target: TargetBase, by: Option[TargetBase]): Unit = {
    val _ = node(target)
    val n = by.map(node).getOrElse(root_node)
    val known = n.synchronized(n.deps.get(target.path.toString))
    // a real dependency outranks a peek at the same target
    val peek = target.is_peek && known.getOrElse(true)
    if (!known.contains(peek)) change(n) {
      n.deps.update(target.path.toString, peek)
    }
  }

  def making(target: TargetBase): Unit = {
    val n = node(target)
    change(n) { n.start = Some(now()) }
  }

  def made(target: TargetBase, error: Option[Throwable]): Unit = {
    val n = node(target)
    change(n) {
      n.end = Some(now())
      error.foreach { e =>
        n.outcome = "failed"
        n.error = Some(e.toString)
      }
    }
  }

  // how the target gets its value: "reused" or "ran", and why it ran
  def decided(
      path: os.RelPath,
      outcome: String,
      reason: Option[String],
      changed: Seq[os.RelPath]
  ): Unit = node_at(path).foreach { n =>
    change(n) {
      n.outcome = outcome
      n.reason = reason
      n.changed = changed.map(_.toString)
    }
  }

  // the target's own function starts or ends
  def running(path: os.RelPath): Unit = node_at(path).foreach { n =>
    change(n) { n.run_start = Some(now()) }
  }
  def ran(path: os.RelPath): Unit = node_at(path).foreach { n =>
    change(n) { n.run_end = Some(now()) }
  }

  def command(path: Option[os.RelPath], c: CommandRun): Unit = {
    val n = path.flatMap(node_at).getOrElse(root_node)
    change(n) { val _ = n.commands.append(c) }
  }

  private lazy val root_node: Node = Node(os.RelPath(root), os.RelPath(root))

  // Links to files in `state`'s workspace are relative to state.graph_dir,
  // where `write` leaves the page, so they stay valid if the workspace moves.
  // A WebServer serves the same files at the same paths.
  private def out(n: Node, state: State): NodeOut = {
    val dir = state.graph_dir
    def link(p: os.Path): Option[String] =
      Option.when(os.exists(p))(p.relativeTo(dir).toString)
    def nonempty(p: os.Path): Option[String] =
      Option.when(os.isFile(p) && os.size(p) > 0)(p.relativeTo(dir).toString)

    n.synchronized {
      val is_root = n eq root_node
      NodeOut(
        path = n.path.toString,
        kind = n.kind.toString,
        outcome = n.outcome,
        reason = n.reason,
        changed = n.changed,
        error = n.error,
        start = n.start,
        end = n.end,
        run_start = n.run_start,
        run_end = n.run_end,
        deps = n.deps.toSeq,
        commands = n.commands.toSeq.map { c =>
          def file(ext: String): Option[String] = c.out_prefix.flatMap { p =>
            nonempty(p / os.up / s"${p.last}.$ext")
          }
          CommandOut(
            id = c.id,
            command = c.command,
            cwd = c.cwd.toString,
            rc = c.rc,
            start = c.start_ms - started_at,
            ms = c.duration_ms,
            out = file("out"),
            err = file("err")
          )
        },
        log = Option.unless(is_root)(state.log_path(n.path)).flatMap(nonempty),
        saved = Option.unless(is_root)(state.saved_path(n.path)).flatMap(link),
        data = Option.unless(is_root)(state.data_path(n.path)).flatMap(link)
      )
    }

  }

  // everything, as of now; `done`: the run is over. What changed stays
  // in `changes`, for whoever already has an older snapshot.
  def snapshot(state: State, done: Boolean): GraphOut = {
    GraphOut(
      started = started_at,
      ms = now(),
      done = done,
      workspace = state.workspace.toString,
      root = root,
      nodes = (root_node +: nodes.values.toSeq.sortBy(_.path.toString))
        .map(out(_, state))
    )
  }

  // the nodes that changed since the last snapshot or changes
  def changes(state: State): Seq[NodeOut] = {
    val it = dirty.iterator().nn
    val ns = mutable.ArrayBuffer[Node]()
    while (it.hasNext) {
      ns.append(it.next().nn)
      it.remove()
    }
    ns.toSeq.map(out(_, state))
  }

  def elapsed: Long = now()

  // Writes the graph's viewer to state.graph_dir: index.html, which shows
  // data.js.
  def write(state: State): Unit = {
    val dir = state.graph_dir
    val graph = snapshot(state, done = true)
    os.makeDir.all(dir)
    os.write.over(dir / "index.html", page)
    os.write.over(
      dir / "data.js",
      s"const GRAPH = ${upickle.default.write(graph)};\n"
    )
  }
}

private[r2] object RunGraph {
  // the run itself, which depends on what it tracks
  val root = "(run)"

  final class Node(val path: os.RelPath, val kind: os.RelPath) {
    // "used": the run needed it but it hasn't decided how to get its value
    var outcome: String = "used"
    var reason: Option[String] = None
    var changed: Seq[String] = Seq()
    var error: Option[String] = None
    var start: Option[Long] = None
    var end: Option[Long] = None
    var run_start: Option[Long] = None
    var run_end: Option[Long] = None
    // what it depends on, and whether it only peeks at it
    val deps: mutable.LinkedHashMap[String, Boolean] = mutable.LinkedHashMap()
    val commands: mutable.ArrayBuffer[CommandRun] = mutable.ArrayBuffer()
  }

  case class CommandOut(
      id: Option[Long],
      command: Seq[String],
      cwd: String,
      rc: Option[Int],
      start: Long,
      ms: Long,
      out: Option[String],
      err: Option[String]
  ) derives ReadWriter

  case class NodeOut(
      path: String,
      kind: String,
      outcome: String,
      reason: Option[String],
      changed: Seq[String],
      error: Option[String],
      start: Option[Long],
      end: Option[Long],
      run_start: Option[Long],
      run_end: Option[Long],
      deps: Seq[(String, Boolean)],
      commands: Seq[CommandOut],
      log: Option[String],
      saved: Option[String],
      data: Option[String]
  ) derives ReadWriter

  case class GraphOut(
      started: Long,
      ms: Long,
      done: Boolean,
      workspace: String,
      root: String,
      nodes: Seq[NodeOut]
  ) derives ReadWriter

  // src/main/resources/ag/r2/graph.{html,css,js}, made into one page
  lazy val page: String = load_page()

  private[r2] def load_page(): String = {
    // Through the loader of our own classes, not the thread's context loader
    // (os.resource's default): the server's threads get the system class
    // loader, which can't see our resources when sbt runs us.
    val root = os.ResourceRoot.ClassLoader(classOf[RunGraph].getClassLoader.nn)
    def resource(name: String): String =
      os.read(os.resource(using root) / "ag" / "r2" / name)
    resource("graph.html")
      .replace("/*CSS*/", resource("graph.css"))
      .replace("/*JS*/", resource("graph.js"))
  }
}
