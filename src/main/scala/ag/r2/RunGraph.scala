package ag.r2

import ag.rules.CommandRun
import upickle.default.ReadWriter

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

// What one run did, kept for the dependency graph it leaves in its workspace
// (see `write`): every target it used, how it got its value, what it depends
// on, and the commands that ran to compute it.
private[r2] final class RunGraph {
  import RunGraph.*

  private val started_at = System.currentTimeMillis()
  private val nodes = TrieMap[os.RelPath, Node]()

  private def now(): Long = System.currentTimeMillis() - started_at

  private def node(target: TargetBase): Node =
    nodes.getOrElseUpdate(target.path, Node(target.path, target.kind))

  private def node_at(path: os.RelPath): Option[Node] = nodes.get(path)

  // `by` (the run itself, for None) depends on `target`
  def used(target: TargetBase, by: Option[TargetBase]): Unit = {
    val _ = node(target)
    val n = by.map(node).getOrElse(root_node)
    n.synchronized {
      // a real dependency outranks a peek at the same target
      val peek = target.is_peek && n.deps.getOrElse(target.path.toString, true)
      n.deps.update(target.path.toString, peek)
    }
  }

  def making(target: TargetBase): Unit = {
    val n = node(target)
    n.synchronized { n.start = Some(now()) }
  }

  def made(target: TargetBase, error: Option[Throwable]): Unit = {
    val n = node(target)
    n.synchronized {
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
    n.synchronized {
      n.outcome = outcome
      n.reason = reason
      n.changed = changed.map(_.toString)
    }
  }

  // the target's own function starts or ends
  def running(path: os.RelPath): Unit = node_at(path).foreach { n =>
    n.synchronized { n.run_start = Some(now()) }
  }
  def ran(path: os.RelPath): Unit = node_at(path).foreach { n =>
    n.synchronized { n.run_end = Some(now()) }
  }

  def command(path: Option[os.RelPath], c: CommandRun): Unit = {
    val n = path.flatMap(node_at).getOrElse(root_node)
    n.synchronized { val _ = n.commands.append(c) }
  }

  private lazy val root_node: Node = Node(os.RelPath(root), os.RelPath(root))

  // Writes the graph's viewer to `dir`: index.html, which shows data.js.
  // Links to files in `state`'s workspace are relative, so they stay valid
  // if the workspace moves along with `dir`.
  def write(dir: os.Path, state: State): Unit = {
    val ended = now()
    def link(p: os.Path): Option[String] =
      Option.when(os.exists(p))(p.relativeTo(dir).toString)
    def nonempty(p: os.Path): Option[String] =
      Option.when(os.isFile(p) && os.size(p) > 0)(p.relativeTo(dir).toString)

    def out(n: Node): NodeOut = n.synchronized {
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

    val graph = GraphOut(
      started = started_at,
      ms = ended,
      workspace = state.workspace.toString,
      root = root,
      nodes = (root_node +: nodes.values.toSeq.sortBy(_.path.toString)).map(out)
    )
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
      workspace: String,
      root: String,
      nodes: Seq[NodeOut]
  ) derives ReadWriter

  // src/main/resources/ag/r2/graph.{html,css,js}, made into one page
  private lazy val page: String = {
    def resource(name: String): String =
      os.read(os.resource / "ag" / "r2" / name)
    resource("graph.html")
      .replace("/*CSS*/", resource("graph.css"))
      .replace("/*JS*/", resource("graph.js"))
  }
}
