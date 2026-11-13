package ag.r2

import ag.web.{RunView, WebConfig, WebPages, WebServer}
import ag.web.WebServer.{html, reply}

import com.sun.net.httpserver.HttpExchange

import java.io.{IOException, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{CopyOnWriteArrayList, CountDownLatch}
import scala.util.control.NonFatal

// Shows a run as it goes, at a WebServer's root: the graph's page (see
// RunGraph), updated live, with controls to pause it and hold targets (see
// Control), and the workspace's targets and command output.
final class LiveRun private (state: State) extends RunView {
  import LiveRun.*

  @volatile private var done = false
  private val clients = new CopyOnWriteArrayList[Client]()
  private val broadcast_lock = new Object

  private final class Client(out: OutputStream) {
    val closed = new CountDownLatch(1)
    def send(event: String): Boolean =
      try {
        out.write(event.getBytes(UTF_8))
        out.flush()
        true
      } catch {
        case _: IOException =>
          closed.countDown()
          false
      }
  }

  private def event(kind: String, json: String): String =
    s"event: $kind\ndata: $json\n\n"

  private def status_json: ujson.Value =
    upickle.default.writeJs(state.control.status)

  // what changed since the last broadcast, to every client
  private def broadcast(): Unit = broadcast_lock.synchronized {
    val nodes = state.graph.changes(state)
    val update = ujson.Obj(
      "nodes" -> upickle.default.writeJs(nodes),
      "ms" -> state.graph.elapsed,
      "done" -> done,
      "control" -> status_json
    )
    val e = event("update", ujson.write(update))
    clients.forEach(c => if (!c.send(e)) { val _ = clients.remove(c) })
  }

  private val broadcaster = Thread.ofVirtual().unstarted { () =>
    var seen = -1L
    var last_ping = System.currentTimeMillis()
    try {
      while (!done) {
        Thread.sleep(250)
        val v = state.version.get()
        if (v != seen) {
          seen = v
          broadcast()
        }
        if (System.currentTimeMillis() - last_ping > 15000) {
          last_ping = System.currentTimeMillis()
          clients.forEach(c =>
            if (!c.send(": ping\n\n")) { val _ = clients.remove(c) }
          )
        }
      }
    } catch { case _: InterruptedException => () }
  }

  def start(): Unit = {
    broadcaster.start()
    state.control.enable()
  }

  // tells the clients the run is over
  def stop(): Unit = {
    done = true
    broadcaster.interrupt()
    broadcaster.join(2000)
    try broadcast()
    catch { case NonFatal(_) => () }
    clients.forEach(_.closed.countDown())
  }

  def handle(ex: HttpExchange, method: String, path: String): Boolean =
    (method, path) match {
      case ("GET", "/") =>
        reply(ex, 200, "text/html", RunGraph.page); true
      case ("GET", "/data.js") =>
        reply(ex, 200, "text/javascript", "const GRAPH = null;\n"); true
      case ("GET", "/events")       => events(ex); true
      case ("POST", "/api/control") => control(ex); true
      case ("GET", _)               => file(ex, path); true
      case _                        => false
    }

  // The stream of changes: a snapshot of everything, then updates. The
  // snapshot is taken under the broadcast lock, so no update falls between.
  private def events(ex: HttpExchange): Unit = {
    ex.getResponseHeaders.nn.set("Content-Type", "text/event-stream")
    ex.sendResponseHeaders(200, 0)
    val client = Client(ex.getResponseBody.nn)
    val ok = broadcast_lock.synchronized {
      val snapshot = ujson.Obj(
        "graph" -> upickle.default.writeJs(state.graph.snapshot(state, done)),
        "control" -> status_json
      )
      val sent = client.send(event("snapshot", ujson.write(snapshot)))
      if (sent) { val _ = clients.add(client) }
      sent
    }
    if (ok) client.closed.await()
  }

  // Changes from the page. Only a JSON body is taken, which another site's
  // page can't send without asking (and the cookie is SameSite anyway).
  private def control(ex: HttpExchange): Unit = {
    val json = Option(ex.getRequestHeaders.nn.getFirst("Content-Type"))
      .exists(_.startsWith("application/json"))
    if (!json) reply(ex, 415, "text/plain", "expected json")
    else {
      val body =
        ujson.read(String(ex.getRequestBody.nn.readNBytes(65536), UTF_8))
      val c = state.control
      body("action").str match {
        case "pause"   => c.pause()
        case "resume"  => c.resume()
        case "release" => c.release(body("id").num.toLong)
        case "break"   => c.set_breakpoint(body("at").str, body("on").bool)
        case "break_on_failure" => c.set_break_on_failure(body("on").bool)
        case other => throw IllegalArgumentException(s"unknown action $other")
      }
      reply(ex, 200, "application/json", ujson.write(status_json))
    }
  }

  // The files the page links to: targets' logs, saved values and data, and
  // commands' output. Only under those two directories, and always as text,
  // so nothing in them runs in the page's origin.
  private def file(ex: HttpExchange, path: String): Unit = {
    val rel = scala.util.Try(os.RelPath(path.stripPrefix("/"))).toOption
    val allowed = rel.filter(r =>
      r.ups == 0 && r.segments.headOption.exists(served_dirs.contains)
    )
    val target = allowed.map(state.workspace / _).filter { p =>
      os.exists(p) && {
        val real = p.toNIO.toRealPath()
        served_dirs.exists(d =>
          real.startsWith((state.workspace / d).toNIO.toRealPath())
        )
      }
    }
    target match {
      case Some(p) if os.isDir(p) =>
        val items = os.list(p).map { f =>
          val name = f.last + (if (os.isDir(f)) "/" else "")
          s"""<li><a href="${html(path.stripSuffix("/"))}/${html(name)}">${html(
              name
            )}</a></li>"""
        }
        reply(
          ex,
          200,
          "text/html",
          s"<!doctype html><meta charset=utf-8><title>${html(p.last)}</title><ul>${items.mkString}</ul>"
        )
      case Some(p) =>
        ex.getResponseHeaders.nn
          .set("Content-Type", "text/plain; charset=utf-8")
        ex.sendResponseHeaders(200, os.size(p))
        val out = ex.getResponseBody.nn
        val _ = java.nio.file.Files.copy(p.toNIO, out)
        out.close()
      case None => reply(ex, 404, "text/plain", "not found")
    }
  }
}

object LiveRun {
  private val served_dirs = Set("targets", "std_out_err")

  // Serves `state`'s run while `f` runs, if there's a config, and returns
  // f's value. A server that can't start (e.g. its port is taken by another
  // workspace's run) is reported and skipped: the run goes on without it.
  //
  // `paused`: the run starts paused, so nothing runs until someone resumes it
  // from the page (after setting breakpoints, say). `pause_at_end`: once `f`
  // is done (or failed), the run waits to exit until someone lets it, so its
  // final state can be looked at. Both need the server: without one, there'd
  // be no way to go on, so they're ignored.
  //
  // `pages`, if any, are served too, at /<prefix>/.
  def serving[A](
      state: State,
      config: Option[WebConfig],
      paused: Boolean = false,
      pause_at_end: Boolean = false,
      pages: Option[WebPages] = None
  )(f: => A): A =
    config.flatMap(c =>
      WebServer.start(state.workspace, c, pages, Some(LiveRun(state)))
    ) match {
      case None =>
        if (paused || pause_at_end)
          println(
            "not pausing: that needs this run to be served (see config.json's \"web\")"
          )
        f
      case Some(server) =>
        if (paused) {
          state.control.pause()
          println("paused: resume this run from its page")
        }
        try f
        finally {
          if (pause_at_end) {
            println("waiting to exit: let this run go from its page")
            state.control.hold_at_end()
          }
          server.stop()
        }
    }
}
