package ag.r2

import ag.common.given_ReadWriter_Path
import upickle.default.ReadWriter

import com.sun.net.httpserver.{
  HttpExchange,
  HttpsConfigurator,
  HttpsServer
}

import java.io.{IOException, OutputStream}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyStore, MessageDigest, SecureRandom}
import java.security.cert.X509Certificate
import java.util.Base64
import java.util.concurrent.{CopyOnWriteArrayList, CountDownLatch, Executors}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import javax.net.ssl.{KeyManagerFactory, SSLContext}
import scala.util.control.NonFatal

// A PKCS12 keystore holding the server's certificate and key
@upickle.implicits.allowUnknownKeys(false)
case class Keystore(path: os.Path, password: String) derives ReadWriter {
  override def toString: String =
    s"Keystore(path = ${path.toString}, password = <redacted>)"
}

// config.json's "web": serve each run at https://<host>:<port>, behind
// a password (see `run hash_password`). Without a keystore, the server makes
// a self-signed certificate on first use, kept in the workspace.
@upickle.implicits.allowUnknownKeys(false)
case class WebConfig(
    port: Int,
    password_hash: String,
    // the address to listen on; all of them by default
    bind: Option[String] = None,
    keystore: Option[Keystore] = None
) derives ReadWriter {
  override def toString: String =
    s"WebConfig(port = $port, password_hash = <redacted>, bind = ${bind.toString}, keystore = ${keystore.toString})"
}

// Shows a run as it goes: the graph's page (see RunGraph), updated live, with
// controls to pause it and hold targets (see Control). Everything but the login
// page needs a session cookie, which a login gets and which stays good across
// runs until it expires or the password changes.
final class WebServer private (
    state: State,
    config: WebConfig,
    http: HttpsServer,
    secret: Array[Byte]
) {
  import WebServer.*

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

  private def start(): Unit = {
    http.createContext("/", ex => handle(ex))
    http.setExecutor(Executors.newVirtualThreadPerTaskExecutor())
    http.start()
    broadcaster.start()
  }

  // tells the clients the run is over, and stops
  private def stop(): Unit = {
    done = true
    broadcaster.interrupt()
    broadcaster.join(2000)
    try broadcast()
    catch { case NonFatal(_) => () }
    clients.forEach(_.closed.countDown())
    http.stop(1)
  }

  ///////////// sessions /////////////

  private def mac(data: String): String = {
    val m = Mac.getInstance("HmacSHA256").nn
    m.init(new SecretKeySpec(secret, "HmacSHA256"))
    Base64.getUrlEncoder.nn.withoutPadding.nn
      .encodeToString(m.doFinal(data.getBytes(UTF_8)))
  }

  // a changed password changes every session's mac, so ends them all
  private def new_session(): String = {
    val expires = System.currentTimeMillis() + session_ms
    s"$expires.${mac(s"$expires|${config.password_hash}")}"
  }

  private def valid_session(ex: HttpExchange): Boolean =
    cookie(ex, cookie_name).exists { v =>
      v.split('.') match {
        case Array(expires, m) =>
          expires.toLongOption.exists(_ > System.currentTimeMillis()) &&
          MessageDigest.isEqual(
            m.getBytes(UTF_8),
            mac(s"$expires|${config.password_hash}").getBytes(UTF_8)
          )
        case _ => false
      }
    }

  ///////////// requests /////////////

  private def handle(ex: HttpExchange): Unit =
    try {
      val h = ex.getResponseHeaders.nn
      h.set("Strict-Transport-Security", "max-age=31536000")
      h.set("X-Content-Type-Options", "nosniff")
      h.set("Referrer-Policy", "no-referrer")
      h.set("Cache-Control", "no-store")
      h.set(
        "Content-Security-Policy",
        "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'; frame-ancestors 'none'"
      )
      val path = ex.getRequestURI.nn.getPath.nn
      (ex.getRequestMethod, path) match {
        case ("GET", "/login") => reply(ex, 200, "text/html", login_page(false))
        case ("POST", "/login") => login(ex)
        case _ if !valid_session(ex) =>
          if (ex.getRequestMethod == "GET") {
            h.set("Location", "login")
            reply(ex, 303, "text/plain", "")
          } else reply(ex, 401, "text/plain", "log in first")
        case ("POST", "/logout") =>
          h.add("Set-Cookie", s"$cookie_name=; Max-Age=0; Path=/; Secure; HttpOnly; SameSite=Strict")
          reply(ex, 200, "text/plain", "")
        case ("GET", "/")          => reply(ex, 200, "text/html", RunGraph.page)
        case ("GET", "/data.js")   => reply(ex, 200, "text/javascript", "const GRAPH = null;\n")
        case ("GET", "/events")    => events(ex)
        case ("POST", "/api/control") => control(ex)
        case ("GET", _)            => file(ex, path)
        case _                     => reply(ex, 404, "text/plain", "not found")
      }
    } catch {
      case NonFatal(e) =>
        try reply(ex, 500, "text/plain", "server error")
        catch { case NonFatal(_) => () }
        say(s"web: ${e.toString}")
    } finally ex.close()

  private def login(ex: HttpExchange): Unit = {
    val body = String(ex.getRequestBody.nn.readNBytes(4096), UTF_8)
    val password = form(body).getOrElse("password", "")
    if (Password.check(password.toCharArray.nn, config.password_hash)) {
      ex.getResponseHeaders.nn.add(
        "Set-Cookie",
        s"$cookie_name=${new_session()}; Max-Age=${session_ms / 1000}; Path=/; Secure; HttpOnly; SameSite=Strict"
      )
      ex.getResponseHeaders.nn.set("Location", "./")
      reply(ex, 303, "text/plain", "")
    } else {
      // one wrong guess at a time, each a second long
      failed_logins.synchronized(Thread.sleep(1000))
      reply(ex, 401, "text/html", login_page(true))
    }
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
          s"""<li><a href="${html(path.stripSuffix("/"))}/${html(name)}">${html(name)}</a></li>"""
        }
        reply(ex, 200, "text/html", s"<!doctype html><meta charset=utf-8><title>${html(p.last)}</title><ul>${items.mkString}</ul>")
      case Some(p) =>
        ex.getResponseHeaders.nn.set("Content-Type", "text/plain; charset=utf-8")
        ex.sendResponseHeaders(200, os.size(p))
        val out = ex.getResponseBody.nn
        val _ = java.nio.file.Files.copy(p.toNIO, out)
        out.close()
      case None => reply(ex, 404, "text/plain", "not found")
    }
  }
}

object WebServer {
  private val cookie_name = "grader_session"
  private val session_ms: Long = 12L * 60 * 60 * 1000
  private val served_dirs = Set("targets", "std_out_err")
  private val failed_logins = new Object
  private val random = new SecureRandom()

  // Serves `state`'s run while `f` runs, if there's a config, and returns
  // f's value. A server that can't start (e.g. its port is taken by another
  // workspace's run) is reported and skipped: the run goes on without it.
  //
  // `paused`: the run starts paused, so nothing runs until someone resumes it
  // from the page (after setting breakpoints, say). `pause_at_end`: once `f`
  // is done (or failed), the run waits to exit until someone lets it, so its
  // final state can be looked at. Both need the server: without one, there'd
  // be no way to go on, so they're ignored.
  def serving[A](
      state: State,
      config: Option[WebConfig],
      paused: Boolean = false,
      pause_at_end: Boolean = false
  )(f: => A): A =
    config.flatMap(c => start(state, c)) match {
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

  private def start(state: State, config: WebConfig): Option[WebServer] =
    try {
      val (ks, password) = keystore(state, config)
      val kmf = KeyManagerFactory
        .getInstance(KeyManagerFactory.getDefaultAlgorithm)
        .nn
      kmf.init(ks, password)
      val ssl = SSLContext.getInstance("TLS").nn
      ssl.init(kmf.getKeyManagers, null, null)
      val address = config.bind match {
        case Some(host) => InetSocketAddress(host, config.port)
        case None       => InetSocketAddress(config.port)
      }
      val http = HttpsServer.create(address, 0).nn
      http.setHttpsConfigurator(HttpsConfigurator(ssl))
      val server = WebServer(state, config, http, session_secret(state))
      server.start()
      state.control.enable()
      println(
        s"serving this run at https://${config.bind.getOrElse(java.net.InetAddress.getLocalHost.nn.getCanonicalHostName)}:${config.port}/"
      )
      Some(server)
    } catch {
      case NonFatal(e) =>
        println(s"not serving this run: ${e.toString}")
        None
    }

  private def web_dir(state: State): os.Path = state.workspace / "web"

  // A random key for signing session cookies, kept in the workspace so
  // sessions outlive a run. Removing it ends every session.
  private def session_secret(state: State): Array[Byte] = {
    val f = web_dir(state) / "session.key"
    if (!os.isFile(f)) {
      val key = new Array[Byte](32)
      random.nextBytes(key)
      os.makeDir.all(web_dir(state))
      os.write.over(f, key, perms = "rw-------")
    }
    os.read.bytes(f)
  }

  private def keystore(
      state: State,
      config: WebConfig
  ): (KeyStore, Array[Char]) = {
    val (path, password) = config.keystore match {
      case Some(k) => (k.path, k.password)
      case None    => self_signed(state)
    }
    val ks = KeyStore.getInstance("PKCS12").nn
    val in = os.read.inputStream(path)
    try ks.load(in, password.toCharArray)
    finally in.close()
    (ks, password.toCharArray.nn)
  }

  // A self-signed certificate for this host, made with keytool on first use
  // and kept in the workspace. Its fingerprint is printed every time, to check
  // against what the browser shows.
  private def self_signed(state: State): (os.Path, String) = {
    val dir = web_dir(state)
    val ks = dir / "self-signed.p12"
    val pass = dir / "self-signed.pass"
    if (!os.isFile(ks) || !os.isFile(pass)) {
      os.makeDir.all(dir)
      os.remove.all(ks)
      val bytes = new Array[Byte](24)
      random.nextBytes(bytes)
      val password = Base64.getUrlEncoder.nn.withoutPadding.nn.encodeToString(bytes).nn
      os.write.over(pass, password, perms = "rw-------")
      val host = java.net.InetAddress.getLocalHost.nn.getCanonicalHostName.nn
      val _ = os
        .proc(
          "keytool", "-genkeypair", "-alias", "grader", "-keyalg", "EC",
          "-groupname", "secp256r1", "-validity", "3650",
          "-dname", s"CN=$host", "-ext", s"SAN=dns:$host,dns:localhost,ip:127.0.0.1",
          "-storetype", "PKCS12", "-keystore", ks, "-storepass", password,
          "-keypass", password
        )
        .call(stdout = os.Pipe, stderr = os.Pipe)
      os.perms.set(ks, "rw-------")
    }
    val password = os.read(pass).trim.nn
    val store = KeyStore.getInstance("PKCS12").nn
    val in = os.read.inputStream(ks)
    try store.load(in, password.toCharArray)
    finally in.close()
    store.getCertificate("grader") match {
      case cert: X509Certificate =>
        val digest = MessageDigest.getInstance("SHA-256").nn.digest(cert.getEncoded).nn
        println(
          s"self-signed certificate, SHA-256 ${digest.map(b => f"$b%02X").mkString(":")}"
        )
      case _ => ()
    }
    (ks, password)
  }

  ///////////// http helpers /////////////

  private def reply(
      ex: HttpExchange,
      code: Int,
      content_type: String,
      body: String
  ): Unit = {
    val bytes = body.getBytes(UTF_8)
    ex.getResponseHeaders.nn.set("Content-Type", s"$content_type; charset=utf-8")
    ex.sendResponseHeaders(code, if (bytes.isEmpty) -1 else bytes.length.toLong)
    if (bytes.nonEmpty) {
      val out = ex.getResponseBody.nn
      out.write(bytes)
      out.close()
    }
  }

  private def cookie(ex: HttpExchange, name: String): Option[String] = {
    val headers = Option(ex.getRequestHeaders.nn.get("Cookie")).toSeq
      .flatMap(l => scala.jdk.CollectionConverters.ListHasAsScala(l).asScala)
    (for {
      header <- headers
      part <- header.split(';').toSeq
      Array(k, v) = part.trim.nn.split("=", 2): @unchecked
      if k == name
    } yield v).headOption
  }

  private def form(body: String): Map[String, String] =
    (for {
      part <- body.split('&').toSeq
      kv = part.split("=", 2)
      if kv.length == 2
    } yield java.net.URLDecoder.decode(kv(0), UTF_8).nn ->
      java.net.URLDecoder.decode(kv(1), UTF_8).nn).toMap

  private def html(s: String): String =
    s.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case '\'' => "&#39;"
      case c    => c.toString
    }

  private def login_page(failed: Boolean): String =
    s"""<!doctype html><html lang="en"><head><meta charset="utf-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1">
       |<title>Run graph login</title><style>
       |:root { --bg: #fbfbfa; --fg: #1d1f23; --line: #d9dbe0; --accent: #2f6fdf; --bad: #c93232; }
       |@media (prefers-color-scheme: dark) { :root { --bg: #15171b; --fg: #e4e6ea; --line: #333842; --accent: #7aa7ff; --bad: #ff7b7b; } }
       |body { background: var(--bg); color: var(--fg); font: 15px system-ui, sans-serif; display: grid; place-items: center; min-height: 100vh; margin: 0; }
       |form { display: grid; gap: 10px; width: min(320px, 90vw); }
       |input, button { font: inherit; padding: 8px 10px; border-radius: 6px; border: 1px solid var(--line); background: var(--bg); color: var(--fg); }
       |button { background: var(--accent); color: #fff; border: 0; cursor: pointer; }
       |p { color: var(--bad); margin: 0; }
       |</style></head><body><form method="post" action="login">
       |<h1 style="font-size:18px;margin:0">Run graph</h1>
       |<input type="password" name="password" placeholder="Password" autofocus required autocomplete="current-password">
       |${if (failed) "<p>Wrong password.</p>" else ""}
       |<button>Log in</button></form></body></html>""".stripMargin
}
