package ag.web

import ag.common.given_ReadWriter_Path
import upickle.default.ReadWriter

import com.sun.net.httpserver.{HttpExchange, HttpsConfigurator, HttpsServer}

import java.io.OutputStream
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyStore, MessageDigest, SecureRandom}
import java.security.cert.X509Certificate
import java.util.Base64
import java.util.concurrent.Executors
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

// More pages for a WebServer to serve, behind its login, at /<prefix>/...
trait WebPages {
  val prefix: String
  // the page at `path` below the prefix ("" for its front page), with its
  // segments still URL-encoded; None if there's none
  def page(path: String): Option[WebPage]
}

case class WebPage(content_type: String, body: String)

// A run, shown as it goes (see ag.r2.LiveRun), at the server's root: the
// requests it answers once logged in, besides the pages'
trait RunView {
  // answers `ex` if it's one of its requests; false if it isn't
  def handle(ex: HttpExchange, method: String, path: String): Boolean
  def start(): Unit
  // the run is over
  def stop(): Unit
}

// Serves `pages`, and a run if there's one (`run`), behind a password.
// Everything but the login page needs a session cookie, which a login gets
// and which stays good across runs until it expires or the password changes.
// The workspace keeps the session key and the self-signed certificate.
final class WebServer private (
    config: WebConfig,
    http: HttpsServer,
    secret: Array[Byte],
    pages: Option[WebPages],
    run: Option[RunView]
) {
  import WebServer.*

  private def start(): Unit = {
    http.createContext("/", ex => handle(ex))
    http.setExecutor(Executors.newVirtualThreadPerTaskExecutor())
    http.start()
    run.foreach(_.start())
  }

  // tells the run's viewers it's over, and stops
  def stop(): Unit = {
    run.foreach(_.stop())
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
        case ("POST", "/login")      => login(ex)
        case _ if !valid_session(ex) =>
          if (ex.getRequestMethod == "GET") {
            h.set("Location", "login")
            reply(ex, 303, "text/plain", "")
          } else reply(ex, 401, "text/plain", "log in first")
        case ("POST", "/logout") =>
          h.add(
            "Set-Cookie",
            s"$cookie_name=; Max-Age=0; Path=/; Secure; HttpOnly; SameSite=Strict"
          )
          reply(ex, 200, "text/plain", "")
        case ("GET", "/") if run.isEmpty && pages.nonEmpty =>
          h.set("Location", s"${pages.get.prefix}/")
          reply(ex, 303, "text/plain", "")
        case ("GET", p) if pages.exists(w => p == s"/${w.prefix}") =>
          h.set("Location", s"${pages.get.prefix}/")
          reply(ex, 303, "text/plain", "")
        case ("GET", p) if pages.exists(w => p.startsWith(s"/${w.prefix}/")) =>
          val w = pages.get
          val raw =
            ex.getRequestURI.nn.getRawPath.nn.stripPrefix(s"/${w.prefix}/")
          w.page(raw) match {
            case Some(page) => reply(ex, 200, page.content_type, page.body)
            case None       => reply(ex, 404, "text/plain", "not found")
          }
        case (method, p) if run.exists(_.handle(ex, method, p)) => ()
        case _ => reply(ex, 404, "text/plain", "not found")
      }
    } catch {
      case NonFatal(e) =>
        try reply(ex, 500, "text/plain", "server error")
        catch { case NonFatal(_) => () }
        ag.common.say(s"web: ${e.toString}")
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
}

object WebServer {
  private val cookie_name = "grader_session"
  private val session_ms: Long = 12L * 60 * 60 * 1000
  private val failed_logins = new Object
  private val random = new SecureRandom()

  // Serves just `pages` (no run) until the program is stopped. The
  // workspace holds the server's keys and certificate, as for runs. False if
  // the server can't start (the reason is printed).
  def serve_pages(
      workspace: os.Path,
      config: WebConfig,
      pages: WebPages
  ): Boolean =
    start(workspace, config, Some(pages), None) match {
      case None    => false
      case Some(_) =>
        new java.util.concurrent.CountDownLatch(1).await()
        true
    }

  // Starts serving `pages` and `run`; None if it can't (the reason is
  // printed)
  def start(
      workspace: os.Path,
      config: WebConfig,
      pages: Option[WebPages],
      run: Option[RunView]
  ): Option[WebServer] =
    try {
      val (ks, password) = keystore(workspace, config)
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
      val server =
        WebServer(config, http, session_secret(workspace), pages, run)
      server.start()
      val host = config.bind.getOrElse(
        java.net.InetAddress.getLocalHost.nn.getCanonicalHostName
      )
      val what =
        if (run.nonEmpty) "this run" else pages.fold("")(_.prefix)
      println(s"serving $what at https://$host:${config.port}/")
      pages
        .filter(_ => run.nonEmpty)
        .foreach(w =>
          println(
            s"  and ${w.prefix} at https://$host:${config.port}/${w.prefix}/"
          )
        )
      Some(server)
    } catch {
      case NonFatal(e) =>
        println(
          s"not serving ${if (run.nonEmpty) "this run" else "pages"}: ${e.toString}"
        )
        None
    }

  private def web_dir(workspace: os.Path): os.Path = workspace / "web"

  // A random key for signing session cookies, kept in the workspace so
  // sessions outlive a run. Removing it ends every session.
  private def session_secret(workspace: os.Path): Array[Byte] = {
    val f = web_dir(workspace) / "session.key"
    if (!os.isFile(f)) {
      val key = new Array[Byte](32)
      random.nextBytes(key)
      os.makeDir.all(web_dir(workspace))
      os.write.over(f, key, perms = "rw-------")
    }
    os.read.bytes(f)
  }

  private def keystore(
      workspace: os.Path,
      config: WebConfig
  ): (KeyStore, Array[Char]) = {
    val (path, password) = config.keystore match {
      case Some(k) => (k.path, k.password)
      case None    => self_signed(workspace)
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
  private def self_signed(workspace: os.Path): (os.Path, String) = {
    val dir = web_dir(workspace)
    val ks = dir / "self-signed.p12"
    val pass = dir / "self-signed.pass"
    if (!os.isFile(ks) || !os.isFile(pass)) {
      os.makeDir.all(dir)
      os.remove.all(ks)
      val bytes = new Array[Byte](24)
      random.nextBytes(bytes)
      val password =
        Base64.getUrlEncoder.nn.withoutPadding.nn.encodeToString(bytes).nn
      os.write.over(pass, password, perms = "rw-------")
      val host = java.net.InetAddress.getLocalHost.nn.getCanonicalHostName.nn
      val _ = os
        .proc(
          "keytool",
          "-genkeypair",
          "-alias",
          "grader",
          "-keyalg",
          "EC",
          "-groupname",
          "secp256r1",
          "-validity",
          "3650",
          "-dname",
          s"CN=$host",
          "-ext",
          s"SAN=dns:$host,dns:localhost,ip:127.0.0.1",
          "-storetype",
          "PKCS12",
          "-keystore",
          ks,
          "-storepass",
          password,
          "-keypass",
          password
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
        val digest =
          MessageDigest.getInstance("SHA-256").nn.digest(cert.getEncoded).nn
        println(
          s"self-signed certificate, SHA-256 ${digest.map(b => f"$b%02X").mkString(":")}"
        )
      case _ => ()
    }
    (ks, password)
  }

  ///////////// http helpers /////////////

  def reply(
      ex: HttpExchange,
      code: Int,
      content_type: String,
      body: String
  ): Unit = {
    val bytes = body.getBytes(UTF_8)
    ex.getResponseHeaders.nn
      .set("Content-Type", s"$content_type; charset=utf-8")
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

  def html(s: String): String =
    s.flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
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
