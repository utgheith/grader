package ag.r2

import ag.web.{Password, WebConfig}
import munit.FunSuite

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.security.KeyStore
import javax.net.ssl.{SSLContext, TrustManagerFactory}
import scala.jdk.CollectionConverters.*

class LiveRunTests extends FunSuite {
  private def free_port(): Int = {
    val s = java.net.ServerSocket(0)
    try s.getLocalPort
    finally s.close()
  }

  private def config() = WebConfig(
    port = free_port(),
    password_hash = Password.hash("pw".toCharArray),
    bind = Some("127.0.0.1")
  )

  // waits until `p` holds for root's control
  private def eventually(root: State)(p: Control.Status => Boolean): Unit = {
    val limit = System.currentTimeMillis() + 30000
    while (!p(root.control.status) && System.currentTimeMillis() < limit)
      Thread.sleep(10)
    assert(p(root.control.status), root.control.status.toString)
  }

  test("paused, the run's first target waits until someone resumes") {
    val root = State(os.temp.dir(deleteOnExit = true))
    val only: Target[Int] = Scope("start_paused").target(EmptyTuple)((_, _) => 5)
    LiveRun.serving(root, Some(config()), paused = true) {
      assert(root.control.status.paused)
      @volatile var value = 0
      val t = Thread.ofVirtual().start(() => value = root.track(only))
      eventually(root)(_.waiting.nonEmpty)
      assertEquals(root.control.status.waiting.map(_.gate), Seq("Start"))
      assertEquals(value, 0)
      root.control.resume()
      t.join(30000)
      assertEquals(value, 5)
    }
  }

  test("pause_at_end waits to exit until someone lets it, even after a failure") {
    val root = State(os.temp.dir(deleteOnExit = true))
    @volatile var exited = false
    val t = Thread.ofVirtual().start { () =>
      try
        LiveRun.serving(root, Some(config()), pause_at_end = true) {
          throw Exception("the run failed")
        }
      catch { case _: Exception => exited = true }
    }
    eventually(root)(_.waiting.nonEmpty)
    val held = root.control.status.waiting
    assertEquals(held.map(w => (w.path, w.gate)), Seq((RunGraph.root, "End")))
    Thread.sleep(100)
    assert(!exited)
    root.control.release(held.head.id)
    t.join(30000)
    assert(exited, "the failure still comes out once it's let go")
  }

  test("without a server, neither pause applies") {
    val root = State(os.temp.dir(deleteOnExit = true))
    val only: Target[Int] = Scope("no_server").target(EmptyTuple)((_, _) => 7)
    val v = LiveRun.serving(root, None, paused = true, pause_at_end = true)(root.track(only))
    assertEquals(v, 7)
  }

  test("the server shows a run behind a password, and controls it") {
    val workspace = os.temp.dir(deleteOnExit = true)
    val port = free_port()
    val config = WebConfig(
      port = port,
      password_hash = Password.hash("pw".toCharArray),
      bind = Some("127.0.0.1")
    )
    os.write(workspace / "targets" / "x" / "log.txt", "logged", createFolders = true)
    os.write(workspace / "secret.txt", "not served")
    val root = State(workspace)
    LiveRun.serving(root, Some(config)) {
      // trusts just the certificate the server made
      val trust = KeyStore.getInstance("PKCS12")
      val pass = os.read(workspace / "web" / "self-signed.pass").trim.toCharArray
      trust.load(os.read.inputStream(workspace / "web" / "self-signed.p12"), pass)
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(trust)
      val ssl = SSLContext.getInstance("TLS")
      ssl.init(null, tmf.getTrustManagers, null)
      val client = HttpClient.newBuilder().sslContext(ssl).build()
      val base = s"https://127.0.0.1:$port"

      def get(path: String, cookie: String = ""): HttpResponse[String] = {
        val b = HttpRequest.newBuilder(URI(base + path))
        if (cookie.nonEmpty) { val _ = b.header("Cookie", cookie) }
        client.send(b.build(), HttpResponse.BodyHandlers.ofString())
      }
      def post(path: String, body: String, kind: String, cookie: String = ""): HttpResponse[String] = {
        val b = HttpRequest
          .newBuilder(URI(base + path))
          .header("Content-Type", kind)
          .POST(HttpRequest.BodyPublishers.ofString(body))
        if (cookie.nonEmpty) { val _ = b.header("Cookie", cookie) }
        client.send(b.build(), HttpResponse.BodyHandlers.ofString())
      }

      // nothing without logging in
      val anon = get("/")
      assertEquals(anon.statusCode, 303)
      assertEquals(anon.headers.firstValue("Location").get, "login")
      assertEquals(get("/login").statusCode, 200)
      assertEquals(post("/api/control", """{"action":"pause"}""", "application/json").statusCode, 401)
      assertEquals(get("/", "grader_session=1.forged").statusCode, 303)
      assertEquals(post("/login", "password=wrong", "application/x-www-form-urlencoded").statusCode, 401)

      val ok = post("/login", "password=pw", "application/x-www-form-urlencoded")
      assertEquals(ok.statusCode, 303)
      val set_cookie = ok.headers.firstValue("Set-Cookie").get
      assert(set_cookie.contains("Secure") && set_cookie.contains("HttpOnly") && set_cookie.contains("SameSite=Strict"))
      val cookie = set_cookie.takeWhile(_ != ';')

      // the page, live
      val page = get("/", cookie)
      assertEquals(page.statusCode, 200)
      assert(page.body.contains("draw_graph"))
      assert(page.headers.firstValue("Content-Security-Policy").isPresent)
      assertEquals(get("/data.js", cookie).body.trim, "const GRAPH = null;")

      // the event stream starts with a snapshot
      val events = client.send(
        HttpRequest.newBuilder(URI(base + "/events")).header("Cookie", cookie).build(),
        HttpResponse.BodyHandlers.ofLines()
      )
      val first = events.body.iterator.asScala.takeWhile(_.nonEmpty).toSeq
      events.body.close()
      assertEquals(first.head, "event: snapshot")
      val snapshot = ujson.read(first(1).stripPrefix("data: "))
      assertEquals(snapshot("graph")("root").str, RunGraph.root)
      assertEquals(snapshot("control")("paused").bool, false)

      // controls take json only
      assertEquals(post("/api/control", """{"action":"pause"}""", "text/plain", cookie).statusCode, 415)
      val paused = post("/api/control", """{"action":"pause"}""", "application/json", cookie)
      assertEquals(paused.statusCode, 200)
      assert(ujson.read(paused.body)("paused").bool)
      assert(root.control.status.paused)
      val _ = post("/api/control", """{"action":"break","at":"a/b","on":true}""", "application/json", cookie)
      assertEquals(root.control.status.breakpoints, Seq("a/b"))
      val _ = post("/api/control", """{"action":"resume"}""", "application/json", cookie)
      assert(!root.control.status.paused)

      // the workspace's targets and command output, as text; nothing else
      val log = get("/targets/x/log.txt", cookie)
      assertEquals((log.statusCode, log.body), (200, "logged"))
      assert(log.headers.firstValue("Content-Type").get.startsWith("text/plain"))
      assertEquals(get("/secret.txt", cookie).statusCode, 404)
      assertEquals(get("/targets/../secret.txt", cookie).statusCode, 404)
      assertEquals(get("/targets/x/log.txt").statusCode, 303, "files need a login too")
    }
    // the session key and certificate are the owner's alone
    assertEquals(os.perms(workspace / "web" / "session.key").toString, "rw-------")
    assertEquals(os.perms(workspace / "web" / "self-signed.p12").toString, "rw-------")
  }
}
