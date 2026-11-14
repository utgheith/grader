package ag.web

import ag.common.TempDirs
import munit.FunSuite

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.security.KeyStore
import javax.net.ssl.{SSLContext, TrustManagerFactory}

class PasswordTests extends FunSuite {
  test("a hash checks its password, and only it") {
    val h = Password.hash("secret".toCharArray)
    assert(h.startsWith("pbkdf2-sha256$"))
    assert(Password.check("secret".toCharArray, h))
    assert(!Password.check("Secret".toCharArray, h))
    assertNotEquals(Password.hash("secret".toCharArray), h, "salted")
    assert(!Password.check("secret".toCharArray, "nonsense"))
    assert(!Password.check("secret".toCharArray, "pbkdf2-sha256$1$!!$!!"))
  }
}

class WebServerTests extends FunSuite {
  private def free_port(): Int = {
    val s = java.net.ServerSocket(0)
    try s.getLocalPort
    finally s.close()
  }

  private object pages extends WebPages {
    val prefix = "status"
    def page(path: String): Option[WebPage] =
      Option.when(path == "" || path == "x")(WebPage("text/html", s"page [$path]"))
  }

  test("pages alone, without a run: behind the password, from /") {
    val workspace = TempDirs.dir()
    val port = free_port()
    val config = WebConfig(
      port = port,
      password_hash = Password.hash("pw".toCharArray),
      bind = Some("127.0.0.1")
    )
    val server = WebServer.start(workspace, config, Some(pages), None).get
    try {
      val trust = KeyStore.getInstance("PKCS12")
      val pass = os.read(workspace / "web" / "self-signed.pass").trim.toCharArray
      trust.load(os.read.inputStream(workspace / "web" / "self-signed.p12"), pass)
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(trust)
      val ssl = SSLContext.getInstance("TLS")
      ssl.init(null, tmf.getTrustManagers, null)
      val client = HttpClient.newBuilder().sslContext(ssl).build()
      def get(path: String, cookie: String = ""): HttpResponse[String] = {
        val b = HttpRequest.newBuilder(URI(s"https://127.0.0.1:$port$path"))
        if (cookie.nonEmpty) { val _ = b.header("Cookie", cookie) }
        client.send(b.build(), HttpResponse.BodyHandlers.ofString())
      }
      assertEquals(get("/status/x").statusCode, 303, "a login first")
      val ok = client.send(
        HttpRequest
          .newBuilder(URI(s"https://127.0.0.1:$port/login"))
          .header("Content-Type", "application/x-www-form-urlencoded")
          .POST(HttpRequest.BodyPublishers.ofString("password=pw"))
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
      val cookie = ok.headers.firstValue("Set-Cookie").get.takeWhile(_ != ';')
      val root = get("/", cookie)
      assertEquals((root.statusCode, root.headers.firstValue("Location").get), (303, "status/"))
      assertEquals(get("/status/x", cookie).body, "page [x]")
      assertEquals(get("/status/y", cookie).statusCode, 404)
      // no run: none of its requests
      assertEquals(get("/events", cookie).statusCode, 404)
      assertEquals(get("/data.js", cookie).statusCode, 404)
    } finally server.stop()
  }
}
