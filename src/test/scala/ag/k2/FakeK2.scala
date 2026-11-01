package ag.k2

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.time.Instant
import scala.collection.mutable

// A K2 stream on localhost, speaking the parts of K2's HTTP API the client
// uses, for tests. Each subscription keeps one position, and leases one batch
// at a time per worker. `clock` stamps arriving records.
final class FakeK2(var clock: () => Instant = () => Instant.now().nn) {
  final case class Stored(
      content: String,
      headers: Map[String, String],
      at: Instant
  )
  private final case class Lease(id: String, from: Int, until: Int)
  private final case class Sub(
      name: String,
      var position: Int,
      leases: mutable.Map[String, Lease]
  )

  val records: mutable.ArrayBuffer[Stored] = mutable.ArrayBuffer()
  private val subs = mutable.LinkedHashMap[String, Sub]()
  private var next_id = 0

  // The next produce fails with (HTTP status, code, retryable), after
  // storing its batch if `store`.
  var fail_next_produce: Option[(Int, Int, Boolean, Boolean)] = None
  var produce_calls: Int = 0
  var token: Option[String] = None

  private val server =
    HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0).nn
  server.createContext("/", (ex: HttpExchange) => handle(ex))
  server.start()

  val url: String = s"http://127.0.0.1:${server.getAddress.nn.getPort}"

  def stop(): Unit = server.stop(0)

  private def fresh(prefix: String): String = {
    next_id += 1
    s"$prefix$next_id"
  }

  private def ok(result: ujson.Value): (Int, ujson.Value) =
    200 -> ujson.Obj(
      "result" -> result,
      "success" -> true,
      "errors" -> ujson.Arr(),
      "messages" -> ujson.Arr()
    )

  private def produce_error(status: Int, code: Int, retryable: Boolean) =
    status -> ujson.Obj(
      "success" -> false,
      "error" -> ujson.Obj(
        "code" -> code,
        "message" -> "fake",
        "retryable" -> retryable
      )
    )

  private def handle(ex: HttpExchange): Unit = synchronized {
    val method = ex.getRequestMethod.nn
    val uri = ex.getRequestURI.nn
    val path = uri.getPath.nn.split('/').toList.filter(_.nonEmpty)
    val body =
      String(ex.getRequestBody.nn.readAllBytes().nn, StandardCharsets.UTF_8)
    val json = if (body.isEmpty) ujson.Null else ujson.read(body)
    val authorized = token.forall(t =>
      Option(ex.getRequestHeaders.nn.getFirst("Authorization"))
        .contains(s"Bearer $t")
    )
    val response: (Int, ujson.Value) =
      if (!authorized) produce_error(401, 10209, false)
      else
        (method, path) match {
          case ("POST", List("produce")) if body.length > 5_000_000 =>
            produce_calls += 1
            produce_error(413, 10206, false)
          case ("POST", List("produce")) =>
            produce_calls += 1
            val batch = json("records").arr.toSeq.map(r =>
              Stored(
                r("content").str,
                r.obj
                  .get("headers")
                  .fold(Map())(_.obj.map((k, v) => k -> v.str).toMap),
                clock()
              )
            )
            fail_next_produce match {
              case Some((status, code, retryable, store)) =>
                fail_next_produce = None
                if (store) records ++= batch
                produce_error(status, code, retryable)
              case None =>
                records ++= batch
                200 -> ujson.Obj("success" -> true)
            }
          case ("GET", List("subscriptions")) =>
            val name = Option(uri.getQuery).map(_.stripPrefix("name="))
            ok(ujson.Arr.from(subs.collect {
              case (id, s) if name.forall(_ == s.name) =>
                ujson.Obj("id" -> id, "name" -> s.name)
            }))
          case ("POST", List("subscriptions")) =>
            val id = fresh("sub")
            val start =
              if (json("start_at")("type").str == "earliest") 0
              else records.size
            subs(id) = Sub(json("name").str, start, mutable.Map())
            ok(ujson.Obj("id" -> id))
          case ("POST", List("subscriptions", id, "consume")) =>
            val sub = subs(id)
            val worker = json("worker_id").str
            val lease = sub.leases.get(worker).orElse {
              val from = sub.position
              val until =
                (from + json("max_records").num.toInt).min(records.size)
              Option.when(until > from) {
                val l = Lease(fresh("batch"), from, until)
                sub.leases(worker) = l
                l
              }
            }
            ok(
              lease.fold(
                ujson.Obj(
                  "batch_id" -> ujson.Null,
                  "leased_until_ms" -> ujson.Null,
                  "records" -> ujson.Arr()
                )
              ) { l =>
                ujson.Obj(
                  "batch_id" -> l.id,
                  "leased_until_ms" -> clock()
                    .plusSeconds(300)
                    .nn
                    .toEpochMilli
                    .toDouble,
                  "records" -> ujson.Arr.from(
                    records.slice(l.from, l.until).map { r =>
                      val o = ujson.Obj(
                        "timestamp_ms" -> r.at.toEpochMilli.toDouble,
                        "content" -> r.content
                      )
                      if (r.headers.nonEmpty)
                        o("headers") = ujson.Obj
                          .from(r.headers.map((k, v) => k -> ujson.Str(v)))
                      o
                    }
                  )
                )
              }
            )
          case ("POST", List("subscriptions", id, "batches", batch, what)) =>
            val sub = subs(id)
            val worker = json("worker_id").str
            sub.leases.get(worker).filter(_.id == batch) match {
              case Some(l) if what == "ack" =>
                sub.position = l.until
                sub.leases -= worker
                ok(ujson.Obj())
              case Some(_) if what == "nack" =>
                sub.leases -= worker
                ok(ujson.Obj())
              case Some(_) =>
                ok(
                  ujson.Obj(
                    "leased_until_ms" -> clock()
                      .plusSeconds(300)
                      .nn
                      .toEpochMilli
                      .toDouble
                  )
                )
              case None if what == "ack" =>
                ok(ujson.Obj()) // acking twice is fine
              case None =>
                409 -> ujson.Obj(
                  "success" -> false,
                  "errors" -> ujson.Arr(
                    ujson.Obj("code" -> 10218, "message" -> "not your lease")
                  )
                )
            }
          case _ => 404 -> ujson.Obj("success" -> false)
        }
    val (status, reply) = response
    val bytes = ujson.write(reply).getBytes(StandardCharsets.UTF_8)
    ex.getResponseHeaders.nn.add("Content-Type", "application/json")
    ex.sendResponseHeaders(status, bytes.length.toLong)
    val out = ex.getResponseBody.nn
    out.write(bytes)
    out.close()
  }
}
