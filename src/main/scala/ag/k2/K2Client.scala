package ag.k2

import language.experimental.captureChecking
import language.experimental.saferExceptions

import java.io.IOException
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{Duration, Instant}
import scala.annotation.nowarn

// ---- K2's HTTP API (https://developers.cloudflare.com/k2/)
//
// A stream is a durable log of records. A record is opaque bytes (`content`)
// plus a few string headers; K2 stamps it with the time it arrives. Producers
// append batches, atomically. Consumers read through named subscriptions:
// each hands out leased batches, and moves past a batch once it's acked.
// Delivery is at least once, and K2 doesn't deduplicate: a retried produce
// can store a batch twice.

final case class Record(content: Array[Byte], headers: Map[String, String])

// A record as read: with K2's arrival time.
final case class Received(
    content: Array[Byte],
    headers: Map[String, String],
    at: Instant
)

// A leased batch: ack it once handled, or K2 redelivers it once the lease
// runs out.
final case class Batch(
    id: String,
    leased_until: Instant,
    records: Seq[Received]
)

// An error K2 reported, or K2 out of reach (status and code 0, retryable).
// `retryable`: the same request may be sent again later. `maybe_stored`
// (code 10212): a produce failed after possibly storing the batch, so
// sending it again may store it twice. Checked: sources are often offline,
// so every caller has to decide what that means for it.
final case class K2Exception(
    status: Int,
    code: Int,
    message: String,
    retryable: Boolean
) extends Exception(s"K2 error $code (HTTP $status): $message") {
  def maybe_stored: Boolean = code == 10212
}

object K2 {
  // per record (content and headers) and per produce request
  val max_record_bytes: Int = 1_000_000
  val max_request_bytes: Int = 5_000_000

  // the codes K2 says to retry after a backoff (the others need a change)
  private val retryable_codes = Set(10211, 10214, 10216, 10217)

  def is_retryable(code: Int): Boolean = retryable_codes.contains(code)
}

// One stream. `token`: an API token with K2 Produce and/or Consume, or None
// for a stream whose HTTP input needs no authentication (producing only).
// `base`: the stream's endpoint; tests point it at a fake K2. A capability: a
// pure function can't send with one.
final class K2Client(
    stream_id: String,
    token: Option[String],
    base: String = "",
    http: HttpClient = HttpClient.newHttpClient().nn,
    timeout: Duration = Duration.ofSeconds(30).nn
) extends caps.SharedCapability {
  private val endpoint =
    if (base.nonEmpty) base.stripSuffix("/")
    else s"https://$stream_id.k2.cloudflarestorage.com"

  private def request(
      method: String,
      path: String,
      body: Option[ujson.Value]
  ): ujson.Value throws K2Exception = {
    val builder = HttpRequest
      .newBuilder(URI.create(endpoint + path))
      .nn
      .timeout(timeout)
      .nn
      .method(
        method,
        body.fold(HttpRequest.BodyPublishers.noBody().nn)(b =>
          HttpRequest.BodyPublishers.ofString(ujson.write(b)).nn
        )
      )
      .nn
    body.foreach(_ => builder.header("Content-Type", "application/json"))
    token.foreach(t => builder.header("Authorization", s"Bearer $t"))
    // (the compiler makes a CanThrow for this try that nothing uses, and
    // then warns about it)
    @nowarn("msg=unused local definition")
    val response =
      try http.send(builder.build(), HttpResponse.BodyHandlers.ofString()).nn
      catch {
        case e: IOException =>
          val k = K2Exception(0, 0, s"can't reach K2: ${e.toString}", true)
          val _ = k.initCause(e)
          throw k
      }
    val text = String.valueOf(response.body())
    val json = scala.util.Try(ujson.read(text)).toOption
    val ok = json.exists(
      _.objOpt.exists(_.get("success").exists(_.boolOpt.contains(true)))
    )
    if (response.statusCode() / 100 == 2 && ok) json.get
    else throw error_of(response.statusCode(), json, text)
  }

  // Produce errors come as {"error": {code, message, retryable}}; the others
  // as {"errors": [{code, message}]}.
  private def error_of(
      status: Int,
      json: Option[ujson.Value],
      text: String
  ): K2Exception = {
    val err = for {
      obj <- json.flatMap(_.objOpt)
      e <- obj.get("error").orElse {
        for {
          errors <- obj.get("errors")
          arr <- errors.arrOpt
          first <- arr.headOption
        } yield first
      }
      fields <- e.objOpt
    } yield fields
    def field[A](name: String, as: ujson.Value => Option[A]): Option[A] =
      for {
        fields <- err
        v <- fields.get(name)
        a <- as(v)
      } yield a
    val code = field("code", _.numOpt).fold(0)(_.toInt)
    val message = field("message", _.strOpt).getOrElse(text.take(200))
    val retryable = field("retryable", _.boolOpt)
      .getOrElse(K2.is_retryable(code) || (code == 0 && status >= 500))
    K2Exception(status, code, message, retryable)
  }

  // Appends `records` to the stream, all or none.
  def produce(records: Seq[Record]): Unit throws K2Exception = {
    val _ = request(
      "POST",
      "/produce",
      Some(
        ujson.Obj(
          "records" -> ujson.Arr.from(records.map { r =>
            val o = ujson.Obj("content" -> Crypto.b64(r.content))
            if (r.headers.nonEmpty)
              o("headers") =
                ujson.Obj.from(r.headers.map((k, v) => k -> ujson.Str(v)))
            o
          })
        )
      )
    )
  }

  // The id of the subscription named `name`, creating it (to start at the
  // stream's oldest record, or after its newest) if there's none.
  // Subscriptions can't be changed: an existing one keeps its own start.
  def subscription(
      name: String,
      from_earliest: Boolean
  ): String throws K2Exception = {
    val found = request(
      "GET",
      s"/subscriptions?name=${java.net.URLEncoder.encode(name, "UTF-8")}",
      None
    )("result").arr.headOption
    found.fold {
      request(
        "POST",
        "/subscriptions",
        Some(
          ujson.Obj(
            "name" -> name,
            "start_at" -> ujson.Obj(
              "type" -> (if (from_earliest) "earliest" else "latest")
            )
          )
        )
      )("result")("id").str
    }(_("id").str)
  }

  // The next batch for `worker` (at most `max` records), or None if there
  // are no records now. A worker that already holds a lease gets that batch
  // again, with the lease renewed.
  def consume(
      subscription: String,
      worker: String,
      max: Int
  ): Option[Batch] throws K2Exception = {
    val r = request(
      "POST",
      s"/subscriptions/$subscription/consume",
      Some(ujson.Obj("worker_id" -> worker, "max_records" -> max))
    )("result")
    r("batch_id").strOpt.map { id =>
      Batch(
        id,
        Instant.ofEpochMilli(millis(r("leased_until_ms"))).nn,
        r("records").arr.toSeq.map { rec =>
          Received(
            Crypto.unb64(rec("content").str),
            rec.obj
              .get("headers")
              .fold(Map())(_.obj.map((k, v) => k -> v.str).toMap),
            Instant.ofEpochMilli(millis(rec("timestamp_ms"))).nn
          )
        }
      )
    }
  }

  private def batch_call(
      subscription: String,
      worker: String,
      batch: String,
      what: String
  ): ujson.Value throws K2Exception =
    request(
      "POST",
      s"/subscriptions/$subscription/batches/$batch/$what",
      Some(ujson.Obj("worker_id" -> worker))
    )("result")

  // done with `batch`: K2 moves past it (safe to repeat)
  def ack(
      subscription: String,
      worker: String,
      batch: String
  ): Unit throws K2Exception =
    val _ = batch_call(subscription, worker, batch, "ack")

  // give `batch` back now, to be redelivered as a new batch
  def nack(
      subscription: String,
      worker: String,
      batch: String
  ): Unit throws K2Exception =
    val _ = batch_call(subscription, worker, batch, "nack")

  // five more minutes on `batch`'s lease; returns when the lease now ends
  def extend(
      subscription: String,
      worker: String,
      batch: String
  ): Instant throws K2Exception =
    Instant
      .ofEpochMilli(
        millis(
          batch_call(subscription, worker, batch, "extend")("leased_until_ms")
        )
      )
      .nn

  // epoch milliseconds, as a JSON number (or, to be safe, a numeric string)
  private def millis(v: ujson.Value): Long =
    v.numOpt.fold(v.str.toLong)(_.toLong)
}
