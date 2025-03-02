package ag.common

import java.nio.charset.{Charset, StandardCharsets}
import java.security.{DigestOutputStream, MessageDigest}
import java.time.{Duration, Instant, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.collection.{SortedMap, SortedSet}
import upickle.default.{ReadWriter, readwriter}

import java.io.OutputStream
import java.util.concurrent.{CountDownLatch, Semaphore}
import scala.concurrent.Future
import scala.util.Try

///// ReadWriter for SortedMap ////

given [K: {Ordering, ReadWriter}, V: ReadWriter]: ReadWriter[SortedMap[K, V]] =
  readwriter[Seq[(K, V)]]
    .bimap[SortedMap[K, V]](sm => sm.to(Seq), s => s.to(SortedMap))

////// ReadWriter for SortedSet //////

given [K: {Ordering, ReadWriter}]: ReadWriter[SortedSet[K]] =
  readwriter[Seq[K]]
    .bimap[SortedSet[K]](ss => ss.toSeq, s => s.to(SortedSet))

/////// ReadWriter for os.RelPath /////

given ReadWriter[os.RelPath] = readwriter[String]
  .bimap[os.RelPath](_.toString, s => os.RelPath(s))

given ReadWriter[os.Path] = readwriter[String]
  .bimap[os.Path](_.toString, s => os.Path(s))

//////// ReadWriter for LocalDateTime /////

given ReadWriter[LocalDateTime] = {
  val dateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy:MM:dd:HH:mm:ss").nn

  readwriter[String].bimap[LocalDateTime](
    ins => ins.format(dateTimeFormatter).nn,
    str => LocalDateTime.parse(str, dateTimeFormatter).nn
  )
}

///////// ReadWriter for Instant //////////

given ReadWriter[Instant] = readwriter[(Long, Int)].bimap[Instant](
  ins => (ins.getEpochSecond, ins.getNano),
  p => Instant.ofEpochSecond(p._1, p._2)
)

//////// ReadWriter for ZoneId ////////

given ReadWriter[ZoneId] = readwriter[String].bimap[ZoneId](
  zid => zid.getId,
  str => ZoneId.of(str)
)

//////// ReadWriter for ZonedDateTime //////

given ReadWriter[ZonedDateTime] =
  readwriter[(LocalDateTime, ZoneId)].bimap[ZonedDateTime](
    zdt => (zdt.toLocalDateTime, zdt.getZone),
    pair => ZonedDateTime.of(pair._1, pair._2)
  )

//////// ReadWriter for ZonedDateTime //////

given ReadWriter[Duration] = readwriter[String]
  .bimap[Duration](dur => dur.toString, str => Duration.parse(str))

////// MessageDigest //////

extension (md: MessageDigest) {
  def update(s: String, charset: Charset = StandardCharsets.UTF_8.nn): Unit =
    md.update(s.getBytes(charset))
  def update(path: os.Path, exclude: os.RelPath => Boolean): Unit = {
    def one(base: os.Path, path: os.Path): Unit = {
      if (!exclude(path.relativeTo(base))) {
        md.update(path.relativeTo(base).toString)
        if (os.isLink(path)) {
          md.update("L")
          os.followLink(path)
            .foreach(p => md.update(p.relativeTo(base).toString))
          md.update("l")
        } else if (os.isFile(path)) {
          md.update("F")
          val dos = DigestOutputStream(OutputStream.nullOutputStream(), md)
          try {
            os.read.stream(path).writeBytesTo(dos)
          } finally {
            dos.close()
          }
          md.update("f")
        } else if (os.isDir(path)) {
          for {
            p <- os.list(path, sort = true)
          } one(base, p)
        } else if (os.exists(path)) {
          throw Exception(s"$path")
        }
      }
    }

    one(path, path)
  }
}

/////// Future ///////

extension [A](fa: Future[A]) {
  def slow: Try[A] = {
    val latch = new CountDownLatch(1)
    fa.onComplete { _ =>
      latch.countDown()
    }
    latch.await()
    fa.value.get
  }

  inline def block_try: Try[A] = fa.value.getOrElse(slow)
  inline def block: A = block_try.get
}

////// Semaphore /////

extension (s: Semaphore) {
  def down[A](permits: Int)(f: => A): A = try {
    s.acquire(permits)
    f
  } finally {
    s.release(permits)
  }

  def down[A](f: => A): A = down(1)(f)
}

//////// timed ///////

def timed[A](f: => A): (Long, A) = {
  val start = System.currentTimeMillis()
  val out = f
  (System.currentTimeMillis() - start, out)
}

def human(ms: Long): String = {
  if (ms < 1000) {
    s"${ms}ms"
  } else if (ms < 60 * 1000) {
    f"${ms / 1000.0}%.03fs"
  } else {
    f"${ms / (1000.0 * 60.0)}%.03fm"
  }
}
