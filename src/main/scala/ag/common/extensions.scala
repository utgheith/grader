package ag.common

import java.nio.charset.{Charset, StandardCharsets}
import java.security.MessageDigest
import java.time.{Duration, Instant, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.collection.{SortedMap, SortedSet}
import upickle.default.{ReadWriter, readwriter}

import java.util.concurrent.CountDownLatch
import scala.concurrent.Future

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
}

/////// Future ///////

extension [A](fa: Future[A]) {
  def block: A = {
    val latch = new CountDownLatch(1)
    fa.onComplete { _ =>
      latch.countDown()
    }
    latch.await()
    fa.value.get.get
  }
}
