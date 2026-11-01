package ag.k2

import language.experimental.captureChecking
import language.experimental.saferExceptions

import upickle.default.ReadWriter

import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}
import scala.util.control.NonFatal

// ---- Private data, and key shredding
//
// Events themselves are not sealed: anyone who can read the stream can read
// them. Private members go in a Sealed[T], encrypted with a key that never
// enters the stream. Every producer and consumer keeps the keys it holds in
// its own local KeyStore. Shredding a key there makes every Sealed value
// made with it unreadable on that machine, wherever the value is kept (in
// K2, in an outbox, in an archive); shredding it everywhere erases it.

// Names a key. Usually one per subject (e.g. per student), so that shredding
// it erases everything sealed about that subject and nothing else.
final case class KeyId(value: String) {
  override def toString: String = value
}

object KeyId {
  def fresh(): KeyId = KeyId(UUID.randomUUID().toString)

  // (written out: upickle's derivation doesn't work under capture checking)
  given ReadWriter[KeyId] =
    upickle.default.readwriter[String].bimap(_.value, KeyId(_))
}

// Base64, for keys and sealed values in text.
object Crypto {
  def b64(bytes: Array[Byte]): String =
    Base64.getEncoder.nn.encodeToString(bytes).nn
  def unb64(s: String): Array[Byte] = Base64.getDecoder.nn.decode(s).nn
}

// Checked: a key store that doesn't hold what its owner expects (a key
// to seal with, a key that opens what it names, no other secret under an
// id) needs a decision, not a crash.
final class SealedException(message: String) extends Exception(message)

// A value sealed with a key: safe to put in an event. It opens only where
// the local key store has the key. A missing key is not an error: the key
// was never given to this machine, or it was shredded, and either way the
// value is simply unavailable (None). A value that doesn't open with the key
// it names was tampered with, or the key was replaced, or it isn't a T (it
// was sealed as another type, or T has changed since): those are errors,
// all SealedException. What opens is a Plain, tied to `keys` (see Plain). It's sealed with
// AES-256-GCM, bound to its key id (as associated data): it opens only with
// that key, under that id.
final case class Sealed[T](key: KeyId, nonce: String, data: String) {
  def open(using
      keys: KeyStore,
      rw: ReadWriter[T]
  ): Option[Plain[T]^{keys}] throws SealedException =
    keys
      .get(key)
      .map { aes =>
        def failed(why: String): Nothing =
          throw SealedException(s"a value sealed with key ${key.value} $why")
        val box =
          try
            Box(
              IArray.unsafeFromArray(Crypto.unb64(nonce)),
              IArray.unsafeFromArray(Crypto.unb64(data))
            )
          catch {
            case e: IllegalArgumentException =>
              failed(s"isn't base64: ${e.getMessage}")
          }
        val plain =
          try aes.open(box, Sealed.associated(key))
          catch {
            case e: CryptoException =>
              failed(s"doesn't open with it: ${e.getMessage}")
          }
        val value =
          try upickle.default.read[T](plain.copy)
          catch {
            case NonFatal(e) => // upickle's, which vary
              failed(s"doesn't read as expected: ${e.getMessage}")
          }
        Plain(value, keys)
      }
}

object Sealed {
  // what a value is bound to: its key's id
  private def associated(key: KeyId): IArray[Byte] =
    IArray.unsafeFromArray(key.value.getBytes(StandardCharsets.UTF_8))

  // the stored form is the same whatever T is
  given [T] => ReadWriter[Sealed[T]] =
    upickle.default
      .readwriter[ujson.Value]
      .bimap(
        s =>
          ujson.Obj("key" -> s.key.value, "nonce" -> s.nonce, "data" -> s.data),
        j => Sealed(KeyId(j("key").str), j("nonce").str, j("data").str)
      )

  // Seals `value` with `key`, which the local store must have: a producer
  // creates (KeyStore.create) the keys it seals with.
  def apply[T: ReadWriter](value: T, key: KeyId)(using
      keys: KeyStore
  ): Sealed[T] throws SealedException = {
    val aes = keys
      .get(key)
      .getOrElse(throw SealedException(s"no key ${key.value} to seal with"))
    val box = aes.seal(
      IArray.unsafeFromArray(upickle.default.writeToByteArray(value)),
      associated(key)
    )
    Sealed(
      key,
      Crypto.b64(box.nonce.copy),
      Crypto.b64(box.ciphertext.copy)
    )
  }
}

// The keys one producer or consumer holds, in one local file. Keep it off
// git and out of long-term backups: a surviving copy of a shredded key
// undoes the shredding. Written atomically, readable only by its owner, and
// safe to share between processes. Reads are cached and reloaded whenever
// the file changes, so a shred -- even by another process -- is seen at once.
// A capability: what it opens is tracked to it (see Plain).
final class KeyStore(val file: os.Path) extends caps.SharedCapability {
  private val lock_file = file / os.up / s".${file.last}.lock"

  @volatile private var cache: (Long, Map[String, String]) = (-1L, Map())

  private def load(): Map[String, String] =
    if (!os.exists(file)) Map()
    else {
      val stamp = os.mtime(file)
      val (cached_stamp, cached) = cache
      if (cached_stamp == stamp) cached
      else {
        val keys = upickle.default.read[Map[String, String]](os.read(file))
        cache = (stamp, keys)
        keys
      }
    }

  private def save(keys: Map[String, String]): Unit = {
    val tmp = file / os.up / s".${file.last}.tmp"
    os.write.over(tmp, upickle.default.write(keys), perms = "rw-------")
    os.move(tmp, file, replaceExisting = true, atomicMove = true)
    cache = (-1L, Map()) // reload next time
  }

  private def locked[A](f: => A): A = FileLock(lock_file)(f)

  // (a key that isn't one -- the file was damaged -- throws, unchecked, like
  // any other failure to read the file)
  def get(key: KeyId): Option[AesGcm] =
    load().get(key.value).map { secret =>
      try AesGcm(IArray.unsafeFromArray(Crypto.unb64(secret)))
      catch {
        case e: CryptoException =>
          throw IllegalStateException(
            s"${file.toString}: key ${key.value}: ${e.getMessage}"
          )
      }
    }

  def contains(key: KeyId): Boolean = load().contains(key.value)

  // A new key under `key` (which mustn't be in use yet).
  def create(key: KeyId): Unit throws SealedException = locked {
    val keys = load()
    if (keys.contains(key.value))
      throw SealedException(s"key ${key.value} already exists")
    save(keys + (key.value -> Crypto.b64(AesGcm.fresh().bytes.copy)))
  }

  // A key's secret, base64, to hand to a consumer (over a private channel).
  def secret_of(key: KeyId): Option[String] = load().get(key.value)

  // Adds a key from another store (its secret_of). Adding a key this store already
  // has, with the same secret, does nothing; with another secret, throws.
  def add(key: KeyId, secret: String): Unit throws SealedException = locked {
    val keys = load()
    keys.get(key.value) match {
      case Some(s) if s == secret => ()
      case Some(_)                =>
        throw SealedException(
          s"key ${key.value} already exists, with another secret"
        )
      case None =>
        try {
          val _ = AesGcm(IArray.unsafeFromArray(Crypto.unb64(secret)))
        } catch {
          case e: CryptoException =>
            throw SealedException(s"key ${key.value}: ${e.getMessage}")
        }
        save(keys + (key.value -> secret))
    }
  }

  // Forgets `key` for good: every value sealed with it stops opening here.
  def shred(key: KeyId): Unit = locked {
    save(load() - key.value)
  }
}
