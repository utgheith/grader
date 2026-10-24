package ag.ondisk

import upickle.default.ReadWriter

import java.lang.ref.WeakReference
import java.nio.file.AtomicMoveNotSupportedException
import java.security.MessageDigest

// A prototype, not yet used by r2: a reference to a value kept,
// serialized, in a content-addressed Store. The reference itself is small
// (just the content's hash), so it can live inside another serialized value;
// the value is read from disk only when asked for, and kept in memory only
// as long as something else holds on to it.

// Turns values into bytes and back. Content addressing works best when equal
// values always encode to the same bytes (e.g. SortedMap rather than Map);
// otherwise equal values just get different hashes.
trait Codec[T] {
  def encode(t: T): Array[Byte]
  def decode(bytes: Array[Byte]): T
}

object Codec {
  given json[T: ReadWriter]: Codec[T] with {
    def encode(t: T): Array[Byte] = upickle.default.writeToByteArray(t)
    def decode(bytes: Array[Byte]): T = upickle.default.read[T](bytes)
  }
}

// The content's SHA-256, in hex.
final case class Hash(hex: String) {
  override def toString: String = hex
}

object Hash {
  def of(bytes: Array[Byte]): Hash =
    Hash(
      MessageDigest
        .getInstance("SHA-256")
        .nn
        .digest(bytes)
        .nn
        .map(b => f"${b & 0xff}%02x")
        .mkString
    )

  given ReadWriter[Hash] =
    upickle.default.readwriter[String].bimap(_.hex, Hash(_))
}

// What can go wrong reading a blob back: it's gone, or its bytes no longer
// match their hash. Either way the value has to be produced again.
final class BlobUnavailable(val hash: Hash, why: String)
    extends RuntimeException(s"blob ${hash.hex} $why")

// Blobs filed by the hash of their contents, under <root>/ab/cdef...
final class Store(val root: os.Path) {
  def path(hash: Hash): os.Path = root / hash.hex.take(2) / hash.hex.drop(2)

  // Stores `bytes` (once: the same content is stored only once) and
  // returns their hash. Writes go to a temporary file first and are moved
  // into place, so a blob is never seen half-written.
  def put(bytes: Array[Byte]): Hash = {
    val hash = Hash.of(bytes)
    val dest = path(hash)
    if (!os.exists(dest)) {
      os.makeDir.all(dest / os.up)
      val tmp =
        os.temp(
          bytes,
          dir = dest / os.up,
          prefix = ".tmp-",
          deleteOnExit = false
        )
      // Another run may store the same content at the same time; whichever
      // rename lands last replaces the other's identical blob, harmlessly.
      try os.move(tmp, dest, replaceExisting = true, atomicMove = true)
      catch {
        case _: AtomicMoveNotSupportedException =>
          os.move(tmp, dest, replaceExisting = true)
      }
    }
    hash
  }

  // The bytes stored under `hash`, checked against it.
  def get(hash: Hash): Array[Byte] = {
    val p = path(hash)
    if (!os.isFile(p)) throw BlobUnavailable(hash, "is missing")
    val bytes = os.read.bytes(p)
    if (Hash.of(bytes) != hash) throw BlobUnavailable(hash, "is corrupted")
    bytes
  }
}

final class OnDisk[T] private (
    val hash: Hash,
    // the value, while something else still holds it; null if never loaded
    @volatile private var cached: WeakReference[T] | Null
) {

  // the value if it's still in memory
  private def held: T | Null = {
    val ref = cached
    if (ref == null) null else ref.get()
  }

  // Whether the value is still in memory (so `get` needn't read the disk).
  def in_memory: Boolean = held != null

  def get(using store: Store, codec: Codec[T]): T = {
    val value = held
    if (value != null) value
    else {
      val loaded = codec.decode(store.get(hash))
      cached = WeakReference(loaded)
      loaded
    }
  }

  override def equals(that: Any): Boolean = that match {
    case o: OnDisk[?] => o.hash == hash
    case _            => false
  }
  override def hashCode: Int = hash.hashCode
  override def toString: String = s"OnDisk(${hash.hex.take(12)})"
}

object OnDisk {
  // Stores `value` and refers to it; the reference starts out holding it.
  def apply[T](value: T)(using store: Store, codec: Codec[T]): OnDisk[T] =
    new OnDisk[T](store.put(codec.encode(value)), WeakReference(value))

  // A reference to content already in a store, not yet read.
  def of[T](hash: Hash): OnDisk[T] = new OnDisk[T](hash, null)

  // Serialized as just its hash.
  given [T]: ReadWriter[OnDisk[T]] =
    upickle.default.readwriter[Hash].bimap(_.hash, OnDisk.of[T](_))
}
