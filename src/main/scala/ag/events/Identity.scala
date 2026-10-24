package ag.events

import upickle.default.ReadWriter

import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.{CREATE, WRITE}
import java.security.SecureRandom
import java.util.{Base64, UUID}
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

// ---- Identity, and crypto-shredding
//
// Every event refers to a student only by a StudentId: a random UUID, one per
// student per course. Anything that identifies them -- their CSID, their ssh
// key -- is kept in a Box sealed with that student's own key. Keys live
// outside the log (and outside git, whose history would keep them), in a
// KeyStore. Deleting a student's key ("shredding") leaves every event, view
// and checkpoint in place, but no Box of theirs opens any more.

@upickle.implicits.allowUnknownKeys(false)
final case class StudentId(uuid: String) derives ReadWriter {
  override def toString: String = uuid
}

object StudentId {
  def fresh(): StudentId = StudentId(UUID.randomUUID().toString)
}

// AES-256-GCM. Each sealed value gets a random 96-bit nonce, and is bound to
// `context` (associated data): it opens only with the same key *and* context.
object Crypto {
  private val random = SecureRandom()

  def new_key(): Array[Byte] = {
    val k = new Array[Byte](32)
    random.nextBytes(k)
    k
  }

  def seal(
      key: Array[Byte],
      context: String,
      plain: Array[Byte]
  ): (Array[Byte], Array[Byte]) = {
    val nonce = new Array[Byte](12)
    random.nextBytes(nonce)
    val cipher = Cipher.getInstance("AES/GCM/NoPadding").nn
    cipher.init(
      Cipher.ENCRYPT_MODE,
      SecretKeySpec(key, "AES"),
      GCMParameterSpec(128, nonce)
    )
    cipher.updateAAD(context.getBytes(StandardCharsets.UTF_8))
    (nonce, cipher.doFinal(plain).nn)
  }

  // throws if the key or context is wrong, or the data was tampered with
  def open(
      key: Array[Byte],
      context: String,
      nonce: Array[Byte],
      ciphertext: Array[Byte]
  ): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/GCM/NoPadding").nn
    cipher.init(
      Cipher.DECRYPT_MODE,
      SecretKeySpec(key, "AES"),
      GCMParameterSpec(128, nonce)
    )
    cipher.updateAAD(context.getBytes(StandardCharsets.UTF_8))
    cipher.doFinal(ciphertext).nn
  }

  def b64(bytes: Array[Byte]): String =
    Base64.getEncoder.nn.encodeToString(bytes).nn
  def unb64(s: String): Array[Byte] = Base64.getDecoder.nn.decode(s).nn
}

// A value sealed with `key`'s key: safe to write anywhere (events, views,
// checkpoints). It opens only while the key store still has that key, so
// shredding a key erases every Box sealed with it, wherever it's kept.
@upickle.implicits.allowUnknownKeys(false)
final case class Box[T](key: StudentId, nonce: String, data: String) {
  // Some(value) while the key exists; None once it's been shredded. A Box
  // that doesn't open with its key was tampered with: that's an error.
  def get(using keys: KeyStore, rw: ReadWriter[T]): Option[T] =
    keys
      .get(key)
      .map(secret =>
        upickle.default.read[T](
          Crypto.open(secret, key.uuid, Crypto.unb64(nonce), Crypto.unb64(data))
        )
      )
}

object Box {
  // the stored form is the same whatever T is
  given [T]: ReadWriter[Box[T]] = upickle.default.macroRW

  def seal[T: ReadWriter](value: T, key: StudentId)(using
      keys: KeyStore
  ): Box[T] = {
    val secret = keys
      .get(key)
      .getOrElse(throw IllegalStateException(s"no key for ${key.uuid}"))
    val (nonce, data) =
      Crypto.seal(secret, key.uuid, upickle.default.writeToByteArray(value))
    Box(key, Crypto.b64(nonce), Crypto.b64(data))
  }
}

// The students' keys, in one local file (keep it off git and out of long-term
// backups: a copy of a shredded key undoes the shredding). Written
// atomically, readable only by its owner, safe to share between processes.
// Reads are cached, and reloaded whenever the file changes, so a shred --
// even by another process -- is seen at once.
final class KeyStore(val file: os.Path) {
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

  // runs `f` holding both this process's and the OS's lock on the store
  private def locked[A](f: => A): A =
    KeyStore.lock_for(file).synchronized {
      os.makeDir.all(file / os.up)
      val channel = FileChannel.open(lock_file.toNIO, CREATE, WRITE).nn
      try {
        val lock = channel.lock().nn
        try f
        finally lock.release()
      } finally channel.close()
    }

  def get(student: StudentId): Option[Array[Byte]] =
    load().get(student.uuid).map(Crypto.unb64)

  // A new key for `student` (who mustn't have one yet).
  def create(student: StudentId): Unit = locked {
    val keys = load()
    if (keys.contains(student.uuid))
      throw IllegalStateException(s"${student.uuid} already has a key")
    save(keys + (student.uuid -> Crypto.b64(Crypto.new_key())))
  }

  // Forgets `student`'s key for good: every Box sealed with it stops opening.
  def shred(student: StudentId): Unit = locked {
    save(load() - student.uuid)
  }
}

object KeyStore {
  private val locks = java.util.concurrent.ConcurrentHashMap[os.Path, Object]()
  private def lock_for(file: os.Path): Object =
    locks.computeIfAbsent(file, _ => Object()).nn
}

// ---- Events about students (in the course's private log)

// A student's identity: their CSID, sealed with their key.
@upickle.implicits.allowUnknownKeys(false)
final case class StudentIdentity(csid: Box[String]) extends Event
    derives ReadWriter {
  def student: StudentId = csid.key
  def kind: EventKind[StudentIdentity] = StudentIdentity.kind
}

object StudentIdentity {
  val kind: EventKind[StudentIdentity] =
    EventKind[StudentIdentity]("student-identity")
}

// A student's (new or changed) public ssh key, sealed with their key.
@upickle.implicits.allowUnknownKeys(false)
final case class KeySubmitted(ssh_key: Box[String]) extends Event
    derives ReadWriter {
  def student: StudentId = ssh_key.key
  def kind: EventKind[KeySubmitted] = KeySubmitted.kind
}

object KeySubmitted {
  val kind: EventKind[KeySubmitted] = EventKind[KeySubmitted]("key-submitted")
}
