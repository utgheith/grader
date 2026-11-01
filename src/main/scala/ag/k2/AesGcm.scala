package ag.k2

import language.experimental.captureChecking
import language.experimental.saferExceptions

import java.security.{GeneralSecurityException, SecureRandom}
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import scala.annotation.nowarn

// ---- AES-256-GCM, with a Scala API
//
// javax.crypto.Cipher, for the one thing ag.k2 does with it: authenticated
// encryption with associated data. Cipher is stateful (init, then update,
// then doFinal; one use at a time), takes and returns mutable arrays, and
// signals failure with Java checked exceptions that Scala doesn't see. Here
// a key is an immutable value, safe to share between threads; sealing and
// opening are plain functions on it; bytes are IArrays; and the failure a
// caller has to handle is a checked exception.

// Something that isn't a key, or a box that doesn't open: the wrong key,
// the wrong associated data, or a box that was tampered with or truncated.
// (Which of these it was can't be told apart, by design.)
final class CryptoException(message: String) extends Exception(message)

// A mutable copy, for Java APIs. (IArray's own toArray is deprecated.)
extension (a: IArray[Byte])
  private[k2] def copy: Array[Byte] = IArray.genericWrapArray(a).toArray

// What sealing makes: a fresh random nonce, and the ciphertext (with its
// authentication tag). Neither is secret.
final class Box(val nonce: IArray[Byte], val ciphertext: IArray[Byte])

// A 256-bit AES key, used in GCM mode with 96-bit nonces and 128-bit tags.
final class AesGcm private (val bytes: IArray[Byte]) {
  private val spec = SecretKeySpec(bytes.copy, "AES")

  private def cipher(mode: Int, nonce: IArray[Byte]): Cipher = {
    val c = Cipher.getInstance("AES/GCM/NoPadding").nn
    c.init(mode, spec, GCMParameterSpec(AesGcm.tag_bits, nonce.copy))
    c
  }

  // `plain`, encrypted, and bound to `associated` (which isn't encrypted,
  // but has to be the same to open it). Doesn't fail: a valid key and a
  // fresh nonce are all AES-GCM needs.
  def seal(plain: IArray[Byte], associated: IArray[Byte]): Box = {
    val nonce = AesGcm.random_bytes(AesGcm.nonce_bytes)
    val c = cipher(Cipher.ENCRYPT_MODE, nonce)
    c.updateAAD(associated.copy)
    Box(nonce, IArray.unsafeFromArray(c.doFinal(plain.copy).nn))
  }

  // What `box` sealed, if it was sealed with this key and `associated`.
  def open(
      box: Box,
      associated: IArray[Byte]
  ): IArray[Byte] throws CryptoException = {
    if (box.nonce.length != AesGcm.nonce_bytes)
      throw CryptoException(
        s"a ${box.nonce.length}-byte nonce; AES-GCM here uses ${AesGcm.nonce_bytes}"
      )
    // (the compiler makes a CanThrow for this try that nothing uses, and
    // then warns about it)
    @nowarn("msg=unused local definition")
    val plain =
      try {
        val c = cipher(Cipher.DECRYPT_MODE, box.nonce)
        c.updateAAD(associated.copy)
        c.doFinal(box.ciphertext.copy).nn
      } catch {
        case e: GeneralSecurityException =>
          throw CryptoException(s"doesn't open: ${e.toString}")
      }
    IArray.unsafeFromArray(plain)
  }

  // (so that logging one shows nothing)
  override def toString: String = "AesGcm(...)"
}

object AesGcm {
  val key_bytes = 32
  val nonce_bytes = 12
  val tag_bits = 128

  private val random = SecureRandom()

  private def random_bytes(n: Int): IArray[Byte] = {
    val a = new Array[Byte](n)
    random.nextBytes(a)
    IArray.unsafeFromArray(a)
  }

  // a new random key
  def fresh(): AesGcm = new AesGcm(random_bytes(key_bytes))

  // the key that `bytes` are
  def apply(bytes: IArray[Byte]): AesGcm throws CryptoException =
    if (bytes.length != key_bytes)
      throw CryptoException(
        s"a ${bytes.length * 8}-bit key; AES-256 needs ${key_bytes * 8}"
      )
    else new AesGcm(bytes)
}
