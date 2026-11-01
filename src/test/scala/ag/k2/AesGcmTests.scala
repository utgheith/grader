package ag.k2

import scala.unsafeExceptions.canThrowAny
import munit.FunSuite

class AesGcmTests extends FunSuite {
  private def bytes(s: String): IArray[Byte] = IArray.unsafeFromArray(s.getBytes)
  private def text(b: IArray[Byte]): String = String(b.copy)

  test("what's sealed opens, with the same key and associated data") {
    val aes = AesGcm.fresh()
    val box = aes.seal(bytes("ab1234"), bytes("k1"))
    assertEquals(text(aes.open(box, bytes("k1"))), "ab1234")
    // and a key rebuilt from its bytes is the same key
    assertEquals(text(AesGcm(aes.bytes).open(box, bytes("k1"))), "ab1234")
  }

  test("each seal has a fresh nonce") {
    val aes = AesGcm.fresh()
    val a = aes.seal(bytes("x"), bytes("k"))
    val b = aes.seal(bytes("x"), bytes("k"))
    assertNotEquals(a.nonce.toSeq, b.nonce.toSeq)
    assertNotEquals(a.ciphertext.toSeq, b.ciphertext.toSeq)
  }

  test("it doesn't open with another key, other associated data, or changes") {
    val aes = AesGcm.fresh()
    val box = aes.seal(bytes("ab1234"), bytes("k1"))
    val flipped = IArray.unsafeFromArray(box.ciphertext.copy.updated(0, (box.ciphertext(0) ^ 1).toByte))
    val _ = intercept[CryptoException](AesGcm.fresh().open(box, bytes("k1")))
    val _ = intercept[CryptoException](aes.open(box, bytes("k2")))
    val _ = intercept[CryptoException](aes.open(Box(box.nonce, flipped), bytes("k1")))
    val _ = intercept[CryptoException](aes.open(Box(box.nonce, box.ciphertext.take(5)), bytes("k1")))
    val _ = intercept[CryptoException](aes.open(Box(box.nonce.take(8), box.ciphertext), bytes("k1")))
    val _ = intercept[CryptoException](aes.open(Box(IArray.empty, box.ciphertext), bytes("k1")))
  }

  test("a key is 256 bits") {
    val _ = intercept[CryptoException](AesGcm(IArray.fill(16)(0.toByte)))
    assertEquals(AesGcm.fresh().bytes.length, 32)
  }

  test("a key doesn't show itself") {
    assertEquals(AesGcm.fresh().toString, "AesGcm(...)")
  }
}
