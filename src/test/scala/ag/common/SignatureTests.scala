package ag.common

import munit.FunSuite
import upickle.default.{read, write}

class SignatureTests extends FunSuite {
  test("signature from bytes") {
    val bytes = Array[Byte](0x01, 0x02, 0x03, 0xff.toByte)
    val sig = Signature(bytes)
    assert(clue(sig.it) == "010203ff")
  }

  test("signature serialization") {
    val sig = Signature("010203ff")
    val json = write(sig)
    assert(clue(json) == "\"010203ff\"")
    val deserialized = read[Signature](json)
    assert(clue(deserialized) == sig)
  }
}
