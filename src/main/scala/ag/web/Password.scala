package ag.web

import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

// Salted PBKDF2 password hashes, as kept in config.json:
// pbkdf2-sha256$<iterations>$<salt>$<hash>, the last two in base64.
object Password {
  private val iterations = 600_000
  private val random = new SecureRandom()

  private def derive(
      password: Array[Char],
      salt: Array[Byte],
      n: Int
  ): Array[Byte] =
    SecretKeyFactory
      .getInstance("PBKDF2WithHmacSHA256")
      .nn
      .generateSecret(new PBEKeySpec(password, salt, n, 256))
      .nn
      .getEncoded
      .nn

  def hash(password: Array[Char]): String = {
    val salt = new Array[Byte](16)
    random.nextBytes(salt)
    val b64 = Base64.getEncoder.nn
    s"pbkdf2-sha256$$$iterations$$${b64.encodeToString(salt)}$$${b64
        .encodeToString(derive(password, salt, iterations))}"
  }

  // false for a wrong password, or a hash that isn't in the format above
  def check(password: Array[Char], hashed: String): Boolean =
    hashed.split('$') match {
      case Array("pbkdf2-sha256", n, salt, expected) =>
        try {
          val b64 = Base64.getDecoder.nn
          MessageDigest.isEqual(
            derive(password, b64.decode(salt), n.toInt),
            b64.decode(expected)
          )
        } catch { case _: IllegalArgumentException => false }
      case _ => false
    }
}
