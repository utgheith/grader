package ag.grader

import munit.FunSuite
import upickle.default.read

class ConfigTests extends FunSuite {
  private val json = """{
    "gitolite": { "user": "git", "host": "example.com", "port": 22 },
    "cloudflare": { "enabled": true, "id": "acct", "api_key": "secret-token" }
  }"""

  test("a cloudflare block parses") {
    val c = read[Config](json).cloudflare
    assertEquals(c, Some(CloudflareConfig(true, "acct", "secret-token")))
  }

  test("cloudflare is optional") {
    val c = read[Config](
      """{ "gitolite": { "user": "git", "host": "example.com", "port": 22 } }"""
    )
    assertEquals(c.cloudflare, None)
  }

  test("a cloudflare domain parses, and is optional") {
    val with_domain = read[Config](
      json.replace(
        """"api_key"""",
        """"domain": "classprojects.net", "api_key""""
      )
    )
    assertEquals(
      with_domain.cloudflare.flatMap(_.domain),
      Some("classprojects.net")
    )
    assertEquals(read[Config](json).cloudflare.flatMap(_.domain), None)
    assert(!with_domain.toString.contains("secret-token"))
  }

  test("toString doesn't reveal the api_key") {
    val c = read[Config](json)
    assert(!clue(c.toString).contains("secret-token"))
  }
}
