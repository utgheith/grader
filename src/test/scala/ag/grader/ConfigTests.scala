package ag.grader

import munit.FunSuite
import upickle.default.read

class ConfigTests extends FunSuite {
  // every field must be present (null for none); see "every field is required"
  private def config(cloudflare: String): String = s"""{
    "gitolite": { "user": "git", "host": "example.com", "port": 22 },
    "dropbox_path": null, "site_base": null,
    "can_send_mail": null, "can_push_repo": null,
    "cloudflare": $cloudflare
  }"""
  private val json = config(
    """{ "enabled": true, "id": "acct", "api_key": "secret-token", "domain": null }"""
  )

  test("a cloudflare block parses") {
    val c = read[Config](json).cloudflare
    assertEquals(c, Some(CloudflareConfig(true, "acct", "secret-token", None)))
  }

  test("cloudflare is optional") {
    assertEquals(read[Config](config("null")).cloudflare, None)
  }

  test("a cloudflare domain parses, and is optional") {
    val with_domain = read[Config](
      json.replace(""""domain": null""", """"domain": "classprojects.net"""")
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

  test("every field is required, even optional ones") {
    val missing =
      """{ "gitolite": { "user": "git", "host": "example.com", "port": 22 } }"""
    assert(scala.util.Try(read[Config](missing)).isFailure)
  }
}
