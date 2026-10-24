package ag.grader

import ag.common.{down, given_ReadWriter_RelPath}
import ag.r2.{Scope, Target}
import ag.rules.run
import os.RelPath
import upickle.default.{ReadWriter, read}

import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.util.concurrent.Semaphore
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

@upickle.implicits.allowUnknownKeys(false)
case class RemoteServer(user: String, host: String, port: Int)
    derives ReadWriter {
  lazy val ssh_uri: String = s"ssh://$user@$host:$port"
  def git_uri(repo: String): String = s"$ssh_uri/$repo"

  class SshProc(parts: os.Shellable*) {
    def run(
        cwd: os.Path,
        check: Boolean = true,
        stdin: os.ProcessInput = ""
    ): (Int, os.Path, Option[os.Path]) = {
      RemoteServer.ssh_locks.getOrElseUpdate(host, Semaphore(4)).down(1) {
        os.proc(parts*).run(cwd = cwd, check = check, stdin = stdin)
      }
    }

    def check(
        cwd: os.Path,
        stdin: os.ProcessInput = ""
    ): Unit = {
      val _ = run(cwd, true, stdin)
    }

    def lines(cwd: os.Path, stdin: os.ProcessInput = ""): Seq[String] =
      os.read.lines(run(cwd, true, stdin)._2)
  }

  def ssh(parts: os.Shellable*): SshProc = SshProc(
    os.Shellable(Seq("ssh", ssh_uri)) +: parts*
  )

  // def git_clone(repo: String, dir: os.Path): SshProc =
  //  SshProc("git", "clone", git_uri(repo), dir)

  // def git_fork(src: String, dest: String): SshProc = SshProc("fork", src, dest)

}

object RemoteServer {
  val ssh_locks: TrieMap[String, Semaphore] = TrieMap[String, Semaphore]()

}

@upickle.implicits.allowUnknownKeys(false)
case class CloudflareConfig(
    enabled: Boolean,
    id: String,
    api_key: String,
    // a zone on this Cloudflare account; each course's site is served at
    // https://<course project>.<domain>
    domain: Option[String]
) derives ReadWriter {
  // keep the api_key out of logs and pprint output
  override def toString: String =
    s"CloudflareConfig(enabled = $enabled, id = $id, api_key = <redacted>, domain = ${domain.toString})"

  // ---- Cloudflare's REST API, for what wrangler doesn't do (custom domains)

  private lazy val http = HttpClient.newHttpClient().nn

  // The call's `result`, or an exception carrying Cloudflare's errors (which
  // name the problem, e.g. a missing permission) -- never the token.
  private def api(
      method: String,
      path: String,
      body: Option[ujson.Value] = None
  ): ujson.Value = {
    val request = HttpRequest
      .newBuilder(URI.create(s"https://api.cloudflare.com/client/v4$path"))
      .nn
      .header("Authorization", s"Bearer $api_key")
      .nn
      .header("Content-Type", "application/json")
      .nn
      .method(
        method,
        body.fold(HttpRequest.BodyPublishers.noBody().nn)(b =>
          HttpRequest.BodyPublishers.ofString(ujson.write(b)).nn
        )
      )
      .nn
      .build()
      .nn
    val response = http.send(request, HttpResponse.BodyHandlers.ofString()).nn
    val json = scala.util.Try(ujson.read(response.body().nn)).toOption
    json match {
      case Some(j) if j.obj.get("success").exists(_.boolOpt.contains(true)) =>
        j("result")
      case _ =>
        val errors = json
          .flatMap(_.obj.get("errors"))
          .map(ujson.write(_))
          .getOrElse(response.body().nn.take(500))
        throw new IllegalStateException(
          s"Cloudflare API $method $path failed (HTTP ${response.statusCode()}): $errors"
        )
    }
  }

  private def query(s: String): String =
    URLEncoder.encode(s, StandardCharsets.UTF_8).nn

  // The project's own address: usually <project>.pages.dev, but Cloudflare
  // picks a variant when that name is taken.
  def pages_host(project: String): String =
    api("GET", s"/accounts/$id/pages/projects/${query(project)}")(
      "subdomain"
    ).str

  // Serve `project` at `host` (a name in zone `zone`): attach it to the
  // project, and make sure DNS points at the project. Safe to repeat: does
  // nothing once both are in place.
  def ensure_domain(project: String, host: String, zone: String): Unit = {
    val domains = s"/accounts/$id/pages/projects/${query(project)}/domains"
    if (!api("GET", domains).arr.exists(_("name").str == host)) {
      val _ = api("POST", domains, Some(ujson.Obj("name" -> host)))
    }

    // The dashboard adds this record itself; the API isn't documented to,
    // so add it if it isn't there.
    val zone_id = api("GET", s"/zones?name=${query(zone)}").arr.headOption
      .map(_("id").str)
      .getOrElse(
        throw new IllegalStateException(
          s"zone $zone not found: is it on this account, and can the API token read it?"
        )
      )
    val records = s"/zones/$zone_id/dns_records"
    if (api("GET", s"$records?name=${query(host)}").arr.isEmpty) {
      val _ = api(
        "POST",
        records,
        Some(
          ujson.Obj(
            "type" -> "CNAME",
            "name" -> host,
            "content" -> pages_host(project),
            "proxied" -> true,
            "ttl" -> 1 // automatic
          )
        )
      )
    }
  }

  // Credentials go only to wrangler's process. CI=true keeps wrangler from
  // ever stopping to prompt.
  private def wrangler(cwd: os.Path, check: Boolean, quiet: Boolean)(
      args: os.Shellable*
  ): os.CommandResult = {
    val output = if (quiet) os.Pipe else os.Inherit
    os.proc(os.Shellable(Seq("npx", "--yes", "wrangler")) +: args*)
      .call(
        cwd = cwd,
        env = Map(
          "CLOUDFLARE_API_TOKEN" -> api_key,
          "CLOUDFLARE_ACCOUNT_ID" -> id,
          "CI" -> "true"
        ),
        check = check,
        stdout = output,
        stderr = output
      )
  }

  // Publish `site_dir` as the whole of Pages project `project`, creating the
  // project on first use. Anything not in `site_dir` disappears from the site.
  // Wrangler runs in `site_dir`'s parent, where it leaves its .wrangler cache.
  def deploy(site_dir: os.Path, project: String): Unit = {
    val cwd = site_dir / os.up
    // fails harmlessly once the project exists; a real problem (e.g. a bad
    // token) shows up in the deploy below
    val _ = wrangler(cwd, check = false, quiet = true)(
      "pages",
      "project",
      "create",
      project,
      "--production-branch=main"
    )
    val _ = wrangler(cwd, check = true, quiet = false)(
      "pages",
      "deploy",
      site_dir,
      s"--project-name=$project",
      "--branch=main", // the production branch: deploy straight to live
      "--commit-dirty=true"
    )
  }
}

@upickle.implicits.allowUnknownKeys(false)
case class Config(
    gitolite: RemoteServer,
    dropbox_path: Option[os.RelPath],
    site_base: Option[String],
    can_send_mail: Option[Boolean],
    can_push_repo: Option[Boolean],
    cloudflare: Option[CloudflareConfig]
) derives ReadWriter {}

object Config extends Scope(".") {

  private def config_file: os.Path = {
    @tailrec
    def find(d: os.Path): os.Path = {
      val t = d / "config.json"
      if os.isFile(t) then t else find(d / os.up)
    }
    find(os.pwd)
  }

  private def load(): Config = read[Config](os.read(config_file))

  // `cloudflare` is dropped here: a target's value is saved to
  // workspace/targets/.../saved.json, and the api_key shouldn't be.
  private lazy val config: Target[Config] = target() {
    // logger.info(s"loading config from $config_file")
    load().copy(cloudflare = None)
  }

  // Read straight from config.json on every call, never through a target,
  // so the api_key is never cached under workspace/.
  def cloudflare: Option[CloudflareConfig] = load().cloudflare

  // Safe to cache: just the flag. No dependencies, so it's re-read from
  // config.json on every run, and targets that depend on it notice changes.
  lazy val cloudflare_enabled: Target[Boolean] = target() {
    load().cloudflare.exists(_.enabled)
  }

  // Likewise safe to cache, and re-read on every run.
  lazy val cloudflare_domain: Target[Option[String]] = target() {
    load().cloudflare.flatMap(_.domain)
  }

  lazy val gitolite: Target[RemoteServer] = target(config)(_.gitolite)

  lazy val dropbox_path: Target[Option[RelPath]] =
    target(config)(_.dropbox_path)

  lazy val site_base: Target[Option[String]] = target(config)(_.site_base)

  // TODO: peek
  lazy val can_send_mail: Target[Boolean] = target(config) { config =>
    config.can_send_mail.getOrElse(false)
  }.peek

  lazy val can_push_repo: Target[Boolean] =
    target(config)(_.can_push_repo.getOrElse(false))
}
