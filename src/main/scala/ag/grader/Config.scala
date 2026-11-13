package ag.grader

import ag.common.{down, given_ReadWriter_RelPath}
import ag.r2.{Scope, Target}
import ag.rules.{recorded, run}
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
    // a zone on this Cloudflare account, or a name in one (e.g.
    // test.classprojects.net); each course's site is served at
    // https://<course project>.<domain>
    domain: Option[String],
    // put before each course's project name, e.g. "test-": a test setup's
    // projects are then its own, never the live ones
    project_prefix: Option[String] = None
) derives ReadWriter {
  // keep the api_key out of logs and pprint output
  override def toString: String =
    s"CloudflareConfig(enabled = $enabled, id = $id, api_key = <redacted>, domain = ${domain.toString}, project_prefix = ${project_prefix.toString})"

  // Each course is its own Pages project: a deploy replaces a project's
  // whole site, so courses sharing one would erase each other. Project
  // names allow only lowercase letters, digits and dashes.
  def project_of(course_name: String): String =
    project_prefix.getOrElse("") +
      course_name.toLowerCase.nn.map(c => if (c.isLetterOrDigit) c else '-')

  // Deploys `site_dir` as the course's whole site, and serves it at
  // <project>.<domain> if there's a domain. The site's address.
  def publish(site_dir: os.Path, course_name: String): String = {
    val project = project_of(course_name)
    deploy(site_dir, project)
    val host = domain match {
      case Some(d) =>
        val host = s"$project.$d"
        ensure_domain(project, host, d)
        host
      case None => pages_host(project)
    }
    s"https://$host"
  }

  // ---- Cloudflare's REST API, for what wrangler doesn't do (custom domains)

  private lazy val http = HttpClient.newHttpClient().nn

  // The call's `result`, or an exception carrying Cloudflare's errors (which
  // name the problem, e.g. a missing permission) -- never the token.
  private def api(
      method: String,
      path: String,
      body: Option[ujson.Value] = None
  ): ujson.Value =
    api_result(method, path, body).fold(
      (status, errors) =>
        throw new IllegalStateException(
          s"Cloudflare API $method $path failed (HTTP $status): $errors"
        ),
      identity
    )

  // The call's `result`, or its HTTP status and Cloudflare's errors
  private def api_result(
      method: String,
      path: String,
      body: Option[ujson.Value] = None
  ): Either[(Int, String), ujson.Value] = {
    ag.rules.refuse_if_read_only(s"call Cloudflare ($method $path)")
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
        Right(j("result"))
      case _ =>
        val errors = json
          .flatMap(_.obj.get("errors"))
          .map(ujson.write(_))
          .getOrElse(response.body().nn.take(500))
        Left((response.statusCode(), errors))
    }
  }

  // Makes Pages project `project` (deployed from branch main) if it isn't
  // there. Through the API, not wrangler: wrangler's `pages project create`
  // now hands off to Workers, which fails for a site like ours.
  def ensure_project(project: String): Unit = {
    val projects = s"/accounts/$id/pages/projects"
    api_result("GET", s"$projects/${query(project)}") match {
      case Right(_)       => ()
      case Left((404, _)) =>
        ag.r2.shout(s"creating Cloudflare Pages project $project")
        val _ = api(
          "POST",
          projects,
          Some(ujson.Obj("name" -> project, "production_branch" -> "main"))
        )
      case Left((status, errors)) =>
        throw new IllegalStateException(
          s"can't tell if Cloudflare Pages project $project exists (HTTP $status): $errors"
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
    // so add it if it isn't there. `zone` may be a name in a zone (e.g.
    // test.classprojects.net in classprojects.net): the nearest zone up.
    val zone_id = zone
      .split('.')
      .tails
      .filter(_.length >= 2)
      .map(_.mkString("."))
      .flatMap(z =>
        api("GET", s"/zones?name=${query(z)}").arr.headOption.map(_("id").str)
      )
      .nextOption()
      .getOrElse(
        throw new IllegalStateException(
          s"no zone for $zone: is it on this account, and can the API token read it?"
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

  // The wrangler that's known to work: pinned, so a new one can't change
  // how sites are deployed (4.152's `pages project create` hands off to
  // Workers; a later `pages deploy` might too)
  private val wrangler_version = "4.152.0"

  // Credentials go only to wrangler's process. CI=true keeps wrangler from
  // ever stopping to prompt.
  private def wrangler(cwd: os.Path, check: Boolean, quiet: Boolean)(
      args: os.Shellable*
  ): os.CommandResult = {
    ag.rules.refuse_if_read_only("run wrangler")
    val output = if (quiet) os.Pipe else os.Inherit
    val p = os.proc(
      os.Shellable(Seq("npx", "--yes", s"wrangler@$wrangler_version")) +: args*
    )
    recorded(p, cwd) {
      p.call(
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
  }

  // Publish `site_dir` as the whole of Pages project `project`, creating the
  // project on first use. Anything not in `site_dir` disappears from the site.
  // Wrangler runs in `site_dir`'s parent, where it leaves its .wrangler cache.
  def deploy(site_dir: os.Path, project: String): Unit = {
    val cwd = site_dir / os.up
    ensure_project(project)
    // Cloudflare's API fails now and then (e.g. 503 "no healthy upstream"):
    // tried again after a while, a few times
    def attempt(n: Int): Unit = {
      val r = wrangler(cwd, check = false, quiet = false)(
        "pages",
        "deploy",
        site_dir,
        s"--project-name=$project",
        "--branch=main", // the production branch: deploy straight to live
        "--commit-dirty=true"
      )
      if (r.exitCode != 0) {
        if (n >= deploy_tries)
          throw new IllegalStateException(
            s"deploying $project failed $n times (wrangler exit ${r.exitCode})"
          )
        val wait = 30L * n
        ag.r2.shout(
          s"deploying $project failed (wrangler exit ${r.exitCode}); trying again in ${wait}s"
        )
        Thread.sleep(wait * 1000)
        attempt(n + 1)
      }
    }
    attempt(1)
  }

  private val deploy_tries = 3
}

@upickle.implicits.allowUnknownKeys(false)
case class Config(
    gitolite: RemoteServer,
    dropbox_path: Option[os.RelPath],
    site_base: Option[String],
    can_send_mail: Option[Boolean],
    can_push_repo: Option[Boolean],
    cloudflare: Option[CloudflareConfig],
    // serve each run's graph live; see ag.r2.WebConfig
    web: Option[ag.r2.WebConfig] = None
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

  // `cloudflare` and `web` are dropped here: a target's value is saved to
  // workspace/targets/.../saved.json, and their secrets shouldn't be.
  private lazy val config: Target[Config] = target(EmptyTuple) { (_, _) =>
    // logger.info(s"loading config from $config_file")
    load().copy(cloudflare = None, web = None)
  }

  // Read straight from config.json, never through a target, like cloudflare.
  def web: Option[ag.r2.WebConfig] = load().web

  // The git server, read straight from config.json: for what runs outside
  // any State (LocalMirror).
  def gitolite_server: RemoteServer = load().gitolite

  // whether pushing to the git server is allowed, read straight from
  // config.json: for what runs outside any State (Outbox.sync)
  def pushes_allowed: Boolean = load().can_push_repo.getOrElse(false)

  // whether sending mail is allowed, the same way
  def mail_allowed: Boolean = load().can_send_mail.getOrElse(false)

  // where the students drop their keys (dropbox_path, under the home
  // directory), read straight from config.json; None if it has none
  def dropbox_root: Option[os.Path] = load().dropbox_path.map(os.home / _)

  // Read straight from config.json on every call, never through a target,
  // so the api_key is never cached under workspace/.
  def cloudflare: Option[CloudflareConfig] = load().cloudflare

  // Safe to cache: just the flag. No dependencies, so it's re-read from
  // config.json on every run, and targets that depend on it notice changes.
  lazy val cloudflare_enabled: Target[Boolean] = target(EmptyTuple) { (_, _) =>
    load().cloudflare.exists(_.enabled)
  }

  // Likewise safe to cache, and re-read on every run.
  lazy val cloudflare_domain: Target[Option[String]] = target(EmptyTuple) {
    (_, _) =>
      load().cloudflare.flatMap(_.domain)
  }

  lazy val cloudflare_project_prefix: Target[Option[String]] =
    target(EmptyTuple) { (_, _) =>
      load().cloudflare.flatMap(_.project_prefix)
    }

  lazy val gitolite: Target[RemoteServer] =
    target((c = config))((_, deps) => deps.c.gitolite)

  lazy val dropbox_path: Target[Option[RelPath]] =
    target((c = config))((_, deps) => deps.c.dropbox_path)

  lazy val site_base: Target[Option[String]] =
    target((c = config))((_, deps) => deps.c.site_base)

  // TODO: peek
  lazy val can_send_mail: Target[Boolean] = target((config = config)) {
    (_, deps) =>
      deps.config.can_send_mail.getOrElse(false)
  }.peek

  lazy val can_push_repo: Target[Boolean] =
    target((c = config))((_, deps) => deps.c.can_push_repo.getOrElse(false))
}
