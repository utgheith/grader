package ag.grader.cloudflare

import ag.grader.name.CourseName
import upickle.default.ReadWriter

import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets

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
) extends SiteHost derives ReadWriter {
  // where it serves (never its account or key): sites served elsewhere
  // are served again
  def name: String =
    s"cloudflare=${domain.getOrElse("pages.dev")},prefix=${project_prefix.getOrElse("")}"

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
  def publish(site_dir: os.Path, c: CourseName): String = {
    val project = project_of(c.value)
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
        ag.common.shout(s"creating Cloudflare Pages project $project")
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
        ag.common.shout(
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
