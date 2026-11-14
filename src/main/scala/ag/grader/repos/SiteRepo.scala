package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CourseName, Create, HtmlGen, Publish}
import ag.r3.{Target, Tree, WithData}

// A course's report site (as r2's Course.publish_site): the report pages of its
// active projects, and a front page listing every project's page in the
// repo. Everyone can read it. A page is rewritten only when what it shows
// changes, and pages of projects no longer active stay, so it isn't
// replaced whole.
final case class SiteRepo(c: CourseName, env: Env) extends OwnedRepo[SiteRepo]
    derives ToRelPath {
  def repo_name: String = SiteRepo.name_of(c)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq()))
  val message = "update site"
  override protected def replaces_all: Boolean = false

  lazy val latest: Target[WithData[Publish]] =
    owned(_.track(env.grading.site_inputs(c))) { (inputs, producer, dir) =>
      val now = java.time.LocalDateTime.now().nn
      for {
        i <- inputs
        data <- HtmlGen.reports(i, now, why => producer.log(why + "\n"))
      } {
        val _ = HtmlGen.write_page(dir, data)
      }
      val pages = os
        .list(dir)
        .map(_.last)
        .filter(f =>
          f.endsWith(".html") && f != "index.html" && !HtmlGen.is_phase_page(f)
        )
      os.write.over(dir / "index.html", HtmlGen.render_index(c.value, pages))
      Seq()
    }

  // just its files: what's served, so a new base with the same files
  // (after a sync) isn't served again
  lazy val pages: Target[Tree] = target((l = latest))((_, deps) => deps.l.data)
}

object SiteRepo {
  def name_of(c: CourseName): String = s"${c.value}__site"
}

// Where courses' sites are served (e.g. a CloudflareConfig).
trait SiteHost {
  // which host it is, as part of a target's name: never a secret
  def name: String

  // serves `dir` as course `c`'s whole site; its address
  def publish(dir: os.Path, c: CourseName): String
}

object SiteHost {
  given ToRelPath[SiteHost] = h => os.RelPath(h.name)
}

// A course's site, served by `host`: its latest pages, as the outbox has
// them (sync pushes them to the site repo), whether or not they've been
// synced. Served again only when they change.
final case class SiteDeployment(c: CourseName, host: SiteHost, env: Env)
    extends Instance[SiteDeployment] derives ToRelPath {
  // the site's address
  lazy val deployed: Target[String] =
    target((pages = SiteRepo(c, env).pages)) { (producer, deps) =>
      os.makeDir.all(producer.state.scratch_root)
      // the pages less the repo's dot files; the host's tools run in the
      // site's parent (wrangler's .wrangler cache goes there)
      val work =
        os.temp.dir(dir = producer.state.scratch_root, deleteOnExit = false)
      try {
        val site = work / "site"
        os.makeDir(site)
        producer.copy_to(deps.pages, site)
        for (f <- os.list(site) if f.last.startsWith(".")) os.remove.all(f)
        producer.log(s"serving ${SiteRepo.name_of(c)} on ${host.name}\n")
        host.publish(site, c)
      } finally os.remove.all(work)
    }
}
