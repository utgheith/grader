package ag.grader.cloudflare

import ag.grader.targets.Course

import ag.grader.name.CourseName
import ag.grader.repos.SiteRepo

import ag.common.ToRelPath
import ag.rules.{DerivedScope, Source, Target}

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
final case class SiteDeployment(course: Course, host: SiteHost)
    extends DerivedScope[SiteDeployment] derives ToRelPath {
  val source: Source = course.source
  private def c: CourseName = course.c
  // the site's address
  lazy val deployed: Target[String] =
    target((pages = SiteRepo(course).pages)) { (producer, deps) =>
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
