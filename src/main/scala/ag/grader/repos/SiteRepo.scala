package ag.grader.repos

import ag.grader.targets.Course

import ag.grader.web.HtmlGen

import ag.grader.name.CourseName

import ag.grader.gitolite.{Groups, OwnedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.rules.{Source, Target, Tree, WithData}

// A course's report site (as r2's Course.publish_site): the report pages of its
// active projects, and a front page listing every project's page in the
// repo. Everyone can read it. A page is rewritten only when what it shows
// changes, and pages of projects no longer active stay, so it isn't
// replaced whole.
final case class SiteRepo(course: Course) extends OwnedRepo[SiteRepo]
    derives ToRelPath {
  val source: Source = course.source
  private def c: CourseName = course.c
  def repo_name: String = SiteRepo.name_of(c)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq()))
  val message = "update site"
  override protected def replaces_all: Boolean = false

  lazy val latest: Target[WithData[Publish]] =
    owned(_.track(course.site_inputs)) { (inputs, producer, dir) =>
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
          f.endsWith(".html") && f != "index.html" && !HtmlGen
            .is_milestone_page(f)
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
