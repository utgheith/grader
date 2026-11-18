package ag.grader.repos

import ag.grader.targets.Course

import ag.grader.web.HtmlGen

import ag.grader.Outbox

import ag.grader.name.{CourseName, ProjectName}

import ag.grader.gitolite.{OwnedRepo, Create, Publish}

import ag.common.ToRelPath
import ag.rules.{Source, Target, Tree, WithData}

// A course's report site (as r2's Course.publish_site): the report pages of its
// active projects, each project's results files (at
// ClassRepo.results_path, as r2's Project.publish_results), and a front
// page listing every project's page. On the grader's site branch of the
// course's repo (see ClassRepo); what it serves (on Cloudflare) is for
// everyone. A page is rewritten only when what it shows changes, and the
// pages and results of projects (and milestones) no longer published
// stay, so it isn't replaced whole. `grade` publishes it.
final case class SiteRepo(course: Course) extends OwnedRepo[SiteRepo]
    derives ToRelPath {
  val source: Source = course.source
  private def c: CourseName = course.c
  def repo_name: String = ClassRepo.name_of(c)
  override def branch: String = ClassRepo.site_branch
  def create: Option[Create] = Some(ClassRepo.create(c))
  val message = "update site"
  override protected def replaces_all: Boolean = false

  lazy val latest: Target[WithData[Publish]] =
    owned(_.track(course.site_inputs)) { (inputs, producer, dir) =>
      val now = ag.grader.Clock.now
      for {
        i <- inputs
        data <- HtmlGen.reports(i, now, why => producer.log(why + "\n"))
      } {
        val _ = HtmlGen.write_page(dir, data)
      }
      // the results of the milestones each project publishes now
      for {
        i <- inputs
        case (milestone, Right(Some(r)), true) <- i.results
      } os.write.over(
        dir / os.RelPath(
          ClassRepo.results_path(ProjectName(i.project_name), milestone)
        ),
        upickle.default.write(r, indent = 2),
        createFolders = true
      )
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

  // what's served: its files as published (on the mirror), so a new
  // commit with the same files isn't served again
  lazy val pages: Target[Tree] = target((m = inspect)) { (producer, deps) =>
    deps.m.fold(Tree(Outbox.empty_tree))(producer.commit(_).tree)
  }
}
