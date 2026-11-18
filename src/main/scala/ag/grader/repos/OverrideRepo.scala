package ag.grader.repos

import ag.grader.targets.Project

import ag.grader.name.{CourseName, ProjectName}

import ag.grader.gitolite.{HandedOverRepo, Create, Publish}

import ag.common.ToRelPath
import ag.rules.{Source, Target, WithData}

// A project's override: its tests and Makefiles, which the staff keep (as
// Project.publish_override_repo), on the staff's override branch of the
// course's repo (see ClassRepo). The grader starts it from the project repo's tests and
// Makefiles (adding them to what's there, if the staff made it first), and
// marks it .initialized; then it's theirs.
final case class OverrideRepo(project: Project)
    extends HandedOverRepo[OverrideRepo] derives ToRelPath {
  val source: Source = project.source
  private def c: CourseName = project.c
  private def p: ProjectName = project.p
  def repo_name: String = ClassRepo.name_of(c)
  override def branch: String = ClassRepo.override_branch(p)
  def create: Option[Create] = Some(ClassRepo.create(c))
  val message = "update override"

  // handed over once the grader has started it, even if the staff made it
  override protected def is_handed_over(dir: os.Path, exists: Boolean) =
    os.exists(dir / OverrideRepo.marker)

  lazy val latest: Target[WithData[Publish]] =
    handed_over(
      _.track_all(
        (
          repo = ProjectRepo(project).inspect,
          ext = project.test_extensions
        )
      )
    ) { (in, producer, dir) =>
      for (m <- in.repo) {
        val from = producer.checkout(producer.commit(m).tree)
        // what isn't there yet
        def add(f: os.Path): Unit = {
          val to = dir / f.relativeTo(from)
          if (!os.exists(to, followLinks = false))
            os.copy(
              from = f,
              to = to,
              followLinks = false,
              createFolders = true,
              copyAttributes = true
            )
        }
        // its tests
        for {
          f <- os.list(from)
          if !f.last.startsWith(".")
          if in.ext.contains(f.ext)
        } add(f)
        // its Makefiles, anywhere
        os.walk
          .stream(from, followLinks = false)
          .foreach(f =>
            if (os.isFile(f) && OverrideRepo.makefiles.contains(f.last)) add(f)
          )
      }
      os.write(dir / OverrideRepo.marker, "")
      Seq()
    }
}

object OverrideRepo {
  private val marker = ".initialized"
  private val makefiles = Set("Makefile", "makefile")
}
