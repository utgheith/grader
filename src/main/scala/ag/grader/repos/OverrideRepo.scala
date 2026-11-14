package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{CourseName, Create, ProjectName, Publish}
import ag.r3.{Target, WithData}

// A project's override repo: its tests and Makefiles, which the staff keep
// (as Project.publish_override_repo). Everyone can read it, the staff
// write it. The grader starts it from the project repo's tests and
// Makefiles (adding them to what's there, if the staff made it first), and
// marks it .initialized; then it's theirs.
final case class OverrideRepo(c: CourseName, p: ProjectName, env: Env)
    extends HandedOverRepo[OverrideRepo] derives ToRelPath {
  def repo_name: String = OverrideRepo.name_of(c, p)
  def create: Option[Create] =
    Some(Create("empty", Seq(Groups.everyone), Seq(Groups.staff(c))))
  val message = "update override"

  // handed over once the grader has started it, even if the staff made it
  override protected def is_handed_over(dir: os.Path, exists: Boolean) =
    os.exists(dir / OverrideRepo.marker)

  lazy val latest: Target[WithData[Publish]] =
    handed_over(
      _.track_all(
        (
          project = ProjectRepo(c, p, env).inspect,
          ext = ProjectConfig(c, p, env).test_extensions
        )
      )
    ) { (in, producer, dir) =>
      for (m <- in.project) {
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
  def name_of(c: CourseName, p: ProjectName): String =
    s"${ProjectRepo.name_of(c, p)}__override"

  private val marker = ".initialized"
  private val makefiles = Set("Makefile", "makefile")
}
