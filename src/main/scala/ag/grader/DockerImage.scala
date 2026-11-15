package ag.grader

import ag.common.ToRelPath
import ag.grader.repos.{Env, Instance}
import ag.rules.Target

// The image built from a docker file, named by the file's contents: shared
// by every project that uses the file. The file is in the grader's
// directory, not on the mirror, so it's read again every time.
final case class DockerImage(file: String, env: Env)
    extends Instance[DockerImage] derives ToRelPath {
  // no dependencies: it runs every time
  lazy val image: Target[String] =
    target(EmptyTuple)((_, _) => Docker.image_name(os.pwd / os.RelPath(file)))
}
