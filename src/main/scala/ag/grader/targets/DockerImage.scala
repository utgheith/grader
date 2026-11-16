package ag.grader.targets

import ag.grader.Docker

import ag.common.ToRelPath
import ag.rules.{DerivedScope, Source, Target}

// The image built from a docker file, named by the file's contents: shared
// by every project that uses the file. The file is in the grader's
// directory, not on the mirror, so it's read again every time.
final case class DockerImage(file: String, source: Source)
    extends DerivedScope[DockerImage] derives ToRelPath {
  // no dependencies: it runs every time
  lazy val image: Target[String] =
    target(EmptyTuple)((_, _) => Docker.image_name(os.pwd / os.RelPath(file)))
}
