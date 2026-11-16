package ag.grader.name

import ag.common.ToRelPath
import upickle.default.{ReadWriter, readwriter}

// A project's name (as in courses.json and the repos' names), kept apart
// from other strings. Stored as a plain string.
opaque type ProjectName = String

object ProjectName {
  def apply(name: String): ProjectName = name
  extension (p: ProjectName) def value: String = p
  given ReadWriter[ProjectName] = readwriter[String]
  given Ordering[ProjectName] = Ordering.String
  given ToRelPath[ProjectName] = p => os.RelPath(p)
}
