package ag.grader

import ag.common.ToRelPath
import upickle.default.{ReadWriter, readwriter}

// A course's and a project's names (as in courses.json and the repos'
// names), kept apart from other strings and from each other. Stored as
// plain strings.
opaque type CourseName = String

object CourseName {
  def apply(name: String): CourseName = name
  extension (c: CourseName) def value: String = c
  given ReadWriter[CourseName] = readwriter[String]
  given Ordering[CourseName] = Ordering.String
  given ToRelPath[CourseName] = c => os.RelPath(c)
}

opaque type ProjectName = String

object ProjectName {
  def apply(name: String): ProjectName = name
  extension (p: ProjectName) def value: String = p
  given ReadWriter[ProjectName] = readwriter[String]
  given Ordering[ProjectName] = Ordering.String
  given ToRelPath[ProjectName] = p => os.RelPath(p)
}
