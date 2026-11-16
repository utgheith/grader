package ag.grader.name

import ag.common.ToRelPath
import upickle.default.{ReadWriter, readwriter}

// A course's name (as in courses.json and the repos' names), kept apart
// from other strings. Stored as a plain string.
opaque type CourseName = String

object CourseName {
  def apply(name: String): CourseName = name
  extension (c: CourseName) def value: String = c
  given ReadWriter[CourseName] = readwriter[String]
  given Ordering[CourseName] = Ordering.String
  given ToRelPath[CourseName] = c => os.RelPath(c)
}
