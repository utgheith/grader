package ag.grader.repos

import ag.common.ToRelPath
import ag.rules.{Scoped, Source}

// A target definition for one thing (a repo, a course, a project): a case
// class whose fields say which, with `derives ToRelPath`. Its targets are
// named by where they're defined (see Scoped) and then by all its fields
// (but `source`, which says what they work from, not which they are): the
// name is derived, so no field can be left out of it.
trait Instance[S](using key: ToRelPath[S]) extends Scoped { self: S =>
  val source: Source
  lazy val base: os.RelPath = key(self)
}
