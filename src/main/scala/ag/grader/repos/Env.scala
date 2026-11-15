package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.Dropbox
import ag.rules.Source

// What the target definitions work from: the local mirror and the dropbox.
// Passed explicitly to every definition's targets; it isn't part of a
// target's name, so one State works with one Env.
final class Env(val source: Source, val dropbox: Dropbox = Dropbox(None))

object Env {
  // never part of a target's name (see Instance)
  given ToRelPath[Env] = ToRelPath.Omitted()
}
