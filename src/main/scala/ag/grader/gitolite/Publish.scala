package ag.grader.gitolite

import ag.grader.Notice
import upickle.default.ReadWriter

// How the server makes a repo that isn't there: fork `fork_from`, and give
// `readers` and `writers` access (as RepoInfo.update)
@upickle.implicits.allowUnknownKeys(false)
final case class Create(
    fork_from: String,
    readers: Seq[String],
    writers: Seq[String]
) derives ReadWriter

// What a branch of a repo on the server (`branch`, master unless the repo
// is shared) should hold: its files (as the data of the target that worked
// it out), on top of `base`, the commit they were worked out from: the
// branch on the mirror, or, if it isn't there (`exists`), where it starts:
// the branch of repo `from` (for master, the repo `create` forks it from,
// which is where the fork starts; None: nothing). The Outbox records it,
// and `sync` makes the repo if needed (it can't without `create`) and
// pushes it, then sends its `notices`.
@upickle.implicits.allowUnknownKeys(false)
final case class Publish(
    repo: String,
    exists: Boolean,
    base: Option[String],
    message: String,
    create: Option[Create],
    notices: Seq[Notice] = Seq(),
    branch: String = "master",
    from: Option[String] = None
) derives ReadWriter {
  // its name in the outbox, and in what's said about it: the repo, and the
  // branch if it isn't master
  def key: String = if (branch == "master") repo else s"$repo/$branch"
}
