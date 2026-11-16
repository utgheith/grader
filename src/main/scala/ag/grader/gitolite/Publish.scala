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

// What a repo on the server should hold: its files (as the data of the
// target that worked it out), on top of `base`, the commit they were worked
// out from: the repo's master on the mirror, or, if there's no such repo
// (`exists`), the master of the repo `create` forks it from, which is where
// the fork starts (None: no master). The Outbox records it, and `sync`
// makes the repo if needed (it can't without `create`) and pushes it, then
// sends its `notices`.
@upickle.implicits.allowUnknownKeys(false)
final case class Publish(
    repo: String,
    exists: Boolean,
    base: Option[String],
    message: String,
    create: Option[Create],
    notices: Seq[Notice] = Seq()
) derives ReadWriter
