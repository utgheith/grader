package ag.grader

import ag.grader.gitolite.{Publish, LocalMirror}

import ag.rules.WithData

import scala.util.Try

// What the grader would push to the git server, kept as local commits until
// they're pushed (as they're recorded: see main.misc.Publishing; or by `sync`): a bare repo per server repo, or branch of one,
// outbox/<key>.git (see Publish.key), whose master is the commit to push
// (to the branch) and whose outbox.json is the Publish it came from. Its objects come from the mirror's repo and the r3 store, through
// git's alternates (relative: the workspace can move).
//
// A commit's parent is the mirror's master it was worked out from, so a
// push is a fast-forward, or fails if someone pushed since: then the mirror
// is behind, and the next run, on an updated mirror, works it out again.
final class Outbox(val root: os.Path, mirror: os.Path, store: os.Path) {

  // `key`: a Publish's (see Publish.key)
  def dir(key: String): os.Path = root / os.RelPath(s"$key.git")
  private def mirror_dir(repo: String): os.Path =
    mirror / os.RelPath(s"$repo.git")

  private def git(at: os.Path, args: String*): Option[String] =
    Try(
      os.proc("git" +: args)
        .call(cwd = at, stderr = os.Pipe, env = Outbox.identity)
        .out
        .text()
        .trim
    ).toOption

  private def tree_of(at: os.Path, commit: String): Option[String] =
    git(at, "rev-parse", "--verify", "-q", s"$commit^{tree}")

  // the submodule entries (gitlinks) in `rev`'s tree, as "path sha"
  private def gitlinks(at: os.Path, rev: String): Seq[String] =
    git(at, "ls-tree", "-r", rev).toSeq
      .flatMap(_.linesIterator)
      .collect { case s"160000 commit $sha\t$path" => s"$path $sha" }

  private def has_master(d: os.Path): Boolean =
    git(d, "rev-parse", "--verify", "-q", "refs/heads/master").isDefined

  // the keys (see Publish.key) of what has something to push
  def pending: Seq[String] =
    if (!os.isDir(root)) Seq()
    else
      os.walk(root)
        .filter(d => os.isFile(d / "outbox.json"))
        .map(d => d.relativeTo(root).toString.stripSuffix(".git"))
        .sorted

  // Whether what's pending for `key` can no longer be pushed as it is: the
  // branch on the mirror (brought up to date first) has moved off the
  // commit it was worked out on. Then it's to be worked out again; a push
  // would fail.
  def is_stale(key: String): Boolean = {
    val p = publish_of(key)
    val d = mirror_dir(p.repo)
    val current =
      if (!os.isDir(d)) None
      else git(d, "rev-parse", "--verify", "-q", s"refs/heads/${p.branch}")
    current.nonEmpty && current != p.base
  }

  // drops what's pending for `key`
  def drop(key: String): Unit = locked_key(key)(os.remove.all(dir(key)))

  def publish_of(key: String): Publish =
    upickle.default.read[Publish](os.read(dir(key) / "outbox.json"))

  // Records what `p` says its repo should hold: a commit of its files on its
  // base, unless the base has them already (as RepoInfo.update, where there's
  // then nothing to commit), or, with no base, there are none. Then there's
  // nothing to push, but a repo that isn't there is still to be made; a
  // branch that isn't there is pushed as its base, where it starts, and with
  // no base (and nothing to tell) it's left out: a branch can't be there
  // empty, and the repo is made for whichever branch has something. True if
  // there's something to do; what was pending for it is replaced.
  def record(p: WithData[Publish]): Boolean = locked(p.value.repo) {
    val repo = p.value.key
    val d = dir(repo)
    // where the base is: the repo's mirror, or where it starts (see
    // Publish.from)
    val base_dir = mirror_dir(p.value.from.getOrElse(p.value.repo))
    val same = p.value.base match {
      case Some(b) => tree_of(base_dir, b).contains(p.data.id)
      case None    => p.data.id == Outbox.empty_tree
    }
    val empty_branch = same && p.value.base.isEmpty &&
      p.value.branch != "master" && p.value.notices.isEmpty
    if (same && (p.value.exists || empty_branch)) {
      os.remove.all(d)
      false
    } else {
      if (!os.isDir(d)) {
        os.makeDir.all(d)
        val _ = git(d, "init", "-q", "--bare")
      }
      // where its objects are: the store's, and the base's repo's
      val objects = d / "objects"
      val from = Seq(store / "objects") ++
        Option(base_dir / "objects").filter(os.isDir(_))
      os.write.over(
        objects / "info" / "alternates",
        from.map(_.relativeTo(objects).toString + "\n").mkString,
        createFolders = true
      )
      if (same) {
        // (a repo's fork starts at its base; a branch, only if it's pushed)
        p.value.base.filter(_ => p.value.branch != "master") match {
          case Some(b) => val _ = git(d, "update-ref", "refs/heads/master", b)
          case None => val _ = git(d, "update-ref", "-d", "refs/heads/master")
        }
      } else {
        // a commit that would drop a submodule (which a publisher's copy of
        // its base can't keep) isn't made
        val dropped = p.value.base.toSeq
          .flatMap(gitlinks(base_dir, _))
          .toSet
          .diff(gitlinks(d, p.data.id).toSet)
        if (dropped.nonEmpty) {
          os.remove.all(d)
          throw Exception(
            s"$repo: not recorded: it would drop ${dropped.size} submodule entries"
          )
        }
        val current = git(d, "rev-parse", "--verify", "-q", "refs/heads/master")
        val current_ok = current.exists(c =>
          tree_of(d, c).contains(p.data.id) &&
            git(d, "rev-list", "--parents", "-n", "1", c)
              .map(_.split(' ').toSeq.drop(1)) == Some(p.value.base.toSeq)
        )
        if (!current_ok) {
          val parents = p.value.base.toSeq.flatMap(b => Seq("-p", b))
          val commit = git(
            d,
            Seq("commit-tree", p.data.id) ++ parents ++ Seq(
              "-m",
              p.value.message
            )*
          ).getOrElse(throw Exception(s"can't commit to ${d.toString}"))
          val _ = git(d, "update-ref", "refs/heads/master", commit)
        }
      }
      os.write.over(
        d / "outbox.json",
        upickle.default.write(p.value, indent = 2)
      )
      true
    }
  }

  // Does what's pending for `key` (a Publish's, see Publish.key), if
  // anything: makes its repo if it's to be made (by forking: what's pushed
  // goes on top of where the fork starts) and pushes it (a fast-forward or
  // nothing), then sends its notices. Throws if it can't: then it stays
  // pending.
  def push_one(
      server: RemoteServer,
      can_send_mail: Boolean,
      log: String => Unit,
      key: String
  ): Unit = locked_key(key) {
    val d = dir(key)
    if (os.isFile(d / "outbox.json")) {
      val p = publish_of(key)
      val repo = p.repo
      try {
        if (!p.exists && !on_server(server, repo)) {
          val create = p.create.getOrElse(
            throw Exception(s"$repo isn't on the server, and can't be made")
          )
          ag.rules.refuse_if_read_only(s"create $repo")
          log(s"forking $repo from ${create.fork_from}")
          server.ssh("fork", create.fork_from, repo).check(cwd = d)
          for (r <- create.readers)
            server.ssh("perms", repo, "+", "READERS", r).check(cwd = d)
          for (w <- create.writers)
            server.ssh("perms", repo, "+", "WRITERS", w).check(cwd = d)
        }
        if (has_master(d)) {
          ag.rules.refuse_if_read_only(s"push $key")
          log(s"pushing $key")
          server
            .SshProc(
              "git",
              "push",
              "-q",
              server.git_uri(repo),
              s"refs/heads/master:refs/heads/${p.branch}"
            )
            .check(cwd = d)
        }
      } catch {
        case scala.util.control.NonFatal(e) =>
          throw Exception(
            s"$key: not pushed (it stays in the outbox; has the server moved on since the mirror fetched $repo?)",
            e
          )
      }
      os.remove.all(d)
      // the emails about it, now that it's done (a failed one is only
      // logged: it isn't sent again)
      for (n <- p.notices)
        Try(n.send(server, can_send_mail, log)).failed.foreach(e =>
          log(s"$key: mailing ${n.to.toString} failed: ${e.getMessage}")
        )
    }
  }

  // Does what's pending (see push_one), in order, then brings the mirror up
  // to date. Stops at the first that can't be done, and throws: it and the
  // rest stay pending.
  def sync(
      server: RemoteServer,
      can_send_mail: Boolean,
      log: String => Unit
  ): Unit = {
    for (key <- pending) push_one(server, can_send_mail, log, key)
    LocalMirror.update(server, mirror, log)
  }

  // whether `repo` is on the server: asked without reading it, which would
  // make it (gitolite makes a missing repo its reader may create)
  private def on_server(server: RemoteServer, repo: String): Boolean =
    server
      .ssh("info", repo)
      .lines(cwd = root)
      .exists(_.split("""\s""").lastOption.contains(repo))

  // recording and pushing what's for a repo: one at a time (runs record and
  // push from many threads; two of a new repo's branches mustn't both make
  // it)
  private val locks = scala.collection.concurrent.TrieMap[String, Object]()
  private def locked[A](repo: String)(f: => A): A =
    locks.getOrElseUpdate(repo, Object()).synchronized(f)
  // the lock of the repo what's pending for `key` is for (its own, if
  // there's nothing)
  private def locked_key[A](key: String)(f: => A): A =
    locked(Try(publish_of(key).repo).getOrElse(key))(f)
}

object Outbox {
  // git's empty tree
  val empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

  // who the outbox's commits are by
  private val identity = Map(
    "GIT_AUTHOR_NAME" -> "grader",
    "GIT_AUTHOR_EMAIL" -> "grader@localhost",
    "GIT_COMMITTER_NAME" -> "grader",
    "GIT_COMMITTER_EMAIL" -> "grader@localhost"
  )
}
