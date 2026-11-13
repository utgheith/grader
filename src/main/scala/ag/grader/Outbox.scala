package ag.grader

import ag.r3.WithData

import scala.util.Try

// What the grader would push to the git server, kept as local commits for
// `sync` to push: a bare repo per server repo, outbox/<repo>.git, whose
// master is the commit to push and whose outbox.json is the Publish it came
// from. Its objects come from the mirror's repo and the r3 store, through
// git's alternates (relative: the workspace can move).
//
// A commit's parent is the mirror's master it was worked out from, so a
// push is a fast-forward, or fails if someone pushed since: then the mirror
// is behind, and the next run, on an updated mirror, works it out again.
final class Outbox(val root: os.Path, mirror: os.Path, store: os.Path) {

  def dir(repo: String): os.Path = root / os.RelPath(s"$repo.git")
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

  // the repos with something to push
  def pending: Seq[String] =
    if (!os.isDir(root)) Seq()
    else
      os.walk(root)
        .filter(d => os.isFile(d / "outbox.json"))
        .map(d => d.relativeTo(root).toString.stripSuffix(".git"))
        .sorted

  def publish_of(repo: String): Publish =
    upickle.default.read[Publish](os.read(dir(repo) / "outbox.json"))

  // Records what `p` says its repo should hold: a commit of its files on its
  // base, unless the base has them already (as RepoInfo.update, where there's
  // then nothing to commit), or, with no base, there are none. Then there's
  // nothing to push, but a repo that isn't there is still to be made. True
  // if there's something to do; what was pending for the repo is replaced.
  def record(p: WithData[Publish]): Boolean = {
    val repo = p.value.repo
    val d = dir(repo)
    // where the base is: the repo's mirror, or its fork source's
    val base_dir = mirror_dir(
      p.value.create.filterNot(_ => p.value.exists).fold(repo)(_.fork_from)
    )
    val same = p.value.base match {
      case Some(b) => tree_of(base_dir, b).contains(p.data.id)
      case None    => p.data.id == Outbox.empty_tree
    }
    if (same && p.value.exists) {
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
        val _ = git(d, "update-ref", "-d", "refs/heads/master")
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

  // Makes the repos that are to be made (by forking: what's pushed goes on
  // top of where the fork starts) and pushes what's pending to `server` (a
  // fast-forward or nothing), sending each one's notices once it's done;
  // then brings the mirror up to date. What can't be done stays pending, and
  // is reported.
  def sync(
      server: RemoteServer,
      can_send_mail: Boolean,
      log: String => Unit
  ): Unit = {
    // whether `repo` is on the server: asked without reading it, which would
    // make it (gitolite makes a missing repo its reader may create)
    def on_server(repo: String): Boolean =
      server
        .ssh("info", repo)
        .lines(cwd = root)
        .exists(_.split("""\s""").lastOption.contains(repo))
    for (repo <- pending) {
      val d = dir(repo)
      val p = publish_of(repo)
      val done = Try {
        if (!p.exists && !on_server(repo)) {
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
          ag.rules.refuse_if_read_only(s"push $repo")
          log(s"pushing $repo")
          server
            .SshProc(
              "git",
              "push",
              "-q",
              server.git_uri(repo),
              "refs/heads/master:refs/heads/master"
            )
            .check(cwd = d)
        }
      }
      if (done.isSuccess) {
        os.remove.all(d)
        // the emails about it, now that it's done (a failed one is only
        // logged: it isn't sent again)
        for (n <- p.notices)
          Try(n.send(server, can_send_mail, log)).failed.foreach(e =>
            log(s"$repo: mailing ${n.to.toString} failed: ${e.getMessage}")
          )
      } else
        log(
          s"$repo: not done (the server's moved on? update the mirror and run again): ${done.failed.get.getMessage}"
        )
    }
    LocalMirror.update(server, mirror, log)
  }
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
