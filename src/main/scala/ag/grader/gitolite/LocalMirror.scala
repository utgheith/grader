package ag.grader.gitolite

import ag.grader.RemoteServer
import ag.common.down
import ag.rules.Source

import java.util.UUID
import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try

// Keeps a local mirror of the git server: a bare clone of each repo,
// <root>/<repo>.git, with every ref (notes included). Targets read it as an
// ag.rules.Source; only this writes to it (from `sync`),
// and to keep it that way its directories are read-only (as git leaves the
// object files) except while an update changes them, so a stray write fails.
// A repo is fetched only if the
// server's `history` (each repo's master, as of its last push; repos not
// pushed to since the server started keeping it aren't listed) says it
// changed since the last fetch, or if `history` fails or lists nothing, so an
// update with nothing new is two ssh calls: `info` and `history`.
object LocalMirror {
  // what we last fetched, per the server's `history`; inside the repo, which
  // git ignores
  private def stamp(dir: os.Path): os.Path = dir / "grader-history"
  private val unlisted = "unlisted"

  // what gitolite takes as a repo's name ($REPONAME_PATT)
  private val repo_name = """@?[0-9a-zA-Z][-0-9a-zA-Z._@/+]*""".r

  def update(server: RemoteServer, root: os.Path, log: String => Unit): Unit = {
    val started = System.nanoTime()
    os.makeDir.all(root)
    // (the lock file is made once, while the mirror's still writable)
    if (!os.exists(root / ".grader.lock")) writable(root)
    ag.common.RunLock(root, "updating the mirror") {
      try mirror(server, root, log, started)
      finally lock(root)
    }
  }

  // write permission on a directory: what making, renaming or removing
  // anything in it takes (everyone's, and the owner's)
  private val write_bits = Integer.parseInt("222", 8)
  private val owner_write = Integer.parseInt("200", 8)

  private def dirs(d: os.Path): Seq[os.Path] =
    d +: os.walk(d, skip = !os.isDir(_, followLinks = false))

  private def writable(d: os.Path): Unit = {
    val perms = os.perms(d, followLinks = false).value
    if ((perms & owner_write) == 0)
      os.perms.set(d, os.PermSet(perms | owner_write))
  }

  // Makes every directory in the mirror read-only: ends every update, so it
  // also locks what an update that was killed left writable. (Deleting the
  // mirror takes `chmod -R u+w` first.)
  private[grader] def lock(root: os.Path): Unit =
    for {
      d <- dirs(root)
      perms = os.perms(d, followLinks = false).value
      if (perms & write_bits) != 0
    } os.perms.set(d, os.PermSet(perms & ~write_bits))

  // Makes what updating repo `dir` changes writable: the directories from
  // `root` down to it, and all of its own.
  private def unlock(root: os.Path, dir: os.Path): Unit = {
    val segments = dir.relativeTo(root).segments
    for {
      n <- 0 until segments.size
      d = segments.take(n).foldLeft(root)(_ / _)
      if os.isDir(d)
    } writable(d)
    if (os.isDir(dir)) dirs(dir).foreach(writable)
  }

  private def mirror(
      server: RemoteServer,
      root: os.Path,
      log: String => Unit,
      started: Long
  ): Unit = {
    def secs(since: Long): String = f"${(System.nanoTime() - since) / 1e9}%.1fs"
    // (info also lists wild repos' patterns, e.g. [a-z].*: not repos)
    val repos = for {
      line <- server.ssh("info").lines(cwd = os.pwd)
      if line.nonEmpty && !line.startsWith("hello")
      name = line.split("""\s""").toSeq.last
      if repo_name.matches(name)
    } yield name
    val (rc, out, _) = server.ssh("history").run(cwd = os.pwd, check = false)
    val lines =
      if (rc != 0) Seq() else os.read.lines(out).filter(_.trim.nonEmpty)
    val history: Map[String, String] = lines
      .map(_.trim.split("""\s+"""))
      .collect { case Array(name, sha) => (name, sha) }
      .toMap
    // (history succeeds, printing nothing, for a user who can't read the
    // grader repo)
    if (rc != 0) log("the server's history failed: fetching every repo")
    else if (history.isEmpty)
      log("the server's history listed nothing: fetching every repo")
    if (history.size != lines.size)
      log(
        s"the server's history: ${lines.size - history.size} lines aren't \"repo sha\""
      )

    // what to update, and why: new repos, and those whose history changed
    // since we last fetched them
    def why(repo: String): Option[String] = {
      val dir = root / os.RelPath(s"$repo.git")
      if (!Source.is_repo(dir)) Some("new")
      else if (history.isEmpty) Some("no history")
      else if (Try(os.read(stamp(dir)).trim).toOption == expected(repo)) None
      else if (history.contains(repo)) Some("moved")
      else Some("not in history")
    }
    // what the stamp should say: the history's sha, or `unlisted`, for a
    // repo it doesn't list (one not pushed to since the server started
    // keeping history: it can't have changed since we fetched it)
    def expected(repo: String): Option[String] =
      if (history.isEmpty) None else Some(history.getOrElse(repo, unlisted))
    val reasons = for (r <- repos; w <- why(r)) yield (r, w)
    val stale = reasons.map(_._1)
    val by_reason = reasons
      .groupMapReduce(_._2)(_ => 1)(_ + _)
      .map((w, n) => s"$n $w")
      .mkString(", ")
    log(
      s"mirroring into ${root.toString}: ${stale.size} of ${repos.size} repos need updating" +
        (if (by_reason.isEmpty) "" else s" ($by_reason)") +
        s"; the server's history lists ${history.size}"
    )
    val done = AtomicInteger()
    val failed = AtomicInteger()
    // as many at a time as ssh runs (see RemoteServer.ssh_per_host), so each
    // repo's time is its own, not its wait for its turn
    val turns = Semaphore(RemoteServer.ssh_per_host)
    stale
      .map { repo =>
        Thread
          .ofVirtual()
          .nn
          .start { () =>
            turns.down(1) {
              val at = System.nanoTime()
              val how = Try(one(server, root, repo, expected(repo))).fold(
                e => { failed.incrementAndGet(); s"failed: ${e.toString}" },
                identity
              )
              log(
                s"  [${done.incrementAndGet()}/${stale.size}] $repo: $how (${secs(at)})"
              )
            }
          }
          .nn
      }
      .foreach(_.join())
    log(
      s"mirrored in ${secs(started)}: ${stale.size} updated, ${failed.get} failed"
    )
  }

  // brings one repo up to date: "cloned" or "fetched"
  private def one(
      server: RemoteServer,
      root: os.Path,
      repo: String,
      latest: Option[String]
  ): String = {
    val dir = root / os.RelPath(s"$repo.git")
    unlock(root, dir)
    val how =
      if (!Source.is_repo(dir)) {
        // cloned beside it, then moved in: never half a repo
        os.makeDir.all(dir / os.up)
        val tmp = dir / os.up / s".tmp-${UUID.randomUUID().toString}"
        try {
          server
            .SshProc(
              "git",
              "clone",
              "-q",
              "--mirror",
              server.git_uri(repo),
              tmp
            )
            .check(cwd = root)
          os.move(tmp, dir, atomicMove = true)
        } finally os.remove.all(tmp)
        "cloned"
      } else {
        // from the configured server, not the repo's origin (which names
        // whatever server it was cloned from); the refspec --mirror uses
        server
          .SshProc(
            "git",
            "fetch",
            "-q",
            "--prune",
            server.git_uri(repo),
            "+refs/*:refs/*"
          )
          .check(cwd = dir)
        "fetched"
      }
    latest.foreach(os.write.over(stamp(dir), _))
    how
  }
}
