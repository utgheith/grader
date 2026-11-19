package ag.grader.gitolite

import ag.grader.RemoteServer
import ag.common.down
import ag.rules.{Source, Upstream}

import java.util.UUID
import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SortedSet
import scala.collection.concurrent.TrieMap
import scala.util.Try

// Keeps a local mirror of the git server: a bare clone of each repo,
// <root>/<repo>.git, with every ref (notes included). Targets read it as an
// ag.rules.Source; only this writes to it, and to keep it that way its
// directories are read-only (as git leaves the object files) except while
// an update changes them, so a stray write fails. A repo is fetched only if
// the server's `history` (each repo's stamp, as of its last push; repos not
// pushed to since the server started keeping it aren't listed) says it
// changed since the last fetch, or if `history` fails or lists nothing.
//
// Two ways: all at once (LocalMirror.update, for `sync`: an update with
// nothing new is two ssh calls, `info` and `history`), or lazily, as the
// Upstream of a Source (LocalMirror.of, for runs; LocalMirror.renewing, for
// the status pages, which have a mirror of their own): `info` and `history`
// when the first repo is read, then each repo when it's first read. A
// process holds the mirror's lock from its first read until it exits.
final class LocalMirror private (
    server: RemoteServer,
    root: os.Path,
    log: String => Unit
) extends Upstream {
  import LocalMirror.*

  // the mirror's lock (see hold), and its directories writable while this
  // process updates it
  private lazy val locked: Unit = {
    hold(root)
    writable(root)
  }

  // what the server has, read once
  private lazy val listing: Listing = {
    locked
    read_listing(server, log)
  }

  def repos: SortedSet[String] = listing.repos.to(SortedSet)

  // each repo's refresh, run once (a lazy val runs once, whoever asks)
  private final class Once(repo: String) {
    lazy val done: Unit = {
      val l = listing
      if (l.repos.contains(repo))
        for (w <- why(root, l, repo)) {
          val at = System.nanoTime()
          val how = one(server, root, repo, l.expected(repo))
          lock_repo(root, repo)
          log(s"  $repo: $how ($w, ${secs(at)})")
        }
    }
  }
  private val refreshes = TrieMap[String, Once]()

  def refresh(name: String): Unit =
    refreshes.getOrElseUpdate(name, Once(name)).done
}

object LocalMirror {
  // one per mirror per process: its lock is the process's
  private val lazies = TrieMap[os.Path, LocalMirror]()

  // the mirror at `root`, kept up to date lazily (see LocalMirror): the
  // server's history read once per process
  def of(
      server: RemoteServer,
      root: os.Path,
      log: String => Unit
  ): LocalMirror =
    lazies.getOrElseUpdate(root, LocalMirror(server, root, log))

  // The same, for a process that goes on (the status pages): the server's
  // history read again, by a new LocalMirror, once the last one is
  // `ttl_ms` old, so what's read is at most about that much behind.
  def renewing(
      server: RemoteServer,
      root: os.Path,
      ttl_ms: Long,
      log: String => Unit
  ): Upstream = new Upstream {
    private var current: Option[(Long, LocalMirror)] = None
    private def now: LocalMirror = synchronized {
      val t = System.currentTimeMillis()
      current match {
        case Some((at, m)) if t - at < ttl_ms => m
        case _                                =>
          val m = LocalMirror(server, root, log)
          current = Some((t, m))
          m
      }
    }
    def repos: SortedSet[String] = now.repos
    def refresh(name: String): Unit = now.refresh(name)
  }

  // Takes the lock of the mirror at `root` (once per process: every
  // LocalMirror on it shares it) and holds it until the process exits; on
  // the way out, however it goes, the mirror is made read-only again (see
  // lock). Throws if another process holds it.
  private val held = TrieMap[os.Path, Holder]()
  private final class Holder(root: os.Path) {
    lazy val taken: Unit = {
      os.makeDir.all(root)
      if (!os.exists(root / ".grader.lock")) writable(root)
      ag.common.RunLock.acquire(root, "a run reading the mirror") match {
        case Right(_)     => ()
        case Left(holder) =>
          throw Exception(s"the mirror ${root.toString} is in use by $holder")
      }
      Runtime.getRuntime.nn.addShutdownHook(Thread(() => lock(root)))
    }
  }
  private def hold(root: os.Path): Unit =
    held.getOrElseUpdate(root, Holder(root)).taken

  private def secs(since: Long): String =
    f"${(System.nanoTime() - since) / 1e9}%.1fs"

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

  // the same for one repo's own directories, once it's updated (the ones
  // above it, shared with other repos' updates, wait for lock)
  private def lock_repo(root: os.Path, repo: String): Unit = {
    val dir = root / os.RelPath(s"$repo.git")
    if (os.isDir(dir)) lock(dir)
  }

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

  // what the server has: its repos, and each one's stamp in `history`
  // (empty if history failed or listed nothing)
  private final case class Listing(
      repos: Seq[String],
      history: Map[String, String]
  ) {
    // what a repo's stamp should say: the history's sha, or `unlisted`, for
    // a repo it doesn't list (one not pushed to since the server started
    // keeping history: it can't have changed since we fetched it)
    def expected(repo: String): Option[String] =
      if (history.isEmpty) None else Some(history.getOrElse(repo, unlisted))
  }

  private def read_listing(
      server: RemoteServer,
      log: String => Unit
  ): Listing = {
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
    Listing(repos, history)
  }

  // whether `repo` needs updating, and why: a new repo, or one whose history
  // changed since we last fetched it
  private def why(root: os.Path, l: Listing, repo: String): Option[String] = {
    val dir = root / os.RelPath(s"$repo.git")
    if (!Source.is_repo(dir)) Some("new")
    else if (l.history.isEmpty) Some("no history")
    else if (Try(os.read(stamp(dir)).trim).toOption == l.expected(repo)) None
    else if (l.history.contains(repo)) Some("moved")
    else Some("not in history")
  }

  private def mirror(
      server: RemoteServer,
      root: os.Path,
      log: String => Unit,
      started: Long
  ): Unit = {
    val l = read_listing(server, log)
    val (repos, history) = (l.repos, l.history)
    val reasons = for (r <- repos; w <- why(root, l, r)) yield (r, w)
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
              val how = Try(one(server, root, repo, l.expected(repo))).fold(
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
