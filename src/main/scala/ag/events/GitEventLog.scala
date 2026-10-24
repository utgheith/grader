package ag.events

import java.time.Instant

// A log kept in a git repository (for a course's private log: a staff-only
// repo on the git server), one file per event:
//   events/000000001/000000001234.json   (1000 events per directory)
// Every append is one commit, so the repository's history is the log's, and
// a consistent state is simply a commit.
//
// `checkout` is this machine's working copy. Before every read or append it's
// brought to exactly what `remote` has (any leftovers of a failed append are
// discarded). An append pushes one commit; the push is the coordination point:
// the server rejects it if someone else appended first, and then the events are
// renumbered and tried again (append), or not appended at all (append_if).
final class GitEventLog(
    val remote: String,
    val checkout: os.Path,
    kinds: EventKinds,
    branch: String = "main",
    // extra environment for git, e.g. GIT_SSH_COMMAND
    env: Map[String, String] = Map()
) extends EventLog {

  private def git(args: String*): os.CommandResult =
    os.proc("git" +: args)
      .call(
        cwd = checkout,
        env = env,
        check = false,
        stdout = os.Pipe,
        stderr = os.Pipe
      )

  private def git_ok(args: String*): String = {
    val r = git(args*)
    if (r.exitCode != 0)
      throw EventLogException(
        s"git ${args.mkString(" ")} failed in ${checkout.toString}: ${r.err.text().trim}"
      )
    r.out.text()
  }

  // Makes the checkout exactly what the remote has (an empty repository if it
  // has nothing yet).
  private def sync(): Unit = {
    if (!os.isDir(checkout / ".git")) {
      os.makeDir.all(checkout)
      val _ = git_ok("init", "-q", "-b", branch)
      val _ = git_ok("remote", "add", "origin", remote)
    }
    val _ = git_ok("fetch", "-q", "origin")
    val remote_branch = s"refs/remotes/origin/$branch"
    if (git("rev-parse", "--verify", "-q", remote_branch).exitCode == 0) {
      val _ = git_ok("checkout", "-q", "-B", branch, remote_branch)
      val _ = git_ok("reset", "-q", "--hard", remote_branch)
    } else {
      // nothing pushed yet: forget any local attempt
      val _ = git("update-ref", "-d", "HEAD")
      val _ = git_ok("read-tree", "--empty")
    }
    val _ = git_ok("clean", "-qfdx")
  }

  private def events_dir: os.Path = checkout / "events"

  private def path_of(offset: Long): os.Path =
    events_dir / f"${offset / 1000}%09d" / f"$offset%012d.json"

  private def files(): Seq[os.Path] =
    if (!os.isDir(events_dir)) Nil
    else
      os.walk(events_dir)
        .filter(p => os.isFile(p) && p.ext == "json")
        .sortBy(_.last)

  def read(from: Long = 0): Iterator[Logged] = {
    val all = in_process {
      sync()
      files().drop(from.toInt).map(os.read(_))
    }
    all.iterator.map(text => Records.decode(kinds, ujson.read(text)))
  }

  def end: Long = in_process {
    sync()
    files().size.toLong
  }

  def append(events: Seq[Event]): Seq[Logged] =
    in_process(attempt(None, events).get)

  def append_if(expected_end: Long, events: Seq[Event]): Option[Seq[Logged]] =
    in_process(attempt(Some(expected_end), events))

  private val max_attempts = 50

  private def attempt(
      expected_end: Option[Long],
      events: Seq[Event]
  ): Option[Seq[Logged]] = {
    var tries = 0
    while (true) {
      tries += 1
      sync()
      val first = files().size.toLong
      if (expected_end.exists(_ != first)) return None

      val now = Instant.now().nn
      val logged = events.zipWithIndex.map((e, i) => Logged(first + i, now, e))
      for (l <- logged)
        os.write(
          path_of(l.offset),
          ujson.write(Records.encode(l)),
          createFolders = true
        )
      val _ = git_ok("add", "events")
      val _ = git_ok(
        "-c",
        "user.name=grader",
        "-c",
        "user.email=grader@localhost",
        "commit",
        "-q",
        "-m",
        s"events ${first.toString}..${(first + logged.size - 1).toString}"
      )
      val push = git("push", "-q", "origin", s"HEAD:refs/heads/$branch")
      if (push.exitCode == 0) return Some(logged)

      // Rejected. If someone else appended meanwhile, that's a lost race:
      // renumber and retry (or, for append_if, give up). Otherwise it's a
      // real failure.
      val _ = git_ok("fetch", "-q", "origin")
      val theirs =
        git("rev-parse", "--verify", "-q", s"refs/remotes/origin/$branch")
      val raced = theirs.exitCode == 0 && {
        val count = git_ok(
          "ls-tree",
          "-r",
          "--name-only",
          s"refs/remotes/origin/$branch",
          "events"
        ).linesIterator.count(_.endsWith(".json"))
        count > first
      }
      if (!raced)
        throw EventLogException(
          s"pushing to $remote failed: ${push.err.text().trim}"
        )
      if (expected_end.isDefined) return None
      if (tries >= max_attempts)
        throw EventLogException(
          s"gave up appending to $remote after $tries attempts"
        )
    }
    None
  }

  // One appender or reader per checkout at a time within this process; the
  // server takes care of other machines.
  private def in_process[A](f: => A): A =
    GitEventLog.lock_for(checkout).synchronized(f)
}

object GitEventLog {
  private val locks = java.util.concurrent.ConcurrentHashMap[os.Path, Object]()
  private def lock_for(checkout: os.Path): Object =
    locks.computeIfAbsent(checkout, _ => Object()).nn
}
