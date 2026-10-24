package ag.events

import munit.FunSuite

// The server's push hook (src/test/gitolite/hooks/common/post-receive, a copy
// of the real one), in the gitolite container. Tagged "integration"; skipped
// without podman.
class PushHookTests extends FunSuite {
  private val Integration = new munit.Tag("integration")

  private val conf =
    """@me = admin
      |repo @all
      |    RW+    = @me
      |    C      = @me
      |repo grader
      |    R = @me
      |""".stripMargin

  private var started: Option[GitoliteServer] = None
  private def server: GitoliteServer = started.getOrElse {
    assume(GitoliteServer.image.isDefined, "podman isn't available")
    val s = GitoliteServer(conf, Nil)
    started = Some(s)
    s
  }
  override def afterAll(): Unit = started.foreach(_.close())

  // a working copy of `repo`, acting as admin
  private final class Work(repo: String) {
    val dir: os.Path = os.temp.dir(deleteOnExit = true)
    def git(args: String*): String =
      os.proc("git" +: args)
        .call(cwd = dir, env = server.git_env("admin"), stderr = os.Pipe)
        .out
        .trim()
    val _ = git("init", "-q", "-b", "master")
    val _ = git("remote", "add", "origin", server.url(repo))
    def commit(msg: String): String = {
      val _ = git(
        "-c",
        "user.name=t",
        "-c",
        "user.email=t@x",
        "commit",
        "-q",
        "--allow-empty",
        "-m",
        msg
      )
      git("rev-parse", "HEAD")
    }
    def push(refspecs: String*): Unit = {
      val _ = git(("push" +: "-q" +: "origin" +: refspecs)*)
    }
    // the server's push times: commit -> seconds
    def push_times(): Map[String, Long] = {
      val _ = git("fetch", "-q", "-f", "origin", "refs/notes/*:refs/notes/*")
      git("notes", "--ref", "push_time", "list").linesIterator
        .filter(_.nonEmpty)
        .map { l =>
          val commit = l.split(' ')(1)
          commit -> git("notes", "--ref", "push_time", "show", commit).toLong
        }
        .toMap
    }
    def history(): Option[String] =
      Gitolite
        .observe(server.server, server.port, server.ssh("admin"))
        .get(repo)
        .flatten
  }

  test(
    "every commit of a push gets a push time, not just the tip".tag(Integration)
  ) {
    val w = Work("hooked_multi")
    val commits = (1 to 3).map(i => w.commit(s"c$i"))
    w.push("master")
    val times = w.push_times()
    assertEquals(commits.toSet.subsetOf(times.keySet), true, times)
    assertEquals(commits.map(times).distinct.size, 1, "one push, one time")
    assertEquals(w.history(), Some(commits.last))
  }

  test(
    "a commit keeps its arrival time; only new commits get later ones".tag(
      Integration
    )
  ) {
    val w = Work("hooked_later")
    val first = w.commit("first")
    w.push("master")
    Thread.sleep(1100)
    val second = w.commit("second")
    w.push("master")
    val times = w.push_times()
    assert(times(first) < times(second), times)
  }

  test(
    "pushing another branch notes its new commits, and nothing else".tag(
      Integration
    )
  ) {
    val w = Work("hooked_branch")
    val base = w.commit("base")
    w.push("master")
    val before = w.push_times()
    Thread.sleep(1100)
    val _ = w.git("checkout", "-q", "-b", "topic")
    val topic = w.commit("on topic")
    w.push("topic")
    val after = w.push_times()
    assertEquals(after(base), before(base), "master's commit untouched")
    assert(after(topic) > after(base))
    assertEquals(w.history(), Some(base), "history follows the default branch")
    // the note on base is still a single time, not appended to
    assertEquals(
      w.git("notes", "--ref", "push_time", "show", base)
        .linesIterator
        .count(_.nonEmpty),
      1
    )
  }

  test(
    "re-pushing old commits to a new branch changes no push time".tag(
      Integration
    )
  ) {
    val w = Work("hooked_repush")
    val _ = w.commit("c")
    w.push("master")
    val before = w.push_times()
    Thread.sleep(1100)
    w.push("master:refs/heads/copy")
    assertEquals(w.push_times(), before)
  }

  test("deleting a branch is fine and notes nothing".tag(Integration)) {
    val w = Work("hooked_delete")
    val _ = w.commit("c")
    w.push("master", "master:refs/heads/gone")
    val before = w.push_times()
    w.push(":refs/heads/gone")
    assertEquals(w.push_times(), before)
  }

  test(
    "prepare's reading gives an intermediate commit its arrival time".tag(
      Integration
    )
  ) {
    val w = Work("hooked_prepare")
    val middle = {
      val _ = w.commit("a"); val m = w.commit("b"); val _ = w.commit("c"); m
    }
    w.push("master")
    // what prepare does: fetch the notes, read the checked-out commit's
    val _ = w.git("fetch", "-q", "origin", "refs/notes/*:refs/notes/*")
    val lines =
      w.git("notes", "--ref", "push_time", "show", middle).linesIterator.toSeq
    assertEquals(
      ag.grader.PrepareInfo.push_time_from(lines).map(_.getEpochSecond),
      Some(w.push_times()(middle))
    )
  }
}
