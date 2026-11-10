package ag.grader

import munit.FunSuite

// RepoInfo.update with an existing checkout whose origin is a local bare repo
// (so no ssh: that's only for cloning and forking)
class RepoInfoTests extends FunSuite {
  private def git(cwd: os.Path, args: String*): String =
    os.proc(
      Seq("git", "-c", "user.name=t", "-c", "user.email=t@t") ++ args
    ).call(cwd = cwd, stderr = os.Pipe)
      .out
      .text()
      .trim

  private case class Setup(origin: os.Path, checkout: os.Path, other: os.Path)

  // a bare origin with one commit, our checkout of it, and someone else's
  private def setup(): Setup = {
    val root = os.temp.dir(deleteOnExit = true)
    val origin = root / "origin.git"
    val _ = git(root, "init", "-q", "--bare", "-b", "master", origin.toString)
    val seed = root / "seed"
    val _ = git(root, "clone", "-q", origin.toString, seed.toString)
    os.write(seed / "f", "0")
    val _ = git(seed, "add", "f")
    val _ = git(seed, "commit", "-q", "-m", "seed")
    val _ = git(seed, "push", "-q", "origin", "HEAD:refs/heads/master")
    def clone(name: String): os.Path = {
      val d = root / name
      val _ = git(root, "clone", "-q", origin.toString, d.toString)
      val _ = git(d, "config", "user.name", "t")
      val _ = git(d, "config", "user.email", "t@t")
      d
    }
    Setup(origin, clone("checkout"), clone("other"))
  }

  // someone else pushes a change to f
  private def push_other(s: Setup, content: String): String = {
    val _ = git(s.other, "pull", "-q", "--ff-only")
    os.write.over(s.other / "f", content)
    val _ = git(s.other, "commit", "-q", "-a", "-m", s"other $content")
    val _ = git(s.other, "push", "-q", "origin", "HEAD:refs/heads/master")
    git(s.other, "rev-parse", "HEAD")
  }

  private def update(s: Setup, content: String)(before_push: => Unit): Unit = {
    val info = RepoInfo(RemoteServer("u", "h", 1), "r", Some(None))
    info.update(s.checkout, None, s"write $content", Seq(), Seq(), true) { _ =>
      before_push
      os.write.over(s.checkout / "g", content)
    }
  }

  test("a commit a failed push left behind is dropped, not merged") {
    val s = setup()
    // a commit that never got pushed
    os.write(s.checkout / "stale", "x")
    val _ = git(s.checkout, "add", "stale")
    val _ = git(s.checkout, "commit", "-q", "-m", "stale")
    val theirs = push_other(s, "1")

    update(s, "mine")(())

    // ours is a fast-forward on theirs: no merge, no stale commit
    val log = git(s.origin, "log", "--format=%P %s", "master").linesIterator.toSeq
    assertEquals(log.head, s"$theirs write mine")
    assert(!log.exists(_.endsWith("stale")), log)
    assertEquals(git(s.origin, "show", "master:g"), "mine")
  }

  test("a push that isn't a fast-forward fails and changes nothing") {
    val s = setup()
    var theirs = ""
    // someone else pushes after we fetched, before we push
    val _ = intercept[Exception](update(s, "mine") { theirs = push_other(s, "2") })
    assertEquals(git(s.origin, "rev-parse", "master"), theirs)
  }

  test("the next run starts from the server again and pushes") {
    val s = setup()
    val _ = intercept[Exception](update(s, "mine") { val _ = push_other(s, "3") })
    update(s, "mine")(())
    assertEquals(git(s.origin, "show", "master:g"), "mine")
    assertEquals(git(s.origin, "show", "master:f"), "3")
  }
}
