package ag.r3

import munit.FunSuite

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.SortedSet

// Sources: bare repos that something else updates, which r3 only reads
class SourceTests extends FunSuite {
  private def git(cwd: os.Path, env: Map[String, String], args: String*): String =
    os.proc(Seq("git", "-c", "user.name=t", "-c", "user.email=t@t") ++ args)
      .call(cwd = cwd, env = env, stderr = os.Pipe)
      .out
      .text()
      .trim
  private def git(cwd: os.Path, args: String*): String = git(cwd, Map(), args*)

  // a mirror (root) with bare repos, and a clone of each to push from
  private class Setup {
    val base: os.Path = os.temp.dir(deleteOnExit = true)
    val root: os.Path = base / "mirror"
    val ws: os.Path = base / "ws"
    val source: Source = Source(root)

    def make(name: String, dotted: Boolean = true): os.Path = {
      val bare = root / os.RelPath(if (dotted) s"$name.git" else name)
      os.makeDir.all(bare / os.up)
      val _ = git(base, "init", "-q", "--bare", "-b", "master", bare.toString)
      val work = base / "work" / os.RelPath(name)
      os.makeDir.all(work / os.up)
      val _ = git(base, "clone", "-q", bare.toString, work.toString)
      work
    }

    // commits `content` as f, pushes, and returns the commit
    def push(work: os.Path, content: String, env: Map[String, String] = Map()): String = {
      os.write.over(work / "f", content)
      val _ = git(work, "add", "f")
      val _ = git(work, env, "commit", "-q", "-m", s"set $content")
      val _ = git(work, "push", "-q", "origin", "HEAD:refs/heads/master")
      git(work, "rev-parse", "HEAD")
    }

    def push_note(work: os.Path, commit: String, note: String): Unit = {
      val _ = git(work, "notes", "--ref=push_time", "add", "-f", "-m", note, commit)
      val _ = git(work, "push", "-q", "origin", "refs/notes/push_time")
    }
  }

  test("repos: plain, .git and nested ones; refs and ref of a missing repo") {
    val s = Setup()
    val _ = s.make("a")
    val _ = s.make("b", dotted = false)
    val _ = s.make("c/d")
    val st = State(s.ws)
    assertEquals(st.track(s.source.repos), SortedSet("a", "b", "c/d"))
    assertEquals(st.track(s.source.refs("nope")).size, 0)
    assertEquals(st.track(s.source.master("nope")), None)
  }

  test("a reader of master reruns when master moves, not when notes do") {
    val s = Setup()
    val a = s.make("a")
    val other = s.make("other")
    val runs = AtomicInteger(0)
    val sc = Scope("readers")
    val reader: Target[Option[String]] = sc.target((m = s.source.master("a"))) {
      (producer, deps) =>
        val _ = runs.incrementAndGet()
        deps.m.flatMap(c => producer.read_text(producer.commit(c).tree, os.rel / "f"))
    }
    val c1 = s.push(a, "one")
    assertEquals(State(s.ws).track(reader), Some("one"))
    s.push_note(a, c1, "pushed at 1")
    val _ = s.push(other, "x")
    assertEquals(State(s.ws).track(reader), Some("one"))
    assertEquals(runs.get, 1, "notes and other repos moved: not rerun")
    val _ = s.push(a, "two")
    assertEquals(State(s.ws).track(reader), Some("two"))
    assertEquals(runs.get, 2)
  }

  test("a pinned commit keeps reading what it named after the repo moves") {
    val s = Setup()
    val a = s.make("a")
    val c1 = s.push(a, "one")
    val st = State(s.ws)
    val pinned = st.track(s.source.master("a")).get
    assertEquals(pinned.id, c1)
    val _ = s.push(a, "two")
    val tree = st.commit(pinned).tree
    assertEquals(st.read_text(tree, os.rel / "f"), Some("one"))
    assertEquals(os.read(st.checkout(tree) / "f"), "one")
    assertEquals(
      State(s.ws).track(s.source.master("a")).map(c => st.read_text(st.commit(c).tree, os.rel / "f")),
      Some(Some("two"))
    )
  }

  test("commit, history and notes") {
    val s = Setup()
    val a = s.make("a")
    val c1 = s.push(a, "one", Map("GIT_COMMITTER_DATE" -> "2026-01-02T03:04:05+0200"))
    val c2 = s.push(a, "two")
    s.push_note(a, c1, "noted")
    val st = State(s.ws)
    val master = st.track(s.source.master("a")).get
    val info = st.commit(Commit(master.at, c1))
    assertEquals(info.committed, "2026-01-02T03:04:05+02:00")
    assertEquals(info.subject, "set one")
    assertEquals(info.parents, Seq())
    assertEquals(st.history(master).map(_.id), Seq(c2, c1))
    assertEquals(st.history(master, limit = 1).map(_.id), Seq(c2))
    val notes = st.track(s.source.ref("a", "refs/notes/push_time")).get
    assertEquals(st.note(notes, c1).map(_.trim), Some("noted"))
    assertEquals(st.note(notes, c2), None)
  }

  test("reading a source never writes to it") {
    val s = Setup()
    val a = s.make("a")
    val c1 = s.push(a, "one")
    s.push_note(a, c1, "noted")
    def snapshot() =
      os.walk(s.root).map(p => (p, os.isDir(p), if (os.isFile(p)) os.mtime(p) else 0L)).toSet
    val before = snapshot()
    val st = State(s.ws)
    val _ = st.track(s.source.repos)
    val m = st.track(s.source.master("a")).get
    val _ = st.history(m)
    val _ = st.checkout(st.commit(m).tree)
    val _ = st.note(st.track(s.source.ref("a", "refs/notes/push_time")).get, c1)
    assertEquals(snapshot(), before)
  }
}
