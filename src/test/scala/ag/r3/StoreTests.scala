package ag.r3

import ag.common.TempDirs
import munit.FunSuite

import java.util.concurrent.atomic.AtomicInteger

// what r3 adds: targets kept in git, with history, and data as trees
class StoreTests extends FunSuite {
  private def ws(): os.Path = TempDirs.dir()

  private def git(dir: os.Path, args: String*): String =
    os.proc(Seq("git", "--git-dir", (dir / "r3.git").toString) ++ args)
      .call(stderr = os.Pipe)
      .out
      .text()
      .trim

  test("ref names: readable, valid, and a and a/b can both have one") {
    assertEquals(Store.ref_name(os.RelPath("ag/r3/x/10")), "refs/r3/ag/r3/x/10/@")
    assertEquals(Store.encode("a b"), "a%20b")
    assertEquals(Store.encode(".hidden"), "%2Ehidden")
    assertEquals(Store.encode("a..b"), "a.%2Eb")
    assertEquals(Store.encode("x.lock"), "x%2Elock")
    assertEquals(Store.encode("@"), "%40")
    for (c <- Seq("a b", ".x", "a..b", "x.lock", "@", "~^:?*[\\", "é"))
      assert(
        org.eclipse.jgit.lib.Repository.isValidRefName(
          Store.ref_name(os.rel / c)
        ),
        c
      )
    val dir = ws()
    val s = Scope("nest")
    val a: Target[Int] = s.target(EmptyTuple)((_, _) => 1)
    val f: Int => Target[Int] = s.fun((i: Int) => s.target(EmptyTuple)((_, _) => i))
    // f(2)'s path is a's with "2" appended: both refs must fit
    assertEquals(State(dir).track(a) + State(dir).track(f(2)), 3)
  }

  test("history: a commit per change, with the reason, none when nothing changed") {
    val dir = ws()
    var input = 1
    val s = Scope("history")
    val in: Target[Int] = s.target(EmptyTuple)((_, _) => input)
    val out: Target[Int] = s.target((i = in))((_, deps) => deps.i * 10)
    def log() =
      git(dir, "log", "--format=%s", Store.ref_name(out.path)).linesIterator.toSeq
    assertEquals(State(dir).track(out), 10)
    assertEquals(State(dir).track(out), 10)
    assertEquals(log(), Seq("it has no saved result"))
    input = 2
    assertEquals(State(dir).track(out), 20)
    assertEquals(
      log(),
      Seq(s"dependencies changed: ${in.path.toString}", "it has no saved result")
    )
    // `in` reruns every time (no dependencies) but only commits on change
    assertEquals(
      git(dir, "rev-list", "--count", Store.ref_name(in.path)),
      "2"
    )
    assertEquals(
      git(dir, "show", s"${Store.ref_name(out.path)}:result/value.json"),
      "20"
    )
  }

  test("a failed run leaves the saved result, and runs again next time") {
    val dir = ws()
    var input = 1
    var fail = false
    val runs = AtomicInteger(0)
    val s = Scope("fails")
    val in: Target[Int] = s.target(EmptyTuple)((_, _) => input)
    val out: Target[Int] = s.target((i = in)) { (_, deps) =>
      val _ = runs.incrementAndGet()
      if (fail) throw IllegalStateException("boom")
      deps.i
    }
    assertEquals(State(dir).track(out), 1)
    val before = git(dir, "rev-parse", Store.ref_name(out.path))
    input = 2
    fail = true
    val _ = intercept[Exception](State(dir).track(out))
    assertEquals(git(dir, "rev-parse", Store.ref_name(out.path)), before)
    fail = false
    assertEquals(State(dir).track(out), 2)
    assertEquals(runs.get, 3)
  }

  test("data: kept as a tree, read at that tree, checked out read-only") {
    val dir = ws()
    var text = "hello"
    val s = Scope("data")
    val in: Target[String] = s.target(EmptyTuple)((_, _) => text)
    val made: Target[WithData[Int]] = s.target((t = in)) { (producer, deps) =>
      create_data(producer) { d =>
        os.write(d / "sub" / "a.txt", deps.t, createFolders = true)
        os.write(d / "run.sh", "echo hi")
        os.perms.set(d / "run.sh", os.PermSet.fromString("rwxr-xr-x"))
        os.makeDir(d / "empty")
        os.symlink(d / "link", os.rel / "sub" / "a.txt")
        deps.t.length
      }
    }
    val reader: Target[(Option[String], Option[Seq[String]], String)] =
      s.target((m = made)) { (producer, deps) =>
        val co = producer.checkout(deps.m.data)
        assert(!os.perms(co / "sub" / "a.txt").contains(java.nio.file.attribute.PosixFilePermission.OWNER_WRITE))
        assert(os.perms(co / "run.sh").contains(java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE))
        assert(os.isDir(co / "empty"))
        (
          producer.read_text(deps.m.data, os.rel / "sub" / "a.txt"),
          producer.list(deps.m.data),
          os.read(co / "link")
        )
      }
    val st = State(dir)
    assertEquals(
      st.track(reader),
      (Some("hello"), Some(Seq("empty", "link", "run.sh", "sub")), "hello")
    )
    val first = st.track(made).data
    text = "bye"
    val st2 = State(dir)
    assertEquals(st2.track(reader)._1, Some("bye"))
    // the first tree is still readable, unchanged: it's pinned
    assertEquals(st2.read_text(first, os.rel / "sub" / "a.txt"), Some("hello"))
    assert(!os.exists(dir / "scratch") || os.list(dir / "scratch").isEmpty)
  }

  test("update_data starts from the previous data") {
    val dir = ws()
    var n = 1
    val s = Scope("update")
    val in: Target[Int] = s.target(EmptyTuple)((_, _) => n)
    val acc: Target[WithData[Int]] = s.target((i = in)) { (producer, deps) =>
      update_data(producer) { d =>
        os.write(d / s"${deps.i}.txt", "")
        os.list(d).size
      }
    }
    assertEquals(State(dir).track(acc).value, 1)
    n = 2
    assertEquals(State(dir).track(acc).value, 2)
  }

  test("a git repo in a target's data fails the target") {
    val dir = ws()
    val s = Scope("repos")
    val nested: Target[WithData[Unit]] = s.target(EmptyTuple) { (producer, _) =>
      create_data(producer) { d =>
        val _ = os.proc("git", "init", "-q", (d / "x").toString).call()
      }
    }
    val bare: Target[WithData[Unit]] = s.target(EmptyTuple) { (producer, _) =>
      create_data(producer) { d =>
        val _ = os.proc("git", "init", "-q", "--bare", (d / "y.git").toString).call()
      }
    }
    for (t <- Seq(nested, bare)) {
      val e = intercept[Exception](State(dir).track(t))
      assert(
        Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null)
          .exists(x => String.valueOf(x.getMessage).contains("a git repo")),
        e
      )
      assertEquals(State(dir).store.load(t.path), None)
    }
  }
}

// checking a tree out over a directory, and copying one file out of a tree
class StoreCheckoutTests extends FunSuite {
  private def git(dir: os.Path, args: String*): String =
    os.proc(Seq("git", "-c", "user.name=t", "-c", "user.email=t@x") ++ args)
      .call(cwd = dir, stderr = os.Pipe)
      .out
      .text()
      .trim

  // a repo with a file, an executable, a link, a big-ish file and a
  // submodule entry; its tree's id
  private def repo(): (os.Path, String) = {
    val d = TempDirs.dir()
    val _ = git(d, "init", "-q", "-b", "master")
    os.write(d / "a.txt", "new")
    os.write(d / "run.sh", "#!/bin/sh")
    os.perms.set(d / "run.sh", "rwxr-xr-x")
    os.symlink(d / "link", os.rel / "a.txt")
    // as written: java.nio would drop the slash
    val _ = os.proc("ln", "-s", "dir/", (d / "slash").toString).call()
    os.write(d / "dir" / "big", "x" * (3 << 20), createFolders = true)
    val _ = git(d, "add", "-A")
    // a submodule: a gitlink, with no commit behind it here
    val _ = git(d, "update-index", "--add", "--cacheinfo", s"160000,${"1" * 40},sub")
    val _ = git(d, "commit", "-q", "-m", "c")
    (d, git(d, "rev-parse", "HEAD^{tree}"))
  }

  test("checkout_to writes over what's there, merges directories, and makes a submodule an empty directory") {
    val (d, tree) = repo()
    val store = Store(d / ".git", create = false)
    val dest = TempDirs.dir()
    os.write(dest / "a.txt", "old")
    os.write(dest / "link", "a file where the link goes")
    os.write(dest / "dir" / "kept", "mine", createFolders = true)
    store.checkout_to(org.eclipse.jgit.lib.ObjectId.fromString(tree).nn, dest, writable = true)
    assertEquals(os.read(dest / "a.txt"), "new")
    assert(os.isLink(dest / "link"))
    assertEquals(os.readLink(dest / "slash").toString, "dir/")
    assertEquals(os.read(dest / "dir" / "kept"), "mine")
    assertEquals(os.size(dest / "dir" / "big"), 3L << 20)
    assert(os.perms(dest / "run.sh").contains(java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE))
    assert(os.isDir(dest / "sub"))
    assertEquals(os.list(dest / "sub"), IndexedSeq())
  }

  test("copy_file: a file (streamed), a link, nothing for a directory or a missing path") {
    val (d, tree) = repo()
    val store = Store(d / ".git", create = false)
    val id = org.eclipse.jgit.lib.ObjectId.fromString(tree).nn
    val out = TempDirs.dir()
    assert(store.copy_file(id, os.rel / "dir" / "big", out / "big"))
    assertEquals(os.size(out / "big"), 3L << 20)
    assert(store.copy_file(id, os.rel / "link", out / "link"))
    assertEquals(os.readLink(out / "link"), os.rel / "a.txt")
    assert(store.copy_file(id, os.rel / "run.sh", out / "run.sh"))
    assert(os.perms(out / "run.sh").contains(java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE))
    assert(!store.copy_file(id, os.rel / "dir", out / "dir"))
    assert(!store.copy_file(id, os.rel / "sub", out / "sub"))
    assert(!store.copy_file(id, os.rel / "nope", out / "nope"))
  }
}
