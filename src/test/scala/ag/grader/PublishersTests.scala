package ag.grader

import ag.common.TempDirs
import ag.common.given_ReadWriter_SortedMap
import ag.r3.State
import munit.FunSuite

import scala.collection.SortedMap

// The publishers (Publishers) and the outbox they're recorded in (Outbox),
// on made-up repos (see Fixture)
class PublishersTests extends FunSuite {
  import Fixture.*

  private def setup(root: os.Path) = {
    val (r, subs) = Fixture.setup(root)
    val pubs = Publishers(Grading(TestRuns(subs)), Dropbox(None))
    val ob = Outbox(root / "outbox", r.mirror, root / "ws" / "r3.git")
    (r, pubs, ob)
  }

  private def files(dir: os.Path): Seq[String] =
    os.walk(dir).filter(os.isFile(_)).map(_.relativeTo(dir).toString).sorted

  private def git(dir: os.Path, args: String*): String =
    os.proc("git" +: args).call(cwd = dir).out.text().trim

  test("a new repo: a commit with no parent; once it's on the mirror, nothing to push") {
    val root = TempDirs.dir()
    val (r, pubs, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "main.c" -> "mine")()
    val p = State(root / "ws").track(pubs.aliases(c1, p1))
    assertEquals(p.value.base, None)
    assertEquals(p.value.create, Some(Create("empty", Seq("@c1_staff"), Seq())))
    assert(ob.record(p))
    assertEquals(ob.pending, Seq("c1_p1__aliases"))
    // recording it again changes nothing
    val d = ob.dir("c1_p1__aliases")
    val commit = git(d, "rev-parse", "master")
    assert(ob.record(p))
    assertEquals(git(d, "rev-parse", "master"), commit)
    assertEquals(git(d, "rev-list", "--parents", "-n", "1", "master"), commit)
    assertEquals(
      upickle.default.read[SortedMap[CSID, Alias]](git(d, "show", "master:aliases.json")),
      SortedMap(s1 -> Alias(1))
    )

    // as if synced: the mirror gets it
    val m = r.mirror / "c1_p1__aliases.git"
    val _ = git(r.mirror, "init", "-q", "--bare", m.toString)
    val _ = git(d, "push", "-q", m.toString, "master:refs/heads/master")
    val again = State(root / "ws").track(pubs.aliases(c1, p1))
    assertEquals(again.value.base, Some(commit))
    assert(!ob.record(again))
    assertEquals(ob.pending, Seq())
    assert(!os.exists(d))
  }

  test("a repo with nothing in it: made, but nothing pushed (as r2)") {
    val root = TempDirs.dir()
    val (r, pubs, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val p = State(root / "ws").track(pubs.submitted_tests(c1, p1))
    assertEquals(p.data.id, Outbox.empty_tree)
    assert(ob.record(p))
    assertEquals(ob.pending, Seq("c1_p1__submitted_tests"))
    assert(os.proc("git", "rev-parse", "--verify", "-q", "master")
      .call(cwd = ob.dir("c1_p1__submitted_tests"), check = false).exitCode != 0)
    // once the mirror has it (empty), nothing to do
    val _ = git(r.mirror, "init", "-q", "--bare", "c1_p1__submitted_tests.git")
    assert(!ob.record(State(root / "ws").track(pubs.submitted_tests(c1, p1))))
    assertEquals(ob.pending, Seq())
  }

  test("a new repo starts where its fork does: on the fork source's master, with its files") {
    val root = TempDirs.dir()
    val (r, pubs, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "s1.cc" -> "mine", "s1.ok" -> "out")()
    val start = r.commit("empty", test_cutoff.minusDays(60), ".gitignore" -> "*.o\n")()
    val s = State(root / "ws")
    val p = s.track(pubs.submitted_tests(c1, p1))
    assertEquals((p.value.exists, p.value.base), (false, Some(start)))
    assertEquals(files(s.checkout(p.data)), Seq(".gitignore", "001.cc", "001.ok"))
    assert(ob.record(p))
    val d = ob.dir("c1_p1__submitted_tests")
    assertEquals(git(d, "rev-parse", "master^"), start)
    // nothing but the fork's files: the fork is all there is to do
    val q = s.track(pubs.aliases(c1, p1))
    assert(ob.record(q.copy(data = ag.r3.Tree(git(r.mirror / "empty.git", "rev-parse", "master^{tree}")))))
    assert(os.proc("git", "rev-parse", "--verify", "-q", "master")
      .call(cwd = ob.dir("c1_p1__aliases"), check = false).exitCode != 0)
  }

  test("the override repo: made once, from the project's tests and Makefiles") {
    val root = TempDirs.dir()
    val (r, pubs, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30),
      "main.c" -> "x", "t0.cc" -> "a test", "t0.ok" -> "its output", "Makefile" -> "all:",
      "sub/Makefile" -> "sub:", "sub/t1.cc" -> "not at the top", ".hidden.cc" -> "dot")()
    val s = State(root / "ws")
    val p = s.track(pubs.override_repo(c1, p1))
    assertEquals(
      files(s.checkout(p.data)),
      Seq(".initialized", "Makefile", "sub/Makefile", "t0.cc", "t0.ok")
    )
    assertEquals(p.value.create, Some(Create("empty", Seq("@all"), Seq("@c1_staff"))))
    // once it's there, the staff's changes are theirs
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(29), ".initialized" -> "", "t0.ok" -> "changed")()
    val again = State(root / "ws").track(pubs.override_repo(c1, p1))
    assert(!ob.record(again))
  }

  test("the override repo the staff made first: the project's tests and Makefiles added, theirs kept") {
    val root = TempDirs.dir()
    val (r, pubs, _) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x", "t0.cc" -> "a test", "Makefile" -> "all:")()
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(29), "Makefile" -> "theirs", "t9.cc" -> "theirs")()
    val s = State(root / "ws")
    val p = s.track(pubs.override_repo(c1, p1))
    val dir = s.checkout(p.data)
    assertEquals(files(dir), Seq(".initialized", "Makefile", "t0.cc", "t9.cc"))
    assertEquals(os.read(dir / "Makefile"), "theirs")
  }

  test("aliases: a published alias stays, even without a submission; new ones go on from the highest") {
    val root = TempDirs.dir()
    val (r, pubs, _) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "main.c" -> "mine")()
    val _ = r.commit("c1_p1__aliases", test_cutoff, "aliases.json" -> upickle.default.write(SortedMap(CSID("gone") -> Alias(7))))()
    val s = State(root / "ws")
    val p = s.track(pubs.aliases(c1, p1))
    assertEquals(
      upickle.default.read[SortedMap[CSID, Alias]](os.read(s.checkout(p.data) / "aliases.json")),
      SortedMap(CSID("gone") -> Alias(7), s1 -> Alias(8))
    )
  }

  test("tests: the students' by alias and the override repo's; submitted tests: only the students'") {
    val root = TempDirs.dir()
    val (r, pubs, _) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "s1.cc" -> "mine", "s1.ok" -> "my output")()
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(30), "t0.cc" -> "a test", "t0.ok" -> "", "Makefile" -> "")()
    val _ = r.commit("c1_p1__tests", test_cutoff.minusDays(30), "old.cc" -> "gone", ".keep" -> "kept")()
    val s = State(root / "ws")
    assertEquals(
      files(s.checkout(s.track(pubs.tests(c1, p1)).data)),
      Seq(".keep", "001.cc", "001.ok", "t0.cc", "t0.ok")
    )
    assertEquals(
      files(s.checkout(s.track(pubs.submitted_tests(c1, p1)).data)),
      Seq("001.cc", "001.ok")
    )
  }
}
