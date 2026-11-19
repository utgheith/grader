package ag.grader.repos

import ag.grader.name.{Alias, CSID}

import ag.grader.{Fixture, Outbox}

import ag.grader.gitolite.Create

import ag.common.TempDirs
import ag.common.given_ReadWriter_SortedMap
import ag.rules.State
import munit.FunSuite

import scala.collection.SortedMap

// The repos the grader publishes (repos) and the outbox they're recorded in (Outbox),
// on made-up repos (see Fixture)
class PublishersTests extends FunSuite {
  import ag.grader.Fixture.*

  private def setup(root: os.Path) = {
    val (r, source) = Fixture.setup(root)
    val ob = Outbox(root / "outbox", r.mirror, root / "ws" / "r3.git")
    (r, source, ob)
  }

  private def files(dir: os.Path): Seq[String] =
    os.walk(dir).filter(os.isFile(_)).map(_.relativeTo(dir).toString).sorted

  private def git(dir: os.Path, args: String*): String =
    os.proc("git" +: args).call(cwd = dir).out.text().trim

  test(
    "what's pending is stale once the branch moves off its base on the mirror, and can be dropped"
  ) {
    val root = TempDirs.dir()
    val (r, source, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit("c1_s1:p1", test_cutoff.minusDays(1), "main.c" -> "mine")()
    val key = "c1/grader/p1/aliases"
    def latest = State(root / "ws").track(AliasesRepo(Fixture.project(source)).latest)
    assert(ob.record(latest))
    // no such repo, or branch, on the mirror: it can still be pushed
    assert(!ob.is_stale(key))
    val m = r.mirror / "c1.git"
    val _ = git(r.mirror, "init", "-q", "--bare", m.toString)
    assert(!ob.is_stale(key))
    // someone else made the branch: it can't
    val other = root / "other"
    val _ = git(root, "init", "-q", other.toString)
    os.write(other / "theirs.txt", "1")
    val _ = git(other, "add", ".")
    val _ = git(other, "-c", "user.name=x", "-c", "user.email=x@x", "commit", "-q", "-m", "theirs")
    val _ = git(other, "push", "-q", m.toString, "HEAD:refs/heads/grader/p1/aliases")
    assert(ob.is_stale(key))
    // worked out again, on what's there now: it can
    assert(ob.record(latest))
    assert(!ob.is_stale(key))
    // and the branch moves again
    os.write.over(other / "theirs.txt", "2")
    val _ = git(other, "-c", "user.name=x", "-c", "user.email=x@x", "commit", "-q", "-am", "again")
    val _ = git(other, "push", "-q", m.toString, "HEAD:refs/heads/grader/p1/aliases")
    assert(ob.is_stale(key))
    ob.drop(key)
    assertEquals(ob.pending, Seq())
  }

  test(
    "a new branch: a commit with no parent; once it's on the mirror, nothing to push"
  ) {
    val root = TempDirs.dir()
    val (r, source, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit("c1_s1:p1", test_cutoff.minusDays(1), "main.c" -> "mine")()
    val p =
      State(root / "ws").track(AliasesRepo(Fixture.project(source)).latest)
    assertEquals(p.value.base, None)
    assertEquals(p.value.create, Some(Create("empty", Seq("@c1_staff"), Seq("@c1_staff"))))
    assert(ob.record(p))
    assertEquals(ob.pending, Seq("c1/grader/p1/aliases"))
    // recording it again changes nothing
    val d = ob.dir("c1/grader/p1/aliases")
    val commit = git(d, "rev-parse", "master")
    assert(ob.record(p))
    assertEquals(git(d, "rev-parse", "master"), commit)
    assertEquals(git(d, "rev-list", "--parents", "-n", "1", "master"), commit)
    assertEquals(
      upickle.default
        .read[SortedMap[CSID, Alias]](git(d, "show", "master:aliases.json")),
      SortedMap(s1 -> Alias(1))
    )

    // as if synced: the mirror gets it
    val m = r.mirror / "c1.git"
    val _ = git(r.mirror, "init", "-q", "--bare", m.toString)
    val _ = git(d, "push", "-q", m.toString, "master:refs/heads/grader/p1/aliases")
    val again =
      State(root / "ws").track(AliasesRepo(Fixture.project(source)).latest)
    assertEquals(again.value.base, Some(commit))
    assert(!ob.record(again))
    assertEquals(ob.pending, Seq())
    assert(!os.exists(d))
  }

  test(
    "a branch with nothing in it: nothing to do (it can't be there empty; its repo is made for a branch with something)"
  ) {
    val root = TempDirs.dir()
    val (r, source, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val p = State(root / "ws").track(
      SubmittedTestsRepo(Fixture.project(source)).latest
    )
    assertEquals(p.data.id, Outbox.empty_tree)
    assertEquals((p.value.exists, p.value.base), (false, None))
    assert(!ob.record(p))
    assertEquals(ob.pending, Seq())
  }

  test(
    "a new branch of a shared repo doesn't start where the repo's fork does: just its files, with no parent"
  ) {
    val root = TempDirs.dir()
    val (r, source, ob) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit(
      "c1_s1:p1",
      test_cutoff.minusDays(1),
      "s1.cc" -> "mine",
      "s1.ok" -> "out"
    )()
    val _ =
      r.commit("empty", test_cutoff.minusDays(60), ".gitignore" -> "*.o\n")()
    val s = State(root / "ws")
    val p = s.track(SubmittedTestsRepo(Fixture.project(source)).latest)
    assertEquals((p.value.exists, p.value.base), (false, None))
    assertEquals(files(s.checkout(p.data)), Seq("001.cc", "001.ok"))
    assert(ob.record(p))
    val d = ob.dir("c1/grader/p1/submitted_tests")
    val commit = git(d, "rev-parse", "master")
    assertEquals(git(d, "rev-list", "--parents", "-n", "1", "master"), commit)
  }

  test("the override repo: made once, from the project's tests and Makefiles") {
    val root = TempDirs.dir()
    val (r, source, ob) = setup(root)
    val _ = r.commit(
      "c1_p1",
      test_cutoff.minusDays(30),
      "main.c" -> "x",
      "t0.cc" -> "a test",
      "t0.ok" -> "its output",
      "Makefile" -> "all:",
      "sub/Makefile" -> "sub:",
      "sub/t1.cc" -> "not at the top",
      ".hidden.cc" -> "dot"
    )()
    val s = State(root / "ws")
    val p = s.track(OverrideRepo(Fixture.project(source)).latest)
    assertEquals(
      files(s.checkout(p.data)),
      Seq(".initialized", "Makefile", "sub/Makefile", "t0.cc", "t0.ok")
    )
    assertEquals(
      p.value.create,
      Some(Create("empty", Seq("@c1_staff"), Seq("@c1_staff")))
    )
    // once it's there, the staff's changes are theirs
    val _ = r.commit(
      "c1:p1/override",
      test_cutoff.minusDays(29),
      ".initialized" -> "",
      "t0.ok" -> "changed"
    )()
    val again =
      State(root / "ws").track(OverrideRepo(Fixture.project(source)).latest)
    assert(!ob.record(again))
  }

  test(
    "the override repo the staff made first: the project's tests and Makefiles added, theirs kept"
  ) {
    val root = TempDirs.dir()
    val (r, source, _) = setup(root)
    val _ = r.commit(
      "c1_p1",
      test_cutoff.minusDays(30),
      "main.c" -> "x",
      "t0.cc" -> "a test",
      "Makefile" -> "all:"
    )()
    val _ = r.commit(
      "c1:p1/override",
      test_cutoff.minusDays(29),
      "Makefile" -> "theirs",
      "t9.cc" -> "theirs"
    )()
    val s = State(root / "ws")
    val p = s.track(OverrideRepo(Fixture.project(source)).latest)
    val dir = s.checkout(p.data)
    assertEquals(files(dir), Seq(".initialized", "Makefile", "t0.cc", "t9.cc"))
    assertEquals(os.read(dir / "Makefile"), "theirs")
  }

  test(
    "aliases: a published alias stays, even without a submission; new ones go on from the highest"
  ) {
    val root = TempDirs.dir()
    val (r, source, _) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit("c1_s1:p1", test_cutoff.minusDays(1), "main.c" -> "mine")()
    val _ = r.commit(
      "c1:grader/p1/aliases",
      test_cutoff,
      "aliases.json" -> upickle.default.write(
        SortedMap(CSID("gone") -> Alias(7))
      )
    )()
    val s = State(root / "ws")
    val p = s.track(AliasesRepo(Fixture.project(source)).latest)
    assertEquals(
      upickle.default.read[SortedMap[CSID, Alias]](
        os.read(s.checkout(p.data) / "aliases.json")
      ),
      SortedMap(CSID("gone") -> Alias(7), s1 -> Alias(8))
    )
  }

  test(
    "tests: the students' by alias and the override repo's; submitted tests: only the students'"
  ) {
    val root = TempDirs.dir()
    val (r, source, _) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val _ = r.commit(
      "c1_s1:p1",
      test_cutoff.minusDays(1),
      "s1.cc" -> "mine",
      "s1.ok" -> "my output"
    )()
    val _ = r.commit(
      "c1:p1/override",
      test_cutoff.minusDays(30),
      "t0.cc" -> "a test",
      "t0.ok" -> "",
      "Makefile" -> ""
    )()
    val _ = r.commit(
      "c1:grader/p1/tests",
      test_cutoff.minusDays(30),
      "old.cc" -> "gone",
      ".keep" -> "kept"
    )()
    val s = State(root / "ws")
    assertEquals(
      files(
        s.checkout(s.track(TestsRepo(Fixture.project(source)).latest).data)
      ),
      Seq(".keep", "001.cc", "001.ok", "t0.cc", "t0.ok")
    )
    assertEquals(
      files(
        s.checkout(
          s.track(SubmittedTestsRepo(Fixture.project(source)).latest).data
        )
      ),
      Seq("001.cc", "001.ok")
    )
  }
}
