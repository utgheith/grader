package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.State
import munit.FunSuite

import scala.collection.SortedMap

// The courses' publishers (keys, enrollment, grades, site) and the students'
// work repos, on made-up repos (see Fixture)
class CoursePublishersTests extends FunSuite {
  import Fixture.*

  // the dropbox's files must be owned by the student they're named for
  private val me = CSID(System.getProperty("user.name").nn)

  private def setup(root: os.Path, keys: (String, String)*) = {
    val (r, subs) = Fixture.setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val box = root / "dropbox"
    os.makeDir.all(box / "c1")
    for ((name, key) <- keys) os.write(box / "c1" / s"$name.pub", key + "\n")
    val pubs = Publishers(Grading(TestRuns(subs)), Dropbox(Some(box)))
    val ob = Outbox(root / "outbox", r.mirror, root / "ws" / "r3.git")
    (r, pubs, ob)
  }

  private def files(dir: os.Path): Seq[String] =
    os.walk(dir).filter(os.isFile(_)).map(_.relativeTo(dir).toString).sorted

  test("keys: added or changed in gitolite-admin, never removed; never made") {
    val root = os.temp.dir()
    val (r, pubs, _) = setup(root, me.value -> "ssh-ed25519 new")
    val _ = r.commit("gitolite-admin", test_cutoff, "conf/gitolite.conf" -> "repo x\n",
      s"keydir/${me.value}.pub" -> "ssh-ed25519 old", "keydir/gone.pub" -> "k")()
    val s = State(root / "ws")
    val p = s.track(pubs.keys)
    assertEquals((p.value.exists, p.value.create), (true, None))
    val dir = s.checkout(p.data)
    assertEquals(files(dir), Seq("conf/gitolite.conf", "keydir/gone.pub", s"keydir/${me.value}.pub").sorted)
    assertEquals(os.read(dir / "keydir" / s"${me.value}.pub"), "ssh-ed25519 new")
  }

  test("a key file not owned by its student fails") {
    val root = os.temp.dir()
    val (_, pubs, _) = setup(root, "someone_else" -> "k")
    intercept[Exception](State(root / "ws").track(pubs.dropbox.keys("c1")))
  }

  test("enrollment: the dropbox's students and keys") {
    val root = os.temp.dir()
    val (_, pubs, _) = setup(root, me.value -> "ssh-ed25519 mine")
    val s = State(root / "ws")
    val p = s.track(pubs.enrollment("c1"))
    assertEquals(
      upickle.default.read[SortedMap[CSID, String]](os.read(s.checkout(p.data) / "enrollment.json")),
      SortedMap(me -> "ssh-ed25519 mine")
    )
    assertEquals(p.value.create, Some(Create("empty", Seq("@c1_staff"), Seq())))
  }

  test("grades: made with a README, then left to the staff") {
    val root = os.temp.dir()
    val (r, pubs, ob) = setup(root)
    val s = State(root / "ws")
    val p = s.track(pubs.grades("c1"))
    assertEquals(files(s.checkout(p.data)), Seq("README.md"))
    assert(os.read(s.checkout(p.data) / "README.md").contains("# Grading data for `c1`"))
    assertEquals(p.value.create, Some(Create("empty", Seq("@c1_staff"), Seq("@c1_staff"))))
    // once it's there, their changes stand
    val _ = r.commit("c1__grades", test_cutoff, "grades.csv" -> "x")()
    assert(!ob.record(State(root / "ws").track(pubs.grades("c1"))))
  }

  test("a work repo: a fork of the project's repo, nothing pushed") {
    val root = os.temp.dir()
    val (r, pubs, ob) = setup(root)
    val p = State(root / "ws").track(pubs.work_repo("c1", "p1", s1))
    assertEquals(p.value.create, Some(Create("c1_p1", Seq("s1", "@c1_staff"), Seq("s1"))))
    assert(ob.record(p))
    assert(
      os.proc("git", "rev-parse", "--verify", "-q", "master")
        .call(cwd = ob.dir("c1_p1_s1"), check = false).exitCode != 0
    )
    // once made, nothing: the student's commits are theirs
    val _ = r.commit("c1_p1_s1", test_cutoff, "main.c" -> "mine")()
    assert(!ob.record(State(root / "ws").track(pubs.work_repo("c1", "p1", s1))))
  }

  // a commit of `name` adding a submodule entry (a gitlink) at `path`
  private def add_submodule(r: Repos, root: os.Path, name: String, path: String): Unit = {
    val w = root / "work" / name
    val _ = git(w, Map(), "update-index", "--add", "--cacheinfo", s"160000,${"1" * 40},$path")
    val _ = git(w, Map(), "commit", "-q", "-m", "a submodule")
    r.sync(name)
  }

  test("a submodule: a work repo is left alone; a commit that would drop one isn't made") {
    val root = os.temp.dir()
    val (r, pubs, ob) = setup(root)
    val _ = r.commit("c1_p1_s1", test_cutoff, "main.c" -> "mine")()
    add_submodule(r, root, "c1_p1_s1", "sub")
    assert(!ob.record(State(root / "ws").track(pubs.work_repo("c1", "p1", s1))))
    // the grades repo is copied to work it out, which loses the submodule
    val _ = r.commit("c1__grades", test_cutoff, "grades.csv" -> "x")()
    add_submodule(r, root, "c1__grades", "sub")
    val e = intercept[Exception](ob.record(State(root / "ws").track(pubs.grades("c1"))))
    assert(e.getMessage.contains("submodule"), e.getMessage)
    assertEquals(ob.pending, Seq())
  }

  test("site: a page per phase and the front page; old pages stay, unchanged pages aren't rewritten") {
    val root = os.temp.dir()
    val (r, pubs, ob) = setup(root)
    val _ = r.commit("c1__site", test_cutoff, "c1_p0.html" -> "an old project's page")()
    val s = State(root / "ws")
    val p = s.track(pubs.site("c1"))
    val dir = s.checkout(p.data)
    assertEquals(
      files(dir),
      Seq("c1_p0.html", "c1_p1.html", "c1_p1_phase1.html", "c1_p1_phase2.html", "index.html")
    )
    val index = os.read(dir / "index.html")
    assert(index.contains("c1_p0.html") && index.contains("c1_p1.html"))
    // published as is: worked out again, the same pages (generated times and all)
    val m = r.mirror / "c1__site.git"
    assert(ob.record(p))
    val _ = os.proc("git", "push", "-q", m.toString, "master:refs/heads/master").call(cwd = ob.dir("c1__site"))
    Thread.sleep(1100)
    assert(!ob.record(State(root / "ws").track(pubs.site("c1"))))
  }
}
