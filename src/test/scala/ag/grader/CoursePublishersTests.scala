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

  // telling students, copying staff member s9
  private val tell = NotificationConfig(true, Some(CSID("s9")), true, false, true, None)

  private def setup(root: os.Path, keys: (String, String)*) = setup_with(root, quiet, keys*)

  private def setup_with(root: os.Path, n: NotificationConfig, keys: (String, String)*) = {
    val (r, subs) = Fixture.setup(root, notifications = n)
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

  test("a key added or changed is told about, as the course says; an unchanged one isn't") {
    val root = os.temp.dir()
    val (r, pubs, _) = setup_with(root, tell, me.value -> "ssh-ed25519 new")
    val _ = r.commit("gitolite-admin", test_cutoff, "conf/gitolite.conf" -> "repo x\n")()
    val p = State(root / "ws").track(pubs.keys)
    assertEquals(
      p.value.notices,
      Seq(Notice(me, Some(CSID("s9")), Notice.What.KeyUpdated("c1", me)))
    )
    // once it's there, nothing to tell
    val _ = r.commit("gitolite-admin", test_cutoff, s"keydir/${me.value}.pub" -> "ssh-ed25519 new")()
    assertEquals(State(root / "ws").track(pubs.keys).value.notices, Seq())
    // the course doesn't tell about keys
    val root2 = os.temp.dir()
    val (r2, pubs2, _) = setup_with(root2, tell.copy(key_update = false), me.value -> "k")
    val _ = r2.commit("gitolite-admin", test_cutoff, "conf/gitolite.conf" -> "repo x\n")()
    assertEquals(State(root2 / "ws").track(pubs2.keys).value.notices, Seq())
  }

  test("a work repo that's made: the student is told, with the project's README") {
    val root = os.temp.dir()
    val (r, pubs, _) = setup_with(root, tell)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "README" -> "read me")()
    val p = State(root / "ws").track(pubs.work_repo("c1", "p1", s1))
    assertEquals(
      p.value.notices,
      Seq(Notice(s1, Some(CSID("s9")), Notice.What.RepoCreated("c1_p1_s1", "read me")))
    )
    // once it's there, nothing to tell
    val _ = r.commit("c1_p1_s1", test_cutoff, "main.c" -> "mine")()
    assertEquals(State(root / "ws").track(pubs.work_repo("c1", "p1", s1)).value.notices, Seq())
  }

  test("who's told: the student (copying cc), just cc, or nobody") {
    val what = Notice.What.KeyUpdated("c1", s1)
    val cc = CSID("s9")
    assertEquals(Notice.routed(tell, s1, what), Seq(Notice(s1, Some(cc), what)))
    assertEquals(Notice.routed(tell, cc, what), Seq(Notice(cc, None, what)))
    assertEquals(Notice.routed(tell.copy(cc = None), s1, what), Seq(Notice(s1, None, what)))
    assertEquals(Notice.routed(tell.copy(send_to_student = false), s1, what), Seq(Notice(cc, None, what)))
    assertEquals(Notice.routed(quiet, s1, what), Seq())
  }

  test("the emails' words (as NotificationConfig's)") {
    val server = RemoteServer("git", "git.example.com", 22)
    val key = Notice(s1, None, Notice.What.KeyUpdated("c1", s1))
    assertEquals(key.subject(server), "key updated for c1::s1")
    assert(key.body(server).contains(s"ssh ${server.ssh_uri} info"))
    val repo = Notice(s1, None, Notice.What.RepoCreated("c1_p1_s1", "read me"))
    assertEquals(repo.subject(server), s"git clone ${server.git_uri("c1_p1_s1")}")
    assert(repo.body(server).startsWith("read me\n\nTo clone:\n    git clone "))
    // nothing is sent unless config.json allows it
    val said = scala.collection.mutable.Buffer[String]()
    repo.send(server, can_send = false, said += _)
    assertEquals(said.toSeq, Seq(s"not mailing (can_send_mail) s1: ${repo.subject(server)}"))
  }

  test("a key file owned by neither its student nor the keeper fails") {
    val root = os.temp.dir()
    val (_, pubs, _) = setup(root, "someone_else" -> "k")
    val strict = Dropbox(pubs.dropbox.root, keeper = "nobody_here")
    intercept[Exception](State(root / "ws").track(strict.keys("c1")))
  }

  test("a course without a directory in the dropbox has no keys") {
    val root = os.temp.dir()
    val (_, pubs, _) = setup(root, s1.value -> "k")
    assertEquals(State(root / "ws").track(pubs.dropbox.keys("c2")), SortedMap[CSID, String]())
  }

  test("the keeper (by default, the grader's user) can own anyone's key file") {
    val root = os.temp.dir()
    val (_, pubs, _) = setup(root, "someone_else" -> "k")
    assertEquals(
      State(root / "ws").track(pubs.dropbox.keys("c1")),
      SortedMap(CSID("someone_else") -> "k")
    )
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

  test("enrollment: left alone if the dropbox has no directory for the course") {
    val root = os.temp.dir()
    val (r, pubs, ob) = setup(root)
    val _ = r.commit("c1__enrollment", test_cutoff, "enrollment.json" -> "[]")()
    os.remove.all(pubs.dropbox.root.get / "c1")
    assert(!ob.record(State(root / "ws").track(pubs.enrollment("c1"))))
    assertEquals(State(root / "ws").track(pubs.dropbox.keys("c1")), SortedMap[CSID, String]())
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
