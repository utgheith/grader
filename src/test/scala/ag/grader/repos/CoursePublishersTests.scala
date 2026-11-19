package ag.grader.repos

import ag.grader.targets.Dropbox

import ag.grader.cloudflare.{CloudflareConfig, SiteDeployment, SiteHost}

import ag.grader.name.{CSID, CourseName}

import ag.grader.{Fixture, Notice, Outbox, RemoteServer}

import ag.grader.json.NotificationConfig

import ag.grader.gitolite.Create

import ag.common.TempDirs
import ag.common.given_ReadWriter_SortedMap
import ag.rules.State
import munit.FunSuite

import scala.collection.SortedMap

// The courses' publishers (keys, enrollment, grades, site) and the students'
// work repos, on made-up repos (see Fixture)
class CoursePublishersTests extends FunSuite {
  import ag.grader.Fixture.*

  // the dropbox's files must be owned by the student they're named for
  private val me = CSID(System.getProperty("user.name").nn)

  // telling students, copying staff member s9
  private val tell =
    NotificationConfig(true, Some(CSID("s9")), true, false, true, None)

  private def setup(root: os.Path, keys: (String, String)*) =
    setup_with(root, quiet, keys*)

  private def setup_with(
      root: os.Path,
      n: NotificationConfig,
      keys: (String, String)*
  ) = {
    val (r, source) = Fixture.setup(root, notifications = n)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val box = root / "dropbox"
    os.makeDir.all(box / "c1")
    for ((name, key) <- keys) os.write(box / "c1" / s"$name.pub", key + "\n")
    val dropbox = Dropbox(Some(box))
    val ob = Outbox(root / "outbox", r.mirror, root / "ws" / "r3.git")
    (r, source, dropbox, ob)
  }

  private def files(dir: os.Path): Seq[String] =
    os.walk(dir).filter(os.isFile(_)).map(_.relativeTo(dir).toString).sorted

  test("keys: added or changed in gitolite-admin, never removed; never made") {
    val root = TempDirs.dir()
    val (r, source, dropbox, _) = setup(root, me.value -> "ssh-ed25519 new")
    val _ = r.commit(
      "gitolite-admin",
      test_cutoff,
      "conf/gitolite.conf" -> "repo x\n",
      s"keydir/${me.value}.pub" -> "ssh-ed25519 old",
      "keydir/gone.pub" -> "k"
    )()
    val s = State(root / "ws")
    val p = s.track(KeysRepo(source, dropbox).latest)
    assertEquals((p.value.exists, p.value.create), (true, None))
    val dir = s.checkout(p.data)
    assertEquals(
      files(dir),
      Seq(
        "conf/gitolite.conf",
        "keydir/gone.pub",
        s"keydir/${me.value}.pub"
      ).sorted
    )
    assertEquals(
      os.read(dir / "keydir" / s"${me.value}.pub"),
      "ssh-ed25519 new"
    )
  }

  test(
    "a key added or changed is told about, as the course says; an unchanged one isn't"
  ) {
    val root = TempDirs.dir()
    val (r, source, dropbox, _) =
      setup_with(root, tell, me.value -> "ssh-ed25519 new")
    val _ = r.commit(
      "gitolite-admin",
      test_cutoff,
      "conf/gitolite.conf" -> "repo x\n"
    )()
    val p = State(root / "ws").track(KeysRepo(source, dropbox).latest)
    assertEquals(
      p.value.notices,
      Seq(Notice(me, Some(CSID("s9")), Notice.What.KeyUpdated(c1, me)))
    )
    // once it's there, nothing to tell
    val _ = r.commit(
      "gitolite-admin",
      test_cutoff,
      s"keydir/${me.value}.pub" -> "ssh-ed25519 new"
    )()
    assertEquals(
      State(root / "ws").track(KeysRepo(source, dropbox).latest).value.notices,
      Seq()
    )
    // the course doesn't tell about keys
    val root2 = TempDirs.dir()
    val (r2, env2, dropbox2, _) =
      setup_with(root2, tell.copy(key_update = false), me.value -> "k")
    val _ = r2.commit(
      "gitolite-admin",
      test_cutoff,
      "conf/gitolite.conf" -> "repo x\n"
    )()
    assertEquals(
      State(root2 / "ws").track(KeysRepo(env2, dropbox2).latest).value.notices,
      Seq()
    )
  }

  test(
    "a work repo that's made: the student is told, with the project's README"
  ) {
    val root = TempDirs.dir()
    val (r, source, _, _) = setup_with(root, tell)
    val _ =
      r.commit("c1_p1", test_cutoff.minusDays(30), "README" -> "read me")()
    val p =
      State(root / "ws").track(WorkRepo(Fixture.submission(source)).latest)
    assertEquals(
      p.value.notices,
      Seq(
        Notice(
          s1,
          Some(CSID("s9")),
          Notice.What.RepoCreated("c1_s1", "p1", "read me")
        )
      )
    )
    // once it's there, nothing to tell
    val _ = r.commit("c1_s1:p1", test_cutoff, "main.c" -> "mine")()
    assertEquals(
      State(root / "ws")
        .track(WorkRepo(Fixture.submission(source)).latest)
        .value
        .notices,
      Seq()
    )
  }

  test("who's told: the student (copying cc), just cc, or nobody") {
    val what = Notice.What.KeyUpdated(c1, s1)
    val cc = CSID("s9")
    assertEquals(Notice.routed(tell, s1, what), Seq(Notice(s1, Some(cc), what)))
    assertEquals(Notice.routed(tell, cc, what), Seq(Notice(cc, None, what)))
    assertEquals(
      Notice.routed(tell.copy(cc = None), s1, what),
      Seq(Notice(s1, None, what))
    )
    assertEquals(
      Notice.routed(tell.copy(send_to_student = false), s1, what),
      Seq(Notice(cc, None, what))
    )
    assertEquals(Notice.routed(quiet, s1, what), Seq())
  }

  test("the emails' words (as NotificationConfig's)") {
    val server = RemoteServer("git", "git.example.com", 22)
    val key = Notice(s1, None, Notice.What.KeyUpdated(c1, s1))
    assertEquals(key.subject(server), "key updated for c1::s1")
    assert(key.body(server).contains(s"ssh ${server.ssh_uri} info"))
    val repo =
      Notice(s1, None, Notice.What.RepoCreated("c1_s1", "p1", "read me"))
    assertEquals(
      repo.subject(server),
      s"git clone -b p1 ${server.git_uri("c1_s1")}"
    )
    assert(repo.body(server).startsWith("read me\n\nTo clone:\n    git clone "))
    // nothing is sent unless config.json allows it
    val said = scala.collection.mutable.Buffer[String]()
    repo.send(server, can_send = false, said += _)
    assertEquals(
      said.toSeq,
      Seq(s"not mailing (can_send_mail) s1: ${repo.subject(server)}")
    )
  }

  test("a key file owned by neither its student nor the keeper fails") {
    val root = TempDirs.dir()
    val (_, _, dropbox, _) = setup(root, "someone_else" -> "k")
    val strict = Dropbox(dropbox.root, keeper = "nobody_here")
    intercept[Exception](State(root / "ws").track(strict.keys(c1)))
  }

  test("a course without a directory in the dropbox has no keys") {
    val root = TempDirs.dir()
    val (_, _, dropbox, _) = setup(root, s1.value -> "k")
    assertEquals(
      State(root / "ws").track(dropbox.keys(c2)),
      SortedMap[CSID, String]()
    )
  }

  test("the keeper (by default, the grader's user) can own anyone's key file") {
    val root = TempDirs.dir()
    val (_, _, dropbox, _) = setup(root, "someone_else" -> "k")
    assertEquals(
      State(root / "ws").track(dropbox.keys(c1)),
      SortedMap(CSID("someone_else") -> "k")
    )
  }

  test("enrollment: the dropbox's students and keys") {
    val root = TempDirs.dir()
    val (_, source, dropbox, _) = setup(root, me.value -> "ssh-ed25519 mine")
    val s = State(root / "ws")
    val p = s.track(EnrollmentRepo(Fixture.course(source), dropbox).latest)
    assertEquals(
      upickle.default.read[SortedMap[CSID, String]](
        os.read(s.checkout(p.data) / "enrollment.json")
      ),
      SortedMap(me -> "ssh-ed25519 mine")
    )
    assertEquals(
      p.value.create,
      Some(Create("empty", Seq("@c1_staff"), Seq("@c1_staff")))
    )
  }

  test(
    "enrollment: left alone if the dropbox has no directory for the course"
  ) {
    val root = TempDirs.dir()
    val (r, source, dropbox, ob) = setup(root)
    val _ = r.commit("c1:grader/enrollment", test_cutoff, "enrollment.json" -> "[]")()
    os.remove.all(dropbox.root.get / "c1")
    assert(
      !ob.record(
        State(root / "ws").track(
          EnrollmentRepo(Fixture.course(source), dropbox).latest
        )
      )
    )
    assertEquals(
      State(root / "ws").track(dropbox.keys(c1)),
      SortedMap[CSID, String]()
    )
  }

  test("grades: made with a README, then left to the staff") {
    val root = TempDirs.dir()
    val (r, source, _, ob) = setup(root)
    val s = State(root / "ws")
    val p = s.track(GradesRepo(Fixture.course(source)).latest)
    assertEquals(files(s.checkout(p.data)), Seq("README.md"))
    assert(
      os.read(s.checkout(p.data) / "README.md")
        .contains("# Grading data for `c1`")
    )
    assertEquals(
      p.value.create,
      Some(Create("empty", Seq("@c1_staff"), Seq("@c1_staff")))
    )
    // once it's there, their changes stand
    val _ = r.commit("c1:grades", test_cutoff, "grades.csv" -> "x")()
    assert(
      !ob.record(
        State(root / "ws").track(GradesRepo(Fixture.course(source)).latest)
      )
    )
  }

  test(
    "a work branch: in the student's repo (forked from empty), it starts as the project's repo, pushed as is"
  ) {
    val root = TempDirs.dir()
    val (r, source, _, ob) = setup(root)
    val p =
      State(root / "ws").track(WorkRepo(Fixture.submission(source)).latest)
    assertEquals(
      (p.value.repo, p.value.branch, p.value.from, p.value.create),
      (
        "c1_s1",
        "p1",
        Some("c1_p1"),
        Some(Create("empty", Seq("s1", "@c1_staff"), Seq("s1")))
      )
    )
    assert(ob.record(p))
    val project = git(r.mirror / "c1_p1.git", Map(), "rev-parse", "master")
    assertEquals(git(ob.dir("c1_s1/p1"), Map(), "rev-parse", "master"), project)
    // once made, nothing: the student's commits are theirs
    val _ = r.commit("c1_s1:p1", test_cutoff, "main.c" -> "mine")()
    assert(
      !ob.record(
        State(root / "ws").track(WorkRepo(Fixture.submission(source)).latest)
      )
    )
  }

  // a commit of `name` adding a submodule entry (a gitlink) at `path`
  private def add_submodule(
      r: Repos,
      root: os.Path,
      name: String,
      path: String
  ): Unit = {
    val w = root / "work" / name.replace(':', '@')
    val _ = git(
      w,
      Map(),
      "update-index",
      "--add",
      "--cacheinfo",
      s"160000,${"1" * 40},$path"
    )
    val _ = git(w, Map(), "commit", "-q", "-m", "a submodule")
    r.sync(name)
  }

  test(
    "a submodule: a work repo is left alone; a commit that would drop one isn't made"
  ) {
    val root = TempDirs.dir()
    val (r, source, _, ob) = setup(root)
    val _ = r.commit("c1_s1:p1", test_cutoff, "main.c" -> "mine")()
    add_submodule(r, root, "c1_s1:p1", "sub")
    assert(
      !ob.record(
        State(root / "ws").track(WorkRepo(Fixture.submission(source)).latest)
      )
    )
    // the grades repo is copied to work it out, which loses the submodule
    val _ = r.commit("c1:grades", test_cutoff, "grades.csv" -> "x")()
    add_submodule(r, root, "c1:grades", "sub")
    val e = intercept[Exception](
      ob.record(
        State(root / "ws").track(GradesRepo(Fixture.course(source)).latest)
      )
    )
    assert(e.getMessage.contains("submodule"), e.getMessage)
    assertEquals(ob.pending, Seq())
  }

  test(
    "site: a page per milestone, the results, and the front page; old pages stay, unchanged pages aren't rewritten"
  ) {
    val root = TempDirs.dir()
    val (r, source, _, ob) = setup(root)
    val _ = r.commit(
      "c1:grader/site",
      test_cutoff,
      "c1_p0.html" -> "an old project's page"
    )()
    val s = State(root / "ws")
    val p = s.track(SiteRepo(Fixture.course(source)).latest)
    val dir = s.checkout(p.data)
    assertEquals(
      files(dir),
      Seq(
        "c1_p0.html",
        "c1_p1--m1.html",
        "c1_p1--m2.html",
        "c1_p1.html",
        "index.html",
        "results/p1/results_live.json",
        "results/p1/results_m1.json",
        "results/p1/results_m2.json"
      )
    )
    val index = os.read(dir / "index.html")
    assert(index.contains("c1_p0.html") && index.contains("c1_p1.html"))
    // published as is: worked out again, the same pages (generated times and all)
    val m = r.mirror / "c1.git"
    assert(ob.record(p))
    val _ = os
      .proc("git", "push", "-q", m.toString, "master:refs/heads/grader/site")
      .call(cwd = ob.dir("c1/grader/site"))
    Thread.sleep(1100)
    assert(
      !ob.record(
        State(root / "ws").track(SiteRepo(Fixture.course(source)).latest)
      )
    )
  }

  test(
    "a site is served as published, when its pages change, and only then; its host is named without secrets"
  ) {
    val root = TempDirs.dir()
    val (r, source, _, _) = setup(root)
    val served = scala.collection.mutable.Buffer[Seq[String]]()
    val host = new SiteHost {
      def name = "test-host"
      def publish(dir: os.Path, c: CourseName) = {
        served += files(dir)
        s"https://$c.example"
      }
    }
    def deployment = SiteDeployment(Fixture.course(source), host)
    val _ = r.commit(
      "c1:grader/site",
      test_cutoff,
      "index.html" -> "the front page",
      ".initialized" -> ""
    )()
    assertEquals(
      State(root / "ws").track(deployment.deployed),
      "https://c1.example"
    )
    // the pages as grade publishes them (what's on the branch, and the
    // pages and results worked out on top), without the repo's dot files
    assertEquals(served.size, 1)
    assert(
      Seq("index.html", "c1_p1.html", "results/p1/results_m1.json")
        .forall(served.head.contains),
      served.head
    )
    assert(!served.head.exists(_.startsWith(".")), served.head)
    // nothing changed: not served again
    val _ = State(root / "ws").track(deployment.deployed)
    assertEquals(served.size, 1)
    // a page published: served again
    val _ = r.commit(
      "c1:grader/site",
      test_cutoff,
      "c1_p0.html" -> "an old project's page"
    )()
    val _ = State(root / "ws").track(deployment.deployed)
    assertEquals(served.size, 2)
    assert(served.last.contains("c1_p0.html"))
    // a Cloudflare host's name: where it serves, not its account or key
    val cf = CloudflareConfig(
      true,
      "the-account",
      "the-key",
      Some("example.net"),
      Some("test-")
    )
    val path = SiteDeployment(Fixture.course(source), cf).deployed.path.toString
    assert(path.contains("example.net") && path.contains("test-"), path)
    assert(!path.contains("the-account") && !path.contains("the-key"), path)
  }
}
