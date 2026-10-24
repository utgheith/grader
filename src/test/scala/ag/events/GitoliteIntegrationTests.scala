package ag.events

import munit.FunSuite

// Against a real gitolite (in a container; see GitoliteServer). Tagged
// "integration"; skipped without podman. Run just these with
//   testOnly ag.events.GitoliteIntegrationTests
class GitoliteIntegrationTests extends FunSuite {
  private val Integration = new munit.Tag("integration")

  // The real server's rules (users and groups made up): `@me` (the grader)
  // can do anything, course staff can create and write any repo of their
  // course, and `perms` makes others READERS or WRITERS of a repo.
  private val conf =
    """@me = admin
      |@admins = admin
      |@cs439c_f26_staff = admin ta
      |
      |repo @all
      |    RW+    = @me
      |    C      = @me
      |    RW     = @admins
      |    R      = READERS
      |    RW     = WRITERS
      |
      |repo grader
      |    R = @me
      |
      |repo empty
      |    R = @all
      |
      |repo cs439c_f26_.*
      |    RW = @cs439c_f26_staff
      |    C = @cs439c_f26_staff
      |""".stripMargin

  private var started: Option[GitoliteServer] = None
  private def server: GitoliteServer = started.getOrElse {
    assume(GitoliteServer.image.isDefined, "podman isn't available")
    val s = GitoliteServer(conf, Seq("ta", "ada123"))
    started = Some(s)
    s
  }
  override def afterAll(): Unit = started.foreach(_.close())

  private val kinds = EventKinds(
    StudentIdentity.kind,
    KeySubmitted.kind,
    RepoChanged.kind,
    StudentRepoChanged.kind
  )

  private def private_log(user: String): GitEventLog =
    GitEventLog(
      server.url("cs439c_f26__private"),
      os.temp.dir(deleteOnExit = true) / "checkout",
      kinds,
      env = server.git_env(user)
    )

  test(
    "observe: every repo and its latest pushed sha, and no patterns".tag(
      Integration
    )
  ) {
    val sha = server.push("admin", "cs439c_f26_p1", "README", "starter")
    // created by fork (as the grader does), never pushed to
    val forked = server.run("admin", "fork", "empty", "cs439c_f26_p2")
    assertEquals(forked.exitCode, 0, forked.err.text())
    val observed =
      Gitolite.observe(server.server, server.port, server.ssh("admin"))
    assertEquals(observed.get("cs439c_f26_p1"), Some(Some(sha)))
    assertEquals(
      observed.get("cs439c_f26_p2"),
      Some(None),
      "no history line yet"
    )
    assert(observed.keys.forall(!_.contains("*")), observed.keys)
  }

  test(
    "observe fails for a user who can't read the grader repo".tag(Integration)
  ) {
    val _ = server.push("admin", "cs439c_f26_p1", "README", "again")
    val _ = intercept[EventLogException](
      Gitolite.observe(server.server, server.port, server.ssh("ta"))
    )
  }

  test(
    "the private log: staff read and write it, students can't read it".tag(
      Integration
    )
  ) {
    val grader = private_log("admin")
    val _ = grader.append(Seq(RepoChanged("cs439c_f26_p1", Some("abc"))))
    assertEquals(private_log("ta").read().size, 1)
    val _ = intercept[EventLogException](private_log("ada123").read().toList)
  }

  test(
    "the private log over ssh: racing appenders all land, once".tag(Integration)
  ) {
    val start = private_log("admin").end
    val offsets = (1 to 3)
      .map(i =>
        ag.task.Task(
          (1 to 3).flatMap(j =>
            private_log("admin")
              .append(Seq(RepoChanged(s"r$i", Some(s"$j"))))
              .map(_.offset)
          )
        )
      )
      .flatMap(_.block)
    assertEquals(offsets.sorted, (start until start + 9).toSeq)
  }

  test("perms gives a student access to their repo".tag(Integration)) {
    val repo = "cs439c_f26_p3_ada123"
    val _ = server.push("admin", repo, "README", "your work")
    assert(
      server
        .run("ada123", "info")
        .out
        .text()
        .split("\n")
        .forall(!_.endsWith(repo))
    )
    val perms = server.run("admin", "perms", repo, "+", "WRITERS", "ada123")
    assertEquals(perms.exitCode, 0, perms.err.text())
    assert(server.run("ada123", "info").out.text().contains(repo))
    val _ = server.push("ada123", repo, "solution", "mine")
  }

  test(
    "end to end: a student's push becomes a StudentRepoChanged".tag(Integration)
  ) {
    val dir = os.temp.dir(deleteOnExit = true)
    val log = FileEventLog(dir / "private.jsonl", kinds)
    given KeyStore = KeyStore(dir / "keys.json")
    val ada = Identities(CourseState.view(log)).student_for("ada123")
    val sha = server.push("admin", "cs439c_f26_p4_ada123", "README", "start")
    val (appended, unknown) = GitWatcher("cs439c_f26", CourseState.view(log))
      .poll(Gitolite.observe(server.server, server.port, server.ssh("admin")))
    assert(
      appended
        .map(_.event)
        .contains(StudentRepoChanged("p4", ada, StudentRepo.Work, Some(sha))),
      appended.map(_.event)
    )
    assertEquals(unknown, 0)
  }

  test(
    "a push records HEAD in history and notes the push time on it".tag(
      Integration
    )
  ) {
    val before = System.currentTimeMillis() / 1000
    val sha = server.push("admin", "cs439c_f26_p5", "README", "x")
    val observed =
      Gitolite.observe(server.server, server.port, server.ssh("admin"))
    assertEquals(observed.get("cs439c_f26_p5"), Some(Some(sha)))

    val work = os.temp.dir(deleteOnExit = true)
    val _ = os.proc("git", "init", "-q").call(cwd = work)
    val _ = os
      .proc(
        "git",
        "fetch",
        "-q",
        server.url("cs439c_f26_p5"),
        "refs/notes/push_time:refs/notes/push_time",
        "master"
      )
      .call(cwd = work, env = server.git_env("admin"))
    val note = os
      .proc("git", "notes", "--ref", "push_time", "show", sha)
      .call(cwd = work)
      .out
      .trim()
      .toLong
    assert(
      note >= before - 5 && note <= System.currentTimeMillis() / 1000 + 5,
      note
    )
  }

  test(
    "a course watch: dropbox and pushes, into the private repo on the server"
      .tag(Integration)
  ) {
    // the dropbox only trusts files owned by the CSID they're named for, so
    // the test student is whoever runs the tests
    val me = System.getProperty("user.name").nn
    val dropbox = os.temp.dir(deleteOnExit = true)
    os.write(dropbox / s"$me.pub", "ssh-ed25519 AAAA test\n")
    val keys = KeyStore(os.temp.dir(deleteOnExit = true) / "cs439c_f26.json")
    def watch(machine: String) = CourseWatch(
      "cs439c_f26",
      GitEventLog(
        server.url("cs439c_f26__private"),
        os.temp.dir(deleteOnExit = true) / machine,
        PrivateLog.kinds,
        env = server.git_env("admin")
      ),
      keys,
      Some(dropbox)
    )
    def observe() =
      Gitolite.observe(server.server, server.port, server.ssh("admin"))

    val start = private_log("admin").end
    val first = watch("a").poll(observe())
    assertEquals(first.keys, 1)

    val _ = server.push("admin", s"cs439c_f26_p7_$me", "README", "start")
    val second = watch("b").poll(observe()) // another machine
    // (other tests' students' repos on this shared server count as unknown)
    assertEquals((second.keys, second.repos >= 1), (0, true))
    assertEquals(
      watch("c").poll(observe()).copy(unknown_repos = 0),
      WatchSummary(0, 0, 0),
      "nothing new"
    )

    // in the server's private repo, readable with the course's keys
    val logged = private_log("admin").read(from = start).map(_.event).toList
    given KeyStore = keys
    val student = Identities(CourseState.view(private_log("admin"))).find(me).get
    assert(
      logged.exists {
        case e: StudentIdentity => e.student == student
        case _                  => false
      },
      "an identity"
    )
    assert(
      logged.exists {
        case StudentRepoChanged("p7", `student`, StudentRepo.Work, Some(_)) =>
          true
        case _ => false
      },
      logged
    )
  }
}
