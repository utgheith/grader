package ag.events

import munit.FunSuite

class WatcherTests extends FunSuite {
  private val kinds = EventKinds(
    StudentIdentity.kind,
    KeySubmitted.kind,
    RepoChanged.kind,
    StudentRepoChanged.kind
  )

  // a course's private log and key store
  private final class Course {
    val dir: os.Path = os.temp.dir(deleteOnExit = true)
    val log: FileEventLog = FileEventLog(dir / "private.jsonl", kinds)
    given keys: KeyStore = KeyStore(dir / "keys.json")
    // a new view each time, as a separate process would have
    def ids: Identities = Identities(CourseState.view(log))
    def dropbox = DropboxWatcher(CourseState.view(log))
    def git = GitWatcher("cs439c_f26", CourseState.view(log))
    def events: List[Event] = log.read().map(_.event).toList
  }

  // ---- dropbox

  test("a new key enrolls the student and records the key, sealed") {
    val c = Course()
    val _ = c.dropbox.poll(Map("ada123" -> "ssh-ed25519 AAAA ada"))
    val ada = c.ids.find("ada123").get
    assertEquals(
      c.events.collect { case e: KeySubmitted =>
        e.student -> e.ssh_key.get(using c.keys).get
      },
      List(ada -> "ssh-ed25519 AAAA ada")
    )
    val text = os.read(c.log.file)
    assert(
      !text.contains("ada123") && !text.contains("AAAA"),
      "nothing readable in the log"
    )
  }

  test("the same key again changes nothing; a new key is recorded") {
    val c = Course()
    val _ = c.dropbox.poll(Map("ada123" -> "key-1"))
    assertEquals(c.dropbox.poll(Map("ada123" -> "key-1")), Nil)
    assertEquals(c.dropbox.poll(Map("ada123" -> "key-2")).size, 1)
    assertEquals(c.events.count(_.isInstanceOf[KeySubmitted]), 2)
  }

  test("concurrent dropbox watchers record each key once") {
    val c = Course()
    val observed = Map("ada123" -> "key-a", "bob456" -> "key-b")
    val _ =
      (1 to 6).map(_ => ag.task.Task(c.dropbox.poll(observed))).map(_.block)
    assertEquals(c.events.count(_.isInstanceOf[StudentIdentity]), 2)
    assertEquals(c.events.count(_.isInstanceOf[KeySubmitted]), 2)
  }

  test(
    "the real dropbox only trusts files owned by the CSID they're named for"
  ) {
    val dir = os.temp.dir(deleteOnExit = true)
    val me = System.getProperty("user.name").nn
    os.write(dir / s"$me.pub", "ssh-ed25519 mine\n")
    os.write(
      dir / "someone_else.pub",
      "ssh-ed25519 theirs\n"
    ) // owned by me, not them
    os.write(dir / "notes.txt", "ignored")
    assertEquals(Dropbox.observe(dir), Map(me -> "ssh-ed25519 mine"))
  }

  // ---- git

  test("repo names are read relative to the course") {
    import RepoName.*
    def parse(r: String) = RepoName.parse("cs439c_f26", r)
    assertEquals(
      parse("cs439c_f26__enrollment"),
      Some(Shared("cs439c_f26__enrollment"))
    )
    assertEquals(parse("cs439c_f26_p1"), Some(Shared("cs439c_f26_p1")))
    assertEquals(
      parse("cs439c_f26_p1__tests"),
      Some(Shared("cs439c_f26_p1__tests"))
    )
    assertEquals(
      parse("cs439c_f26_p1_ada123"),
      Some(OfStudent("p1", "ada123", StudentRepo.Work))
    )
    assertEquals(
      parse("cs439c_f26_p1_ada123_results"),
      Some(OfStudent("p1", "ada123", StudentRepo.Results))
    )
    assertEquals(parse("cs439t_f26_p1_ada123"), None, "another course")
    assertEquals(parse("courses_config"), None)
  }

  test("changes are recorded once; students by id, never by CSID") {
    val c = Course()
    val ada = c.ids.student_for("ada123")
    val observed = Map(
      "cs439c_f26__enrollment" -> Some("e1"),
      "cs439c_f26_p1" -> Some("s1"),
      "cs439c_f26_p1_ada123" -> Some("a1"),
      "cs439c_f26_p1_ada123_results" -> None,
      "cs439t_f26_p1_bob456" -> Some("x") // another course: not ours
    )
    val (first, unknown) = c.git.poll(observed)
    assertEquals(unknown, 0)
    assertEquals(
      first.map(_.event).toSet,
      Set[Event](
        RepoChanged("cs439c_f26__enrollment", Some("e1")),
        RepoChanged("cs439c_f26_p1", Some("s1")),
        StudentRepoChanged("p1", ada, StudentRepo.Work, Some("a1")),
        StudentRepoChanged("p1", ada, StudentRepo.Results, None)
      )
    )
    assert(!os.read(c.log.file).contains("ada123"))

    assertEquals(c.git.poll(observed)._1, Nil, "nothing changed")
    val (second, _) =
      c.git.poll(observed.updated("cs439c_f26_p1_ada123", Some("a2")))
    assertEquals(
      second.map(_.event),
      Seq(StudentRepoChanged("p1", ada, StudentRepo.Work, Some("a2")))
    )
  }

  test("a repo of no known student is skipped, and its name never logged") {
    val c = Course()
    val (appended, unknown) =
      c.git.poll(Map("cs439c_f26_p1_stranger" -> Some("z")))
    assertEquals((appended, unknown), (Nil, 1))
    assert(!os.exists(c.log.file) || !os.read(c.log.file).contains("stranger"))
  }

  test("concurrent git watchers record each change once") {
    val c = Course()
    val _ = c.ids.student_for("ada123")
    val observed =
      Map("cs439c_f26_p1" -> Some("s1"), "cs439c_f26_p1_ada123" -> Some("a1"))
    val _ = (1 to 6).map(_ => ag.task.Task(c.git.poll(observed))).map(_.block)
    assertEquals(
      c.events.count(e =>
        e.isInstanceOf[RepoChanged] || e.isInstanceOf[StudentRepoChanged]
      ),
      2
    )
  }
}
