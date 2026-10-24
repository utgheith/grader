package ag.events

import munit.FunSuite

class ViewTests extends FunSuite {
  private final class Course {
    val dir: os.Path = os.temp.dir(deleteOnExit = true)
    val log: FileEventLog = FileEventLog(dir / "private.jsonl", PrivateLog.kinds)
    given keys: KeyStore = KeyStore(dir / "keys.json")
    val checkpoint: os.Path = dir / "view.json"
    def view(): View[CourseState] = CourseState.view(log, Some(checkpoint))
  }

  // a log that counts what's read from it
  private final class Counting(log: EventLog) extends EventLog {
    var read_events = 0
    def append(events: Seq[Event]): Seq[Logged] = log.append(events)
    def append_if(expected_end: Long, events: Seq[Event]) =
      log.append_if(expected_end, events)
    def read(from: Long): Iterator[Logged] =
      log.read(from).tapEach(_ => read_events += 1)
    def end: Long = log.end
  }

  test("a view reads each event once, catching up as the log grows") {
    val c = Course()
    val log = Counting(c.log)
    val view = CourseState.view(log)
    val _ = log.append(Seq(RepoChanged("a", Some("1"))))
    assertEquals(view.state.repos, Map[RepoKey, Option[String]](RepoKey.Shared("a") -> Some("1")))
    val _ = log.append(Seq(RepoChanged("a", Some("2")), RepoChanged("b", None)))
    assertEquals(
      view.state.repos,
      Map[RepoKey, Option[String]](
        RepoKey.Shared("a") -> Some("2"),
        RepoKey.Shared("b") -> None
      )
    )
    assertEquals(log.read_events, 3)
  }

  test("a checkpoint picks up where the last run left off") {
    val c = Course()
    import c.given
    val ada = Identities(c.view()).student_for("ada123")
    val log = Counting(c.log)
    val _ = log.append(Seq(RepoChanged("a", Some("1"))))
    val again = CourseState.view(log, Some(c.checkpoint))
    assertEquals(again.state.by_csid, Map("ada123" -> ada))
    assertEquals(log.read_events, 1, "only what came after the checkpoint")
    assert(!os.read(c.checkpoint).contains("ada123"), "nothing in the clear")
    assertEquals(os.perms(c.checkpoint).toString, "rw-------")
  }

  test("shredding reaches into checkpoints") {
    val c = Course()
    import c.given
    val ada = Identities(c.view()).student_for("ada123")
    c.keys.shred(ada)
    val view = c.view()
    assertEquals(view.state.students.keySet, Set(ada), "still there, but")
    assertEquals(view.state.by_csid, Map(), "no longer anyone")
  }

  test("an unreadable checkpoint, or one ahead of the log, is rebuilt") {
    val c = Course()
    import c.given
    val ada = Identities(c.view()).student_for("ada123")
    os.write.over(c.checkpoint, "{nonsense")
    assertEquals(c.view().state.by_csid, Map("ada123" -> ada))
    // a checkpoint of another, longer log
    val other = Course()
    val _ = other.log.append(Seq.fill(3)(RepoChanged("x", None)))
    val _ = other.view().state
    os.copy.over(other.checkpoint, c.checkpoint)
    assertEquals(c.view().state.by_csid, Map("ada123" -> ada))
  }
}
