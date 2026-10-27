package ag.events

import upickle.default.ReadWriter

// The kinds of event in a course's private log.
object PrivateLog {
  val kinds: EventKinds = EventKinds(
    StudentIdentity.kind,
    KeySubmitted.kind,
    RepoChanged.kind,
    StudentRepoChanged.kind
  )
}

// What a course's private log says, as of some point in it.
@upickle.implicits.allowUnknownKeys(false)
final case class CourseState(
    // everyone given an identity: their CSID, sealed
    students: Map[StudentId, Box[String]],
    // each student's latest ssh key, sealed
    ssh_keys: Map[StudentId, Box[String]],
    // each repo's latest recorded sha
    repos: Map[RepoKey, Option[String]]
) derives ReadWriter {
  // CSID -> student, for the students not shredded
  def by_csid(using KeyStore): Map[String, StudentId] =
    students.flatMap((id, csid) => csid.get.map(_ -> id))
}

object CourseState {
  val projection: Projection[CourseState] = new Projection[CourseState] {
    def initial: CourseState = CourseState(Map(), Map(), Map())
    def step(state: CourseState, logged: Logged): CourseState =
      logged.event match {
        case e: StudentIdentity =>
          state.copy(students = state.students.updated(e.student, e.csid))
        case e: KeySubmitted =>
          state.copy(ssh_keys = state.ssh_keys.updated(e.student, e.ssh_key))
        case RepoChanged(name, sha) =>
          state.copy(repos = state.repos.updated(RepoKey.Shared(name), sha))
        case StudentRepoChanged(project, id, which, sha) =>
          state.copy(repos =
            state.repos.updated(RepoKey.Student(project, id, which), sha)
          )
        case _ => state
      }
  }

  def view(log: EventLog, checkpoint: Option[os.Path] = None): View[CourseState] =
    View(log, projection, checkpoint)
}

// Students' identities in a course: one StudentId per CSID, given out once.
final class Identities(view: View[CourseState])(using keys: KeyStore) {
  def find(csid: String): Option[StudentId] = view.state.by_csid.get(csid)

  // None if there's no such student, or they've been shredded
  def csid_of(student: StudentId): Option[String] =
    view.state.students.get(student).flatMap(_.get)

  // The student's identity, recorded now if they have none yet.
  def student_for(csid: String): StudentId = {
    var found: Option[StudentId] = None
    val _ = view.update(
      state =>
        state.by_csid.get(csid) match {
          case Some(id) =>
            found = Some(id)
            Nil
          case None =>
            val identity = Identities.new_identity(csid)
            found = Some(identity.student)
            Seq(identity)
        },
      lost = Identities.forget
    )
    found.get
  }
}

object Identities {
  // A new student, with a new key: to be appended, or else forgotten.
  def new_identity(csid: String)(using keys: KeyStore): StudentIdentity = {
    val id = StudentId.fresh()
    keys.create(id)
    StudentIdentity(Box.seal(csid, id))
  }

  // Shreds the keys of the new identities in `events`, which weren't appended.
  def forget(events: Seq[Event])(using keys: KeyStore): Unit =
    events.foreach {
      case e: StudentIdentity => keys.shred(e.student)
      case _                  => ()
    }
}

// What one round of watching a course appended.
final case class WatchSummary(keys: Int, repos: Int, unknown_repos: Int) {
  override def toString: String =
    s"$keys key(s), $repos repo change(s)" +
      (if (unknown_repos > 0) s", $unknown_repos repo(s) of no known student"
       else "")
}

// One course's window on the outside world: its dropbox and its repos on the
// git server, recorded in its private log.
final class CourseWatch(
    course: String,
    log: EventLog,
    keys: KeyStore,
    // where students drop their keys, if the course has one
    dropbox: Option[os.Path],
    // see Dropbox.observe
    check_owners: Boolean,
    // where to keep the course's view of the log between runs
    checkpoint: Option[os.Path] = None
) {
  private given KeyStore = keys
  private val view = CourseState.view(log, checkpoint)

  // One round. The dropbox goes first, so a new student's identity exists
  // before their repos are looked at. `repos` is the git server's state
  // (Gitolite.observe), shared by every course's round.
  def poll(repos: Map[String, Option[String]]): WatchSummary = {
    // The private repo must be declared in the server's conf, with its own
    // rules (only the grader writes it). Pushing to it otherwise would create
    // it as a wildcard repo, writable by staff and shareable by its creator.
    val private_repo = s"${course}__private"
    if (!repos.contains(private_repo))
      throw EventLogException(
        s"$private_repo isn't on the git server: declare it in gitolite.conf first"
      )
    val new_keys = dropbox.fold(Nil)(d =>
      DropboxWatcher(view).poll(Dropbox.observe(d, check_owners))
    )
    val (changes, unknown) = GitWatcher(course, view).poll(repos)
    WatchSummary(
      new_keys.count(_.event.isInstanceOf[KeySubmitted]),
      changes.size,
      unknown
    )
  }
}
