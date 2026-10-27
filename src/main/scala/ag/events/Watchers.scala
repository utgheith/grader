package ag.events

import upickle.default.ReadWriter

// ---- The system's two windows on the outside world
//
// Both are polled. A watcher compares what it observes with what the log
// already says and appends only the differences, conditionally (append_if):
// if someone else appended meanwhile it looks again, so several watchers may
// run at once, and any of them may be restarted at any time.
//
// What a watcher observes is passed in, so watchers can be tested without the
// dropbox or the git server; Dropbox.observe and Gitolite.observe do the real
// observing.

// ---- The dropbox: students copy their public key to <dropbox>/<csid>.pub

final class DropboxWatcher(view: View[CourseState])(using keys: KeyStore) {

  // `observed`: CSID -> public key, as found in the dropbox. A new student
  // gets an identity, and every new or changed key is recorded, all in one
  // append.
  def poll(observed: Map[String, String]): Seq[Logged] =
    view.update(
      state => {
        val students = state.by_csid
        // in no particular order: the log mustn't hint at CSIDs' order
        scala.util.Random.shuffle(observed.toSeq).flatMap { (csid, ssh_key) =>
          students.get(csid) match {
            case Some(id) =>
              if (state.ssh_keys.get(id).flatMap(_.get).contains(ssh_key)) Nil
              else Seq(KeySubmitted(Box.seal(ssh_key, id)))
            case None =>
              val identity = Identities.new_identity(csid)
              Seq(identity, KeySubmitted(Box.seal(ssh_key, identity.student)))
          }
        }
      },
      lost = Identities.forget
    )
}

object Dropbox {
  // The keys in `dir`. A file counts only if the operating system says its
  // owner is the CSID it's named after; others are skipped, with a warning.
  // `check_owners` = false trusts every file: only for a copy of a dropbox
  // whose owners were checked where it came from (copying loses them). A
  // missing dropbox is an error: it would look like a course with no one in
  // it.
  def observe(dir: os.Path, check_owners: Boolean): Map[String, String] = {
    if (!os.isDir(dir))
      throw EventLogException(s"no dropbox at ${dir.toString}")
    os.list(dir)
      .filter(f => os.isFile(f) && f.ext == "pub")
      .flatMap { f =>
        val csid = f.baseName
        val owner = os.owner(f).getName
        if (!check_owners || owner == csid) Some(csid -> os.read(f).trim.nn)
        else {
          println(s"dropbox: skipping ${f.last}: owned by $owner, not $csid")
          None
        }
      }
      .toMap
  }
}

// ---- The git server: `history` gives each repo's latest sha

// A course or project repo changed (names carry nothing about students).
@upickle.implicits.allowUnknownKeys(false)
final case class RepoChanged(repo: String, sha: Option[String]) extends Event
    derives ReadWriter {
  def kind: EventKind[RepoChanged] = RepoChanged.kind
}

object RepoChanged {
  val kind: EventKind[RepoChanged] = EventKind[RepoChanged]("repo-changed")
}

enum StudentRepo derives ReadWriter:
  case Work, Results

// One of a student's repos changed. The repo's name holds their CSID, so
// it's recorded by StudentId instead.
@upickle.implicits.allowUnknownKeys(false)
final case class StudentRepoChanged(
    project: String,
    student: StudentId,
    which: StudentRepo,
    sha: Option[String]
) extends Event derives ReadWriter {
  def kind: EventKind[StudentRepoChanged] = StudentRepoChanged.kind
}

object StudentRepoChanged {
  val kind: EventKind[StudentRepoChanged] =
    EventKind[StudentRepoChanged]("student-repo-changed")
}

// What a repo of `course` is, from its name.
enum RepoName:
  // <course>__x, <course>_<project>, <course>_<project>__x
  case Shared(repo: String)
  // <course>_<project>_<csid>[_results]
  case OfStudent(project: String, csid: String, which: StudentRepo)

object RepoName {
  def parse(course: String, repo: String): Option[RepoName] = {
    val prefix = s"${course}_"
    if (!repo.startsWith(prefix)) None // another course's, or no course's
    else {
      val rest = repo.drop(prefix.length)
      if (rest.startsWith("_")) Some(Shared(repo)) // <course>__x
      else
        rest.split("_", 2) match {
          case Array(_) => Some(Shared(repo)) // starter
          case Array(_, more) if more.startsWith("_") => Some(Shared(repo))
          case Array(project, more)                   =>
            if (more.endsWith("_results"))
              Some(
                OfStudent(
                  project,
                  more.dropRight("_results".length),
                  StudentRepo.Results
                )
              )
            else Some(OfStudent(project, more, StudentRepo.Work))
        }
    }
  }
}

// What a repo change is about: a shared repo, or one of a student's.
enum RepoKey derives ReadWriter:
  case Shared(name: String)
  case Student(project: String, id: StudentId, which: StudentRepo)

final class GitWatcher(course: String, view: View[CourseState])(using
    KeyStore
) {
  // `observed`: repo -> its latest sha (None: no commits yet), for every repo
  // on the server. Returns what was appended, and how many repos looked like
  // a student's but belong to no known student (skipped, never logged:
  // their names hold CSIDs).
  def poll(observed: Map[String, Option[String]]): (Seq[Logged], Int) = {
    var unknown = 0
    val appended = view.update { state =>
      val students = state.by_csid
      unknown = 0
      val current: Seq[(RepoKey, Option[String])] = observed.toSeq.flatMap {
        (repo, sha) =>
          RepoName.parse(course, repo) match {
            case None                        => None
            case Some(RepoName.Shared(name)) => Some(RepoKey.Shared(name) -> sha)
            case Some(RepoName.OfStudent(project, csid, which)) =>
              students.get(csid) match {
                case Some(id) => Some(RepoKey.Student(project, id, which) -> sha)
                case None     =>
                  unknown += 1
                  None
              }
          }
      }
      current
        .filter((key, sha) => !state.repos.get(key).contains(sha))
        .sortBy(_._1.toString)
        .map {
          case (RepoKey.Shared(name), sha) => RepoChanged(name, sha)
          case (RepoKey.Student(project, id, which), sha) =>
            StudentRepoChanged(project, id, which, sha)
        }
    }
    (appended, unknown)
  }
}

object Gitolite {
  // Every repo on the server and its latest sha, from `info` and `history`
  // (run as a user who can read the `grader` repo: history answers no one
  // else). Fails rather than guess: an empty or partial answer would look
  // like every repo had changed. `ssh` is the command to reach it with.
  def observe(
      server: String,
      port: Int = 22,
      ssh: Seq[String] = Seq("ssh")
  ): Map[String, Option[String]] = {
    def run(command: String): Seq[String] =
      os.proc(ssh, "-p", port.toString, server, command)
        .call(stderr = os.Pipe)
        .out
        .lines()
    // "info" lists repos and, for wildcard rules, patterns (" R W C\tcs439c_.*")
    val repos = run("info")
      .filter(l => l.nonEmpty && !l.startsWith("hello"))
      .map(_.split("""\s""").last)
      .filterNot(name => name.exists("*?[]\\^$+(){}|".contains(_)))
    val history = run("history")
    if (history.forall(_.isEmpty) && repos.nonEmpty)
      throw EventLogException(
        s"$server's history is empty: does this user have read access to the grader repo?"
      )
    val latest = history.flatMap { l =>
      l.split(' ') match {
        case Array(name, sha) => Some(name -> sha.trim)
        case _                => None
      }
    }.toMap
    repos.map(r => r -> latest.get(r)).toMap
  }
}
