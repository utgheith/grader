package ag.grader

import ag.rules.{Maker, Periodic, Rule, SignedPath, say}
import upickle.default.read

import scala.collection.{SortedMap, SortedSet}
import scala.util.control.NonFatal

object Gitolite {

  // private val sem = Semaphore(4)

  val history: Maker[SortedMap[String, String]] =
    Rule(Periodic(ms = 60 * 1000) *: Config.gitolite, null) { (_, g) =>
      val (rc, stdout, stderr) =
        g.ssh("history").run(cwd = os.pwd, check = false)
      if (rc != 0) {
        say(
          s"getting history failed, reverting to expensive check [details ${stderr
              .map(_.relativeTo(os.pwd))}]"
        )
        SortedMap()
      } else {
        (for {
          l <- os.read.lines(stdout)
          parts = l.split(' ').nn
          if parts.length == 2
          Array(name, sha) = parts
        } yield (name, sha)).to(SortedMap)
      }
    }

  val info: Maker[SortedSet[String]] =
    Rule((Periodic(ms = 60 * 1000), Config.gitolite), null) { (_, g) =>
      val lines = {
        g.ssh("info").lines(cwd = os.pwd)
      }
      val repos = for {
        line <- lines
        if line.nonEmpty
        if !line.startsWith("hello")
        repo = line.split("""\s""").nn.toSeq.last.nn
      } yield repo

      SortedSet(repos*)
    }

  val latest: Maker[SortedMap[String, Option[String]]] =
    Rule(history *: info, null) { case (history, info) =>
      val pairs = for {
        repo <- info.toSeq
      } yield (repo, history.get(repo))
      SortedMap(pairs*)
    }

  def repo_info(repo: String): Maker[RepoInfo] =
    Rule(Config.gitolite *: latest, os.RelPath(repo)) { (server, latest) =>
      RepoInfo(server, repo, latest.get(repo))
    }

  // def repo_exists(repo: String): Maker[Boolean] = Rule(latest_for(repo), repo)(_.isDefined)

  def mirror(repo: String): Maker[SignedPath[Boolean]] =
    SignedPath.rule(repo_info(repo), SortedSet(".git"), os.RelPath(repo)) {
      case (dir, RepoInfo(server, _, Some(_))) =>
        try {
          say(s"pulling $repo")
          server.SshProc("git", "pull").check(cwd = dir)
        } catch {
          case NonFatal(_) =>
            os.remove.all(dir)
            os.makeDir.all(dir)
            say(s"cloning $repo (pull failed)")
            server
              .SshProc("git", "clone", "--template=", "-c", "remote.origin.fetch=+refs/notes/*:refs/notes/*", server.git_uri(repo), ".")
              .check(cwd = dir)
        }
        true
      case (_, _) =>
        false
    }

  private val raw_courses: Maker[SortedMap[String, RawCourse]] =
    Rule(mirror("courses_config"), null) { path =>
      if (path.data) {
        val file = path.path / "courses.json"
        val not_sorted = read[Map[String, RawCourseNotSorted]](os.read(file))
        not_sorted.view.mapValues(_.sorted).to(SortedMap)
      } else {
        say("courses_config repo is missing")
        SortedMap()
      }

      // read[SortedMap[String, RawCourse]](os.read(file))
    }

  val course_names: Maker[SortedSet[String]] =
    Rule(raw_courses, null) { courses =>
      courses.keySet
    }

  def raw_course(name: String): Maker[RawCourse] =
    Rule(raw_courses, os.RelPath(name)) { courses =>
      courses(name)
    }

  def raw_project(
      course_name: String,
      project_name: String
  ): Maker[RawProject] = Rule(
    raw_course(course_name),
    os.RelPath(course_name) / project_name
  ) { course =>
    course.projects(project_name)
  }

}
