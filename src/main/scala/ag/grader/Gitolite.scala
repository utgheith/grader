package ag.grader

import ag.git.{Sha, git}
import ag.r2.{Scope, Target, WithData, periodic, update_data, say}

import language.experimental.namedTuples
import upickle.default.read

import scala.collection.{SortedMap, SortedSet}
import scala.util.control.NonFatal

object Gitolite extends Scope(".") {

  // private val sem = Semaphore(4)

  lazy val history: Target[SortedMap[String, String]] =
    target(periodic(60 * 1000), Config.gitolite) { (_, g) =>
      val (rc, stdout, stderr) =
        g.ssh("history").run(cwd = os.pwd, check = false)
      if (rc != 0) {
        println(
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

  lazy val info: Target[SortedSet[String]] =
    target(periodic(60 * 1000), Config.gitolite) { (_, g) =>
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

  lazy val latest: Target[SortedMap[String, Option[String]]] =
    target(history, info) { (history, info) =>
      val pairs = for {
        repo <- info.toSeq
      } yield (repo, history.get(repo))
      SortedMap(pairs*)
    }

  lazy val repo_info: String => Target[RepoInfo] = fun { (repo: String) =>
    target(Config.gitolite, latest) { (server, latest) =>
      RepoInfo(server, repo, latest.get(repo))
    }
  }

  // def repo_exists(repo: String): Maker[Boolean] = Rule(latest_for(repo), repo)(_.isDefined)

  lazy val mirror: String => Target[WithData[Boolean]] = fun { (repo: String) =>
    target[RepoInfo, WithData[Boolean]](repo_info(repo)) { repo_info =>
      update_data[Boolean](skip = _.lastOpt.contains(".git")) { dir =>
        repo_info match {
          case RepoInfo(server, _, Some(_)) =>
            try {
              if (!os.isDir(dir / ".git")) {
                throw Exception("force clone")
              }
              say(s"pulling $repo")
              server.SshProc("git", "pull").check(cwd = dir)
            } catch {
              case NonFatal(_) =>
                os.remove.all(dir)
                os.makeDir.all(dir)
                say(s"cloning $repo because pull failed")
                server
                  .SshProc(
                    "git",
                    "clone",
                    "--template=",
                    "-c",
                    "remote.origin.fetch=+refs/notes/*:refs/notes/*",
                    server.git_uri(repo),
                    "."
                  )
                  .check(cwd = dir)
            }
            true
          case _ =>
            false
        }
      }
    }
  }

  lazy val notes_for_repo
      : (String, String) => Target[SortedMap[Sha.Commit, Sha.Note]] = fun {
    (repo: String, notes_ref: String) =>
      val mirror_target = mirror(repo)
      target(mirror_target) {
        case m @ WithData(true, _, _) =>
          git(C = m.get_data_path)
            .notes(ref = notes_ref)
            .check()
            .map(p => (p.commit_sha, p.note_sha))
            .to(SortedMap)
        case _ =>
          SortedMap[Sha.Commit, Sha.Note]()
      }
  }

  private lazy val raw_courses: Target[SortedMap[String, RawCourse]] = {
    val mirror_target = mirror("courses_config")
    target(mirror_target) { mirror =>
      if (mirror.value) {
        val file = mirror.get_data_path / "courses.json"
        val not_sorted = read[Map[String, RawCourseNotSorted]](os.read(file))
        not_sorted.view.mapValues(_.sorted).to(SortedMap)
      } else {
        say("courses_config repo is missing")
        SortedMap[String, RawCourse]()
      }
    }
  }

  lazy val course_names: Target[SortedSet[String]] = target(raw_courses) {
    courses =>
      courses.keySet
  }

  lazy val raw_course: String => Target[RawCourse] = fun { (name: String) =>
    target(raw_courses) { courses =>
      courses(name)
    }
  }

  lazy val raw_project: (String, String) => Target[RawProject] = fun {
    (course_name: String, project_name: String) =>
      target(raw_course(course_name)) { course =>
        course.projects(project_name)
      }
  }
}
