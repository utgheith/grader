package ag.grader

import ag.git.{Sha, git}
import ag.r2.{Scope, Target, WithData, periodic, update_data, say, shout}

import upickle.default.read

import scala.collection.{SortedMap, SortedSet}
import scala.util.control.NonFatal

object Gitolite extends Scope(".") {

  // private val sem = Semaphore(4)

  lazy val history: Target[SortedMap[String, String]] =
    target((periodic = periodic(60 * 1000), g = Config.gitolite)) { (_, deps) =>
      val (rc, stdout, stderr) =
        deps.g.ssh("history").run(cwd = os.pwd, check = false)
      if (rc != 0) {
        println(
          s"getting history failed, reverting to expensive check [details ${stderr
              .map(_.relativeTo(os.pwd))
              .toString}]"
        )
        SortedMap()
      } else {
        (for {
          l <- os.read.lines(stdout)
          parts = l.split(' ')
          if parts.length == 2
          Array(name, sha) = parts
        } yield (name, sha)).to(SortedMap)
      }
    }

  lazy val info: Target[SortedSet[String]] =
    target((periodic = periodic(60 * 1000), g = Config.gitolite)) { (_, deps) =>
      val lines = {
        deps.g.ssh("info").lines(cwd = os.pwd)
      }
      val repos = for {
        line <- lines
        if line.nonEmpty
        if !line.startsWith("hello")
        repo = line.split("""\s""").toSeq.last
      } yield repo

      SortedSet(repos*)
    }

  lazy val latest: Target[SortedMap[String, Option[String]]] =
    target((history = history, info = info)) { (_, deps) =>
      val pairs = for {
        repo <- deps.info.toSeq
      } yield (repo, deps.history.get(repo))
      SortedMap(pairs*)
    }

  lazy val repo_info: String => Target[RepoInfo] = fun { (repo: String) =>
    target((server = Config.gitolite, latest = latest)) { (_, deps) =>
      RepoInfo(deps.server, repo, deps.latest.get(repo))
    }
  }

  // def repo_exists(repo: String): Maker[Boolean] = Rule(latest_for(repo), repo)(_.isDefined)

  lazy val mirror: String => Target[WithData[Boolean]] = fun { (repo: String) =>
    target((repo_info = repo_info(repo))) { (producer, deps) =>
      update_data[Boolean](producer)(skip = _.lastOpt.contains(".git")) { dir =>
        deps.repo_info match {
          case RepoInfo(server, _, Some(_)) =>
            try {
              if (!os.isDir(dir / ".git")) {
                throw Exception("force clone")
              }
              shout(producer, s"pulling $repo")
              server.SshProc("git", "pull").check(cwd = dir)
            } catch {
              case NonFatal(_) =>
                os.remove.all(dir)
                os.makeDir.all(dir)
                shout(producer, s"cloning $repo because pull failed")
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
      target((mirror_target = mirror_target)) {
        case (producer, (mirror_target = m @ WithData(true, _, _))) =>
          git(C = m.get_data_path(producer))
            .notes(ref = notes_ref)
            .check()
            .map(p => (p.commit_sha, p.note_sha))
            .to(SortedMap)
        case (_, (mirror_target = _)) =>
          SortedMap[Sha.Commit, Sha.Note]()
      }
  }

  private lazy val raw_courses: Target[SortedMap[String, RawCourse]] =
    target((mirror = mirror("courses_config"))) { (producer, deps) =>
      if (deps.mirror.value) {
        val file = deps.mirror.get_data_path(producer) / "courses.json"
        val not_sorted = read[Map[String, RawCourseNotSorted]](os.read(file))
        not_sorted.view.mapValues(_.sorted).to(SortedMap)
      } else {
        say(producer, "courses_config repo is missing")
        SortedMap[String, RawCourse]()
      }
    }

  lazy val course_names: Target[SortedSet[String]] =
    target((courses = raw_courses)) { (_, deps) =>
      deps.courses.keySet
    }

  lazy val raw_course: String => Target[RawCourse] = fun { (name: String) =>
    target((courses = raw_courses)) { (_, deps) =>
      deps.courses(name)
    }
  }

  lazy val raw_project: (String, String) => Target[RawProject] = fun {
    (course_name: String, project_name: String) =>
      target((course = raw_course(course_name))) { (_, deps) =>
        deps.course.projects(project_name)
      }
  }
}
