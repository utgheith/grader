package ag.grader

import ag.git.{Sha, git}
import ag.r2.{
  Many,
  Producer,
  Scope,
  Target,
  Tracker,
  WithData,
  periodic,
  run_if_needed,
  update_data,
  say,
  shout
}

import upickle.default.{ReadWriter, read}

import scala.NamedTuple.NamedTuple
import scala.collection.{SortedMap, SortedSet, mutable}
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

// A local checkout of a gitolite repo, up to date with the server. Whatever a
// gitolite_target's function leaves in `dir` is committed and pushed after it
// returns.
case class Checkout(
    dir: os.Path,
    info: RepoInfo,
    // whether the repo was created for this run
    forked: Boolean
) {
  private val after_push_actions = mutable.ArrayBuffer[() => Unit]()

  // Runs `action` once the changes are on the server, or there was nothing to
  // push. It doesn't run if the update fails, so it's safe to announce the
  // changes from here: a retry won't repeat the announcement.
  def after_push(action: => Unit): Unit = {
    val _ = after_push_actions.append(() => action)
  }

  private[grader] def run_after_push(): Unit =
    after_push_actions.foreach(action => action())
}

// A scope with targets that keep a gitolite repo up to date.
class GitoliteScope(base_ : os.RelPath | String | Scope) extends Scope(base_) {

  // val x = gitolite_target(repo = "r", msg = "update r")((a = ta)) {
  //   (producer, deps, repo) => ... deps.a ... repo.dir ...
  // }
  //
  // Same as target { ... }, with these additions: the target also depends on
  // the repo, so it runs again when someone else pushes to it, and it needs
  // Config.can_push_repo to push or to create the repo. The repo is created on
  // first use by forking `fork_from` and giving `readers` and `writers` access;
  // they aren't applied to a repo that already exists. With `fork_from = None`,
  // a missing repo is an error.
  def gitolite_target[N <: Tuple, V <: Tuple, Out: ReadWriter](
      repo: String,
      msg: String,
      fork_from: Option[String] = Some("empty"),
      readers: Seq[String] = Seq(),
      writers: Seq[String] = Seq()
  )(targets: NamedTuple[N, V])(
      f: (Producer[?], Target.Values[N, V], Checkout) => Out
  )(using
      only_targets: Tuple.Union[V] <:< (Target[?] | Many[?]),
      fn: sourcecode.FullName
  ): Target[WithData[Out]] =
    complex_gitolite_target(repo, msg, fork_from, readers, writers) { tracker =>
      val values = tracker.track_all(targets)
      (producer, checkout) => f(producer, values, checkout)
    }

  // The same, for dependencies that depend on the values of others: track them
  // and return what to do with the checkout, which runs only if needed.
  //
  // val x = complex_gitolite_target(repo = "r", msg = "update r") { tracker =>
  //   val as = tracker.track(ta)
  //   (producer, repo) => ... as ... repo.dir ...
  // }
  def complex_gitolite_target[Out: ReadWriter](
      repo: String,
      msg: String,
      fork_from: Option[String] = Some("empty"),
      readers: Seq[String] = Seq(),
      writers: Seq[String] = Seq()
  )(f: Tracker[?] => (Producer[?], Checkout) => Out)(using
      fn: sourcecode.FullName
  ): Target[WithData[Out]] = complex_target { tracker =>
    val deps = tracker.track_all(
      (info = Gitolite.repo_info(repo), can_push_repo = Config.can_push_repo)
    )
    val update = f(tracker)
    run_if_needed(tracker) { producer =>
      update_data(producer)(_.lastOpt.contains(".git")) { dir =>
        val (out, checkout) = deps.info.update(
          path = dir,
          fork_from = fork_from,
          msg = msg,
          readers = readers,
          writers = writers,
          can_push_repo = deps.can_push_repo
        ) { forked =>
          val checkout = Checkout(dir, deps.info, forked)
          (update(producer, checkout), checkout)
        }
        checkout.run_after_push()
        out
      }
    }
  }
}
