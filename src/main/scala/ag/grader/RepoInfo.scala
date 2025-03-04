package ag.grader

import ag.r2.say
import upickle.default.ReadWriter
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import ag.rules.{check, run}

case class RepoInfo(
    server: RemoteServer,
    repo_name: String,
    latest: Option[Option[String]]
) derives ReadWriter {

  def update[A](
      path: os.Path,
      fork_from: Option[String],
      msg: String,
      readers: Seq[String],
      writers: Seq[String],
      can_push_repo: Boolean
  )(f: Boolean => A): A = {
    var forked: Boolean = false

    def clone_it(): Unit = {
      say(s"cloning $repo_name")
      server
        .SshProc(
          "git",
          "clone",
          "--template=",
          server.git_uri(repo_name),
          "."
        )
        .check(cwd = path)
      server
        .SshProc("git", "fetch", "origin", "refs/notes/*:refs/notes/*")
        .check(cwd = path)
    }

    try {
      if (!os.isDir(path / ".git")) {
        throw Exception("forcing")
      }
      /* The most common path: the repo exists, and we have a local checkout, just pull the latest */
      os.proc("git", "reset", "--hard").check(cwd = path)
      os.proc("git", "clean", "-fdx").check(cwd = path)
      os.proc("git", "checkout", "master").check(cwd = path)
      server.SshProc("git", "pull").check(cwd = path)
      server
        .SshProc("git", "fetch", "origin", "refs/notes/*:refs/notes/*")
        .check(cwd = path)
    } catch {
      case NonFatal(_) =>
        /* Second common path: the repo exists, but we don't have a local checkout, clone it */
        os.remove.all(path)
        os.makeDir.all(path)
        try {
          clone_it()
        } catch {
          case NonFatal(_) =>
            /* Least common path: the repo doesn't exist, fork it then clone it */
            fork_from match {
              case Some(fork_from) =>
                if (!can_push_repo)
                  throw Exception(s"not allowed to fork $repo_name")
                say(s"forking $repo_name")
                server
                  .SshProc("ssh", server.ssh_uri, "fork", fork_from, repo_name)
                  .check(cwd = os.pwd)
                forked = true
              case None =>
                throw Exception(s"don't know how to create $repo_name")
            }
            /* set permissions */
            for (r <- readers) {
              server
                .SshProc(
                  "ssh",
                  server.ssh_uri,
                  "perms",
                  repo_name,
                  "+",
                  "READERS",
                  r
                )
                .run(os.pwd)
            }
            for (w <- writers) {
              server
                .SshProc(
                  "ssh",
                  server.ssh_uri,
                  "perms",
                  repo_name,
                  "+",
                  "WRITERS",
                  w
                )
                .run(os.pwd)
            }
            clone_it()
        }
    }

    // TODO: failures here will leave the directory in a corrupted state
    //       this is by design but maybe it can be reconsidered
    // why is it ok? because we do a reset/clean before we use it again
    val out = f(forked)
    val _ = os.proc("git", "add", ".").run(cwd = path)
    Try(os.proc("git", "commit", "-a", "-m", msg).run(cwd = path)) match {
      case Success(_) =>
        if (!can_push_repo) throw Exception(s"not allowed to push $repo_name")
        say(s"pushing $repo_name")
        val _ = server.SshProc("git", "push").run(cwd = path)
      case Failure(_) =>
    }
    out
  }

}
