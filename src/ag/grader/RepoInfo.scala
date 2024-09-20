package ag.grader

import upickle.default.ReadWriter
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import ag.rules.{run, say}

case class RepoInfo(server: RemoteServer, repo_name: String, latest: Option[Option[String]]) derives ReadWriter {

  def update[A](path: os.Path, fork_from:  Option[String], msg: String, readers: Seq[String], writers: Seq[String])(f: Boolean => A): A = {
    var forked: Boolean = false

    try {
      /* The most common path: the repo exists and we have a local checkout, just pull the latest */
      os.proc("git", "reset", "--hard").run(cwd = path)
      os.proc("git", "clean", "-fdx").run(cwd = path)
      os.proc("git", "checkout", "master").run(cwd = path)
      server.SshProc("git", "pull").run(cwd = path)
    } catch {
      case NonFatal(_) =>
        /* Second common path: the repo exists but we don't have a local checkout, clone it */
        os.remove.all(path)
        os.makeDir.all(path)
        try {
          say(s"cloning ${repo_name}")
          server.SshProc("git", "clone", server.git_uri(repo_name), ".").run(cwd = path)
        } catch {
          case NonFatal(_) =>
            /* Least common path: the repo doesn't exists, fork it then clone it */
            fork_from match {
              case Some(fork_from) =>
                say(s"forking ${repo_name}")
                server.SshProc("ssh", server.ssh_uri, "fork", fork_from, repo_name).run(cwd = os.pwd)
                forked = true
              case None =>
                throw Exception(s"don't know how to create $repo_name")
            }
            /* set permissions */
            for (r <- readers) {
              server.SshProc("ssh", server.ssh_uri, "perms", repo_name, "+", "READERS", r).run(os.pwd)
            }
            for (w <- writers) {
              server.SshProc("ssh", server.ssh_uri, "perms", repo_name, "+", "WRITERS", w).run(os.pwd)
            }
            say(s"cloning $repo_name")
            server.SshProc("git", "clone", server.git_uri(repo_name), ".").run(cwd = path)
        }
    }

    // TODO: failures here will leave the directory in a corrupted state
    //       this is by design but maybe it can be reconsidered
    // why is it ok? because we do a reset/clean before we use it again
    val out = f(forked)
    val _ = os.proc("git", "add", ".").run(cwd = path)
    Try(os.proc("git", "commit", "-a", "-m", msg).run(cwd = path)) match {
      case Success(_) =>
        say(s"pushing $repo_name")
        val _ = server.SshProc("git", "push").run(cwd = path)
      case Failure(_) =>
    }
    out
  }

}