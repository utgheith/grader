package ag.grader

import ag.common.{down, given_ReadWriter_RelPath, given_VirtualExecutionContext, run}
import ag.r2.{Target, run_if_needed, target}
import os.RelPath
import upickle.default.{ReadWriter, read}

import java.util.concurrent.Semaphore
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

@upickle.implicits.allowUnknownKeys(false)
case class RemoteServer(user: String, host: String, port: Int)
    derives ReadWriter {
  lazy val ssh_uri: String = s"ssh://$user@$host:$port"
  def git_uri(repo: String): String = s"$ssh_uri/$repo"

  class SshProc(parts: os.Shellable*) {
    def run(
        cwd: os.Path,
        check: Boolean = true,
        stdin: os.ProcessInput = ""
    ): (Int, os.Path, Option[os.Path]) = {
      RemoteServer.ssh_locks.getOrElseUpdate(host, Semaphore(4)).down(1) {
        os.proc(parts*).run(cwd = cwd, check = check, stdin = stdin)
      }
    }

    def check(
        cwd: os.Path,
        stdin: os.ProcessInput = ""
    ): Unit = {
      val _ = run(cwd, true, stdin)
    }

    def lines(cwd: os.Path, stdin: os.ProcessInput = ""): Seq[String] =
      os.read.lines(run(cwd, true, stdin)._2)
  }

  def ssh(parts: os.Shellable*): SshProc = SshProc(
    os.Shellable(Seq("ssh", ssh_uri)) +: parts*
  )

  // def git_clone(repo: String, dir: os.Path): SshProc =
  //  SshProc("git", "clone", git_uri(repo), dir)

  // def git_fork(src: String, dest: String): SshProc = SshProc("fork", src, dest)

}

object RemoteServer {
  val ssh_locks: TrieMap[String, Semaphore] = TrieMap[String, Semaphore]()

}

@upickle.implicits.allowUnknownKeys(false)
case class Config(
    gitolite: RemoteServer,
    dropbox_path: Option[os.RelPath] = None,
    site_base: Option[String] = None,
    can_send_mail: Option[Boolean] = None,
    can_push_repo: Option[Boolean] = None
) derives ReadWriter {}

object Config {

  private lazy val config: Target[Config] = target() {

      @tailrec
      def find(d: os.Path): os.Path = {
        val t = d / "config.json"
        if os.isFile(t) then t else find(d / os.up)
      }

      val cf = find(os.pwd)
      // logger.info(s"loading config from $cf")
      read[Config](os.read(cf))
    
  }

  lazy val gitolite: Target[RemoteServer] = target(config)(_.gitolite)
  
  lazy val dropbox_path: Target[Option[RelPath]] = target(config)(_.dropbox_path)
  
  lazy val site_base: Target[Option[String]] = target(config)(_.site_base)
  
  // TODO: peek
  lazy val can_send_mail: Target[Boolean] =  target(config) { config =>
    throw Exception("implement peek")
    config.can_send_mail.getOrElse(false)
  }
  
  lazy val can_push_repo: Target[Boolean] = target(config)(_.can_push_repo.getOrElse(false))
}
