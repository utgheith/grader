package ag.grader

import ag.rules.{Maker, Rule, down, run, given_ReadWriter_RelPath}
import upickle.default.{ReadWriter, read}

import java.util.concurrent.Semaphore
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

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
        os.proc(parts*).run(cwd = cwd, check = check)
      }
    }
  }

  def ssh(parts: os.Shellable*): SshProc = SshProc(
    (os.Shellable(Seq("ssh", ssh_uri)) +: parts)*
  )

  def git_clone(repo: String, dir: os.Path): SshProc =
    SshProc("git", "clone", git_uri(repo), dir)

  def git_fork(src: String, dest: String): SshProc = SshProc("fork", src, dest)

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

  private val config: Maker[Config] = Rule() {
    @tailrec
    def find(d: os.Path): os.Path = {
      val t = d / "config.json"
      if os.isFile(t) then t else find(d / os.up)
    }
    val cf = find(os.pwd)
    // logger.info(s"loading config from $cf")
    read[Config](os.read(cf))
  }

  val gitolite: Maker[RemoteServer] = Rule(config, null)(_.gitolite)
  val dropbox_path: Maker[Option[os.RelPath]] =
    Rule(config, null)(_.dropbox_path)
  val site_base: Maker[Option[String]] = Rule(config, null)(_.site_base)
  val can_send_mail: Maker[Boolean] =
    Rule(config, null)(_.can_send_mail.getOrElse(false))
  val can_push_repo: Maker[Boolean] =
    Rule(config, null)(_.can_push_repo.getOrElse(false))
}
