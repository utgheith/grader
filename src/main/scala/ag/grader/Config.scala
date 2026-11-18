package ag.grader

import ag.grader.cloudflare.CloudflareConfig

import ag.common.{down, given_ReadWriter_RelPath}
import ag.rules.run
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
      RemoteServer.ssh_locks
        .getOrElseUpdate(host, Semaphore(RemoteServer.ssh_per_host))
        .down(1) {
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
  // how many ssh processes run at once to one host (the rest wait their turn)
  val ssh_per_host: Int = 4
  val ssh_locks: TrieMap[String, Semaphore] = TrieMap[String, Semaphore]()

}

@upickle.implicits.allowUnknownKeys(false)
case class Config(
    gitolite: RemoteServer,
    dropbox_path: Option[os.RelPath],
    site_base: Option[String],
    can_send_mail: Option[Boolean],
    can_push_repo: Option[Boolean],
    cloudflare: Option[CloudflareConfig],
    // serve each run's graph live; see ag.web.WebConfig
    web: Option[ag.web.WebConfig] = None,
    // a file holding the course's time (see Clock), relative to this file's
    // directory; a simulation's (gitolite-mirror/sim), never a live config's
    clock: Option[String] = None
) derives ReadWriter {}

object Config {

  private def config_file: os.Path = {
    @tailrec
    def find(d: os.Path): os.Path = {
      val t = d / "config.json"
      if os.isFile(t) then t else find(d / os.up)
    }
    find(os.pwd)
  }

  private def load(): Config = read[Config](os.read(config_file))

  // Read straight from config.json, never through a target, like cloudflare.
  def web: Option[ag.web.WebConfig] = load().web

  // The git server, read straight from config.json: for what runs outside
  // any State (LocalMirror).
  def gitolite_server: RemoteServer = load().gitolite

  // whether pushing to the git server is allowed, read straight from
  // config.json: for what runs outside any State (Outbox.sync)
  def pushes_allowed: Boolean = load().can_push_repo.getOrElse(false)

  // whether sending mail is allowed, the same way
  def mail_allowed: Boolean = load().can_send_mail.getOrElse(false)

  // where the students drop their keys (dropbox_path, under the home
  // directory), read straight from config.json; None if it has none
  def dropbox_root: Option[os.Path] = load().dropbox_path.map(os.home / _)

  // the file holding the course's time (see Clock), read straight from
  // config.json; None if it has none: the course runs on the wall clock
  def clock_file: Option[os.Path] =
    load().clock.map(f => os.Path(f, config_file / os.up))

  // Read straight from config.json on every call, never through a target,
  // so the api_key is never cached under workspace/.
  def cloudflare: Option[CloudflareConfig] = load().cloudflare
}
