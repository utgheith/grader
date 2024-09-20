package ag.grader

import ag.rules.{Maker, Rule, down, run, given_ReadWriter_RelPath}
import upickle.default.{ReadWriter, read}

import java.util.concurrent.Semaphore
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

/*
class RepoUpdater(server: RemoteServer, repo_name: String) {
  private val lock = RepoUpdater.locks.getOrElseUpdate(this, ReentrantLock())
  
  private val path: os.Path = {
    val d = os.temp.dir(dir=os.pwd, prefix=s"temp_${server.user}_${server.host}_${server.port}_${repo_name}_", deleteOnExit=true)
    val _ = server.SshProc("git", "clone", server.git_uri(repo_name), ".").run(cwd = d)
    d
  }

  def update[A](msg: String)(f: os.Path => A): A = {
    lock.lock()
    try {
      val _ = os.proc("git", "reset", "--hard").run(cwd = path)
      val _ = os.proc("git", "clean", "-fdx").run(cwd = path)
      val _ = os.proc("git", "checkout", "master").run(cwd = path)
      val _ = server.SshProc("git", "pull").run(cwd = path)
      
      // TODO: failures here will leave the directory in a corrupted state
      //       this is by design but maybe it can be reconsidered
      // why is it ok? because we do a reset/clean before we use it again
      val out = f(path)
      val _ = os.proc("git", "add", ".").run(cwd = path)
      Try(os.proc("git", "commit", "-a", "-m", msg).run(cwd = path)) match {
        case Success(_) =>
          val _ = server.SshProc("git", "push").run(cwd = path)
        case Failure(_) =>
      }
      out
    } finally {
      lock.unlock()
    }
  }


}

object RepoUpdater {
  private val locks = TrieMap[RepoUpdater, ReentrantLock]()
  //private val updaters = TrieMap[(RemoteServer, String), RepoUpdater]()

  //def apply(server: RemoteServer, repo_name: String): RepoUpdater =
  //  updaters.getOrElseUpdate((server, repo_name), new RepoUpdater(server, repo_name))

}
*/

@upickle.implicits.allowUnknownKeys(false)
case class RemoteServer(user: String, host: String, port: Int) derives ReadWriter {
  lazy val ssh_uri: String = s"ssh://$user@$host:$port"
  def git_uri(repo: String): String = s"$ssh_uri/$repo"

  /*
  def fork_from(src: String, dest: String): Unit = {
    val _ = os.proc("ssh", ssh_uri, "fork", src, dest).run()
  }*/

  /*
  def is_git_repo(path: os.Path): Boolean = os.isDir(path / ".git")
  */
  /*
  def check_working_repo(dir: os.Path): Unit = {
    if (dir == os.Path) {
      throw Exception("pwd is not a valid working repo")
    }
    if (!is_git_repo(dir)) {
      throw Exception(s"${dir.relativeTo(os.pwd)} is not a git repo")
    }
  }*/

  /*

  def clone_repo(src: String, target: os.Path): Unit = {
    os.makeDir.all(target)
    SshProc("git", "clone", git_uri(src), ".").run(cwd=target)
  } */

  class SshProc(parts: os.Shellable*) {
    def run(cwd: os.Path, check: Boolean = true, stdin: os.ProcessInput = ""): (Int, os.Path, Option[os.Path]) = {
      RemoteServer.ssh_locks.getOrElseUpdate(host, Semaphore(4)).down(1) {
        os.proc(parts*).run(cwd=cwd, check=check)
      }
    }
  }
  
  def ssh(parts: os.Shellable*): SshProc = SshProc((os.Shellable(Seq("ssh", ssh_uri)) +: parts)*)
  
  def git_clone(repo: String, dir: os.Path): SshProc = SshProc("git", "clone", git_uri(repo), dir)
  
  def git_fork(src: String, dest: String): SshProc = SshProc("fork", src, dest)
  
  
}

object RemoteServer {
  val ssh_locks: TrieMap[String, Semaphore] = TrieMap[String, Semaphore]()

  
  
  
}

@upickle.implicits.allowUnknownKeys(false)
case class Config(
  gitolite: RemoteServer,
  dropbox_path: Option[os.RelPath] = None,
  site_base: Option[String] = None
) derives ReadWriter {}

object Config {

  private val config: Maker[Config] = Rule() {
    @tailrec
    def find(d: os.Path): os.Path = {
      val t = d / "config.json"
      if os.isFile(t) then t else find(d / os.up)
    }
    val cf = find(os.pwd)
    //logger.info(s"loading config from $cf")
    read[Config](os.read(cf))
  }

  val gitolite: Maker[RemoteServer] = Rule(config, null)(_.gitolite)
  val dropbox_path: Maker[Option[os.RelPath]] = Rule(config, null)(_.dropbox_path)
  val site_base: Maker[Option[String]] = Rule(config, null)(_.site_base)
}
