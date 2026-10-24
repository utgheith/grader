package ag.events

// A throwaway gitolite server in a container, built from src/test/gitolite
// to behave like the real one (its `history` command and the hook that feeds
// it, wildcard repos, fork, perms). Users get throwaway ssh keys; `admin`
// plays the grader. Needs podman: GitoliteServer.image is None without it,
// and tests using it should skip themselves.
final class GitoliteServer(
    conf: String,
    users: Seq[String],
    // a post-receive hook to try instead of the server's
    hook: Option[os.Path] = None,
    // a directory with repositories/*.git to start from (copied in)
    seed: Option[os.Path] = None
) extends AutoCloseable {
  private val dir = os.temp.dir(deleteOnExit = true)
  private val setup = dir / "setup"

  private def key(user: String): os.Path = dir / user
  for (user <- "admin" +: users) {
    val _ = os
      .proc(
        "ssh-keygen",
        "-q",
        "-t",
        "ed25519",
        "-N",
        "",
        "-C",
        user,
        "-f",
        key(user)
      )
      .call()
    val pub = os.read(key(user) / os.up / s"$user.pub")
    if (user == "admin")
      os.write(setup / "admin.pub", pub, createFolders = true)
    else os.write(setup / "keys" / s"$user.pub", pub, createFolders = true)
  }
  os.write(setup / "gitolite.conf", conf, createFolders = true)
  hook.foreach(h => os.copy(h, setup / "post-receive"))

  private val container: String = os
    .proc(
      Seq("podman", "run", "-d", "-p", "127.0.0.1::22")
        ++ Seq("-v", s"${setup.toString}:/setup:ro,Z")
        ++ seed.toSeq.flatMap(d => Seq("-v", s"${d.toString}:/seed:ro"))
        :+ GitoliteServer.image.getOrElse(
          throw IllegalStateException("no podman")
        )
    )
    .call()
    .out
    .trim()

  val port: Int =
    os.proc("podman", "port", container, "22")
      .call()
      .out
      .trim()
      .split(":")
      .last
      .toInt

  val server = "git@127.0.0.1"

  // the ssh command (without host or port) that acts as `user`
  def ssh(user: String): Seq[String] =
    Seq(
      "ssh",
      "-i",
      key(user).toString,
      "-o",
      "StrictHostKeyChecking=no",
      "-o",
      "UserKnownHostsFile=/dev/null",
      "-o",
      "IdentitiesOnly=yes",
      "-o",
      "LogLevel=ERROR"
    )

  // environment for git, acting as `user`
  def git_env(user: String): Map[String, String] =
    Map("GIT_SSH_COMMAND" -> ssh(user).mkString(" "))

  def url(repo: String): String = s"ssh://$server:$port/$repo"

  def run(user: String, command: String*): os.CommandResult =
    os.proc(ssh(user), "-p", port.toString, server, command)
      .call(check = false, stdout = os.Pipe, stderr = os.Pipe)

  // Commits a file to `repo` (creating it, for wildcard repos) as `user`;
  // returns the new sha.
  def push(
      user: String,
      repo: String,
      file: String,
      content: String
  ): String = {
    val work = os.temp.dir(dir = dir)
    def git(args: String*) =
      os.proc("git" +: args)
        .call(cwd = work, env = git_env(user), stderr = os.Pipe)
    val _ = git("init", "-q", "-b", "master")
    // a repo that doesn't exist yet can't be fetched: start from nothing
    val _ = os
      .proc("git", "fetch", "-q", url(repo))
      .call(cwd = work, env = git_env(user), stderr = os.Pipe, check = false)
    val fetched = os
      .proc("git", "rev-parse", "-q", "--verify", "FETCH_HEAD")
      .call(cwd = work, check = false, stdout = os.Pipe)
      .exitCode == 0
    if (fetched) { val _ = git("reset", "-q", "--hard", "FETCH_HEAD") }
    os.write.over(work / file, content)
    val _ = git("add", file)
    val _ = git(
      "-c",
      "user.name=t",
      "-c",
      "user.email=t@x",
      "commit",
      "-q",
      "-m",
      file
    )
    val _ = git("push", "-q", url(repo), "HEAD:master")
    git("rev-parse", "HEAD").out.trim()
  }

  // wait until gitolite answers
  private var tries = 0
  while (run("admin", "info").exitCode != 0) {
    tries += 1
    if (tries > 1500) // a seed can take a while to copy in
      throw IllegalStateException("gitolite container didn't start")
    Thread.sleep(200)
  }

  def close(): Unit = {
    val _ = os.proc("podman", "rm", "-f", container).call(check = false)
  }
}

object GitoliteServer {
  // the image, built once per run (cached by podman); None without podman
  lazy val image: Option[String] =
    if (scala.util.Try(os.proc("podman", "--version").call()).isFailure) None
    else {
      val tag = "grader-gitolite-test"
      val _ = os
        .proc(
          "podman",
          "build",
          "-q",
          "-t",
          tag,
          os.pwd / "src" / "test" / "gitolite"
        )
        .call(stderr = os.Pipe)
      Some(tag)
    }
}
