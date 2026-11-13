package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.Source

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

// Made-up repos mirrored as LocalMirror does, for the r3 grader's tests: a
// course c1 with a project p1 and students s1 and s2
object Fixture {
  val zone: ZoneId = ZoneId.systemDefault.nn
  val code_cutoff: LocalDateTime = LocalDateTime.of(2026, 10, 10, 23, 59).nn
  val test_cutoff: LocalDateTime = LocalDateTime.of(2026, 10, 3, 23, 59).nn
  val s1: CSID = CSID("s1")

  // seconds since the epoch of a local time
  def at(t: LocalDateTime): Long = ZonedDateTime.of(t, zone).nn.toEpochSecond

  def git(dir: os.Path, env: Map[String, String], args: String*): String =
    os.proc(Seq("git", "-c", "user.name=t", "-c", "user.email=t@x") ++ args)
      .call(cwd = dir, env = env, stderr = os.Pipe)
      .out
      .text()
      .trim

  // A work tree per repo under root/work; each commit is mirrored into
  // root/mirror. Returns the commit's id.
  class Repos(root: os.Path) {
    val mirror: os.Path = root / "mirror"
    def commit(name: String, when: LocalDateTime, files: (String, String)*)(
        removed: String*
    ): String = {
      val d = root / "work" / name
      if (!os.isDir(d / ".git")) {
        os.makeDir.all(d)
        val _ = git(d, Map(), "init", "-q", "-b", "master")
      }
      for ((f, text) <- files) os.write.over(d / os.RelPath(f), text, createFolders = true)
      for (f <- removed) os.remove.all(d / os.RelPath(f))
      val t = s"${at(when)} +0000"
      val env = Map("GIT_COMMITTER_DATE" -> t, "GIT_AUTHOR_DATE" -> t)
      val _ = git(d, env, "add", "-A")
      val _ = git(d, env, "commit", "-q", "--allow-empty", "-m", s"at ${when.toString}\n\nmore")
      sync(name)
      git(d, Map(), "rev-parse", "HEAD")
    }
    def note(name: String, ref: String, sha: String, text: String): Unit = {
      val _ = git(root / "work" / name, Map(), "notes", "--ref", ref, "add", "-f", "-m", text, sha)
      sync(name)
    }
    def sync(name: String): Unit = {
      val dest = mirror / s"$name.git"
      os.makeDir.all(mirror)
      if (os.exists(dest)) {
        val _ = git(dest, Map(), "fetch", "-q", "--prune", (root / "work" / name).toString, "+refs/*:refs/*")
      } else {
        val _ = git(mirror, Map(), "clone", "-q", "--mirror", (root / "work" / name).toString, dest.toString)
      }
    }
  }

  def setup(root: os.Path, change: RawProject => RawProject = identity): (Repos, Submissions) = {
    val r = Repos(root)
    val raw = RawProject(
      active = true, cores = 1, code_cutoff = code_cutoff, test_cutoff = test_cutoff,
      ignore_tests = false, phase1_tests = Seq(), phase1_weight = 1,
      phase2_tests = Seq(), phase2_weight = 1, test_weights = Seq(), bad_tests = Seq(),
      docker_file = None, test_extensions = Seq("cc", "ok"), staff = Seq(),
      extensions = Extensions(Map(), Map()), iterations = Iterations(1, 1)
    )
    val rc = RawCourseNotSorted(
      active = true,
      notifications = NotificationConfig(false, None, false, false, false, None),
      projects = Map("p1" -> change(raw)),
      staff = SortedSet()
    )
    val _ = r.commit("courses_config", test_cutoff, "courses.json" -> upickle.default.write(Map("c1" -> rc)))()
    val _ = r.commit("c1__enrollment", test_cutoff,
      "enrollment.json" -> upickle.default.write(SortedMap(s1 -> "k1", CSID("s2") -> "k2")))()
    (r, Submissions(Definitions(Source(r.mirror))))
  }

}
