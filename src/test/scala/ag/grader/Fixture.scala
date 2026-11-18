package ag.grader

import ag.grader.targets.{Course, Project, Submission, SubmissionMilestone}

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.json.{
  RawCourseNotSorted,
  RawProject,
  RawMilestone,
  Iterations,
  NotificationConfig
}

import ag.common.given_ReadWriter_SortedMap
import ag.rules.Source

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

// Made-up repos mirrored as LocalMirror does, for the r3 grader's tests: a
// course c1 with a project p1 and students s1 and s2
object Fixture {
  val zone: ZoneId = ZoneId.systemDefault.nn
  val code_cutoff: LocalDateTime = LocalDateTime.of(2026, 10, 10, 23, 59).nn
  val test_cutoff: LocalDateTime = LocalDateTime.of(2026, 10, 3, 23, 59).nn
  val s1: CSID = CSID("s1")
  val c1: CourseName = CourseName("c1")
  val c2: CourseName = CourseName("c2")
  val p1: ProjectName = ProjectName("p1")
  val p2: ProjectName = ProjectName("p2")

  // p1's milestones: m1, at the test cutoff, then m2, at the code cutoff
  val m1: Milestone = Milestone.Named("m1")
  val m2: Milestone = Milestone.Named("m2")
  def milestones(
      m1_tests: Seq[String] = Seq(),
      m2_tests: Seq[String] = Seq()
  ): Seq[RawMilestone] = Seq(
    RawMilestone("m1", test_cutoff, m1_tests, 1),
    RawMilestone("m2", code_cutoff, m2_tests, 1)
  )
  // p1 with those tests in its milestones
  def with_tests(
      m1_tests: Seq[String] = Seq(),
      m2_tests: Seq[String] = Seq()
  ): RawProject => RawProject =
    _.copy(milestones = milestones(m1_tests, m2_tests))

  // c1's project `p`, and a student's submission in its p1, from `source`
  def course(source: Source): Course = repos.CoursesConfig(source).course(c1)
  def project(source: Source, p: ProjectName = p1): Project =
    course(source).project(p)
  def submission(source: Source, csid: CSID = s1): Submission =
    project(source).submission(csid)
  // and its milestone
  def graded(
      source: Source,
      milestone: Milestone,
      csid: CSID = s1
  ): SubmissionMilestone =
    submission(source, csid).milestone(milestone)

  // seconds since the epoch of a local time
  def at(t: LocalDateTime): Long = ZonedDateTime.of(t, zone).nn.toEpochSecond

  def git(dir: os.Path, env: Map[String, String], args: String*): String =
    os.proc(Seq("git", "-c", "user.name=t", "-c", "user.email=t@x") ++ args)
      .call(cwd = dir, env = env, stderr = os.Pipe)
      .out
      .text()
      .trim

  // A work tree per repo under root/work; each commit is mirrored into
  // root/mirror. A name "repo:branch" is that branch of a shared repo (see
  // ag.grader.repos.StudentRepo), with a work tree of its own (whose master
  // is mirrored as the branch). Returns the commit's id.
  class Repos(root: os.Path) {
    val mirror: os.Path = root / "mirror"
    private def work(name: String): os.Path =
      root / "work" / name.replace(':', '@').replace('/', '@')
    def commit(name: String, when: LocalDateTime, files: (String, String)*)(
        removed: String*
    ): String = {
      val d = work(name)
      if (!os.isDir(d / ".git")) {
        os.makeDir.all(d)
        val _ = git(d, Map(), "init", "-q", "-b", "master")
      }
      for ((f, text) <- files)
        os.write.over(d / os.RelPath(f), text, createFolders = true)
      for (f <- removed) os.remove.all(d / os.RelPath(f))
      val t = s"${at(when)} +0000"
      val env = Map("GIT_COMMITTER_DATE" -> t, "GIT_AUTHOR_DATE" -> t)
      val _ = git(d, env, "add", "-A")
      val _ = git(
        d,
        env,
        "commit",
        "-q",
        "--allow-empty",
        "-m",
        s"at ${when.toString}\n\nmore"
      )
      sync(name)
      git(d, Map(), "rev-parse", "HEAD")
    }
    def note(name: String, ref: String, sha: String, text: String): Unit = {
      val _ = git(
        work(name),
        Map(),
        "notes",
        "--ref",
        ref,
        "add",
        "-f",
        "-m",
        text,
        sha
      )
      sync(name)
    }
    def sync(name: String): Unit = {
      val (repo, refspecs) = name.split(':') match {
        case Array(r, b) =>
          (
            r,
            Seq(
              s"+refs/heads/master:refs/heads/$b",
              "+refs/notes/*:refs/notes/*"
            )
          )
        case _ => (name, Seq("+refs/*:refs/*"))
      }
      val dest = mirror / s"$repo.git"
      os.makeDir.all(mirror)
      if (!os.exists(dest)) {
        val _ = git(mirror, Map(), "init", "-q", "--bare", dest.toString)
      }
      val _ = git(
        dest,
        Map(),
        Seq("fetch", "-q", work(name).toString) ++ refspecs*
      )
    }
  }

  val quiet: NotificationConfig =
    NotificationConfig(false, None, false, false, false, None)

  def setup(
      root: os.Path,
      change: RawProject => RawProject = identity,
      notifications: NotificationConfig = quiet
  ): (Repos, Source) = {
    val r = Repos(root)
    val raw = RawProject(
      active = true,
      cores = 1,
      ignore_tests = false,
      milestones = milestones(),
      test_weights = Seq(),
      bad_tests = Seq(),
      docker_file = None,
      test_extensions = Seq("cc", "ok"),
      staff = Seq(),
      iterations = Iterations(1, 1)
    )
    val rc = RawCourseNotSorted(
      active = true,
      notifications = notifications,
      projects = Map("p1" -> change(raw)),
      staff = SortedSet()
    )
    val _ = r.commit(
      "courses_config",
      test_cutoff,
      "courses.json" -> upickle.default.write(Map("c1" -> rc))
    )()
    val _ = r.commit(
      "c1:grader/enrollment",
      test_cutoff,
      "enrollment.json" -> upickle.default.write(
        SortedMap(s1 -> "k1", CSID("s2") -> "k2")
      )
    )()
    (r, Source(r.mirror))
  }

}
