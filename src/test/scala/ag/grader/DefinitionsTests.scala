package ag.grader

import ag.common.TempDirs
import ag.r3.{Source, State}
import munit.FunSuite

import java.time.LocalDateTime
import scala.collection.SortedSet

// The course and project definitions (Definitions), on made-up mirrored repos
class DefinitionsTests extends FunSuite {
  import Fixture.{c1, p1, p2}
  private def git(dir: os.Path, args: String*): String =
    os.proc(Seq("git", "-c", "user.name=t", "-c", "user.email=t@x") ++ args)
      .call(cwd = dir, stderr = os.Pipe)
      .out
      .text()
      .trim

  // a bare repo `name` in `mirror`, with one commit on master holding `files`
  private def repo(mirror: os.Path, name: String, files: (String, String)*): Unit = {
    val work = TempDirs.dir()
    val _ = git(work, "init", "-q", "-b", "master")
    for ((f, text) <- files) os.write.over(work / os.RelPath(f), text, createFolders = true)
    val _ = git(work, "add", "-A")
    val _ = git(work, "commit", "-q", "--allow-empty", "-m", s"update $name")
    val dest = mirror / s"$name.git"
    os.remove.all(dest)
    os.makeDir.all(mirror)
    val _ = git(mirror, "clone", "-q", "--mirror", work.toString, dest.toString)
    os.remove.all(work)
  }

  private val cutoff = LocalDateTime.of(2026, 10, 1, 23, 59).nn
  private val s1 = CSID("s1")

  private def raw(active: Boolean) = RawProject(
    active = active, cores = 2,
    code_cutoff = cutoff, test_cutoff = cutoff.minusDays(7).nn,
    ignore_tests = false, phase1_tests = Seq("b", "a"), phase1_weight = 30,
    phase2_tests = Seq(), phase2_weight = 70, test_weights = Seq(),
    bad_tests = Seq(), docker_file = Some("Dockerfile"), test_extensions = Seq("ok"),
    staff = Seq(CSID("ta")), extensions = Extensions(Map(s1 -> 2), Map(s1 -> 3)),
    iterations = Iterations(2, 4)
  )

  private def courses(mirror: os.Path, p1_active: Boolean = true): Unit = {
    val rc = RawCourseNotSorted(
      active = true,
      notifications = NotificationConfig(false, None, false, false, false, None),
      projects = Map("p1" -> raw(p1_active), "p2" -> raw(true)),
      staff = SortedSet(CSID("prof"))
    )
    repo(mirror, "courses_config", "courses.json" -> upickle.default.write(Map("c1" -> rc)))
  }

  test("courses and settings come from courses_config; a project is active only if its repo is there") {
    val root = TempDirs.dir()
    val mirror = root / "mirror"
    courses(mirror)
    repo(mirror, "c1_p1", "README" -> "base")
    val d = Definitions(Source(mirror))
    val s = State(root / "ws")
    assertEquals(s.track(d.course_names), SortedSet(c1))
    assertEquals(s.track(d.active_courses(Definitions.any)), SortedSet(c1))
    assertEquals(s.track(d.course_staff(c1)), SortedSet(CSID("prof")))
    assertEquals(s.track(d.project_names(c1)), SortedSet(p1, p2))
    // p2 has no repo
    assertEquals(s.track(d.project_active(c1, p1)), true)
    assertEquals(s.track(d.project_active(c1, p2)), false)
    assertEquals(s.track(d.active_projects(c1, Definitions.any)), SortedSet(p1))
    // picked by a regex on the name
    assertEquals(s.track(d.active_courses("c[2-9]".r)), SortedSet[CourseName]())
    assertEquals(s.track(d.active_projects(c1, "p[1-2]".r)), SortedSet(p1))
    assertEquals(s.track(d.active_projects(c1, "p2".r)), SortedSet[ProjectName]())
    assertEquals(s.track(d.phase1_tests(c1, p1)), SortedSet("a", "b"))
    assertEquals(s.track(d.docker_file(c1, p1)), Some("Dockerfile"))
    assertEquals(s.track(d.project_staff(c1, p1)), SortedSet(CSID("ta")))
    assertEquals(s.track(d.find_project(c1, ProjectName("nope"))), None)
    assertEquals(s.track(d.find_course(CourseName("nope"))), None)
  }

  test("deadlines: phase 1 at the test cutoff, phase 2 and the latest at the code cutoff, plus extensions") {
    val root = TempDirs.dir()
    val mirror = root / "mirror"
    courses(mirror)
    val d = Definitions(Source(mirror))
    val s = State(root / "ws")
    def deadline(csid: CSID, phase: Phase) = s.track(d.deadline(c1, p1, csid, phase))
    val other = CSID("s2")
    assertEquals(deadline(other, Phase.One), cutoff.minusDays(7).nn)
    assertEquals(deadline(other, Phase.Two), cutoff)
    assertEquals(deadline(other, Phase.Latest), cutoff)
    assertEquals(deadline(s1, Phase.One), cutoff.minusDays(5).nn)
    assertEquals(deadline(s1, Phase.Two), cutoff.plusDays(3).nn)
    assertEquals(deadline(s1, Phase.Latest), cutoff.plusDays(3).nn)
  }

  test("a new State sees a change to courses_config, and to whether a project repo is there") {
    val root = TempDirs.dir()
    val mirror = root / "mirror"
    courses(mirror)
    val d = Definitions(Source(mirror))
    assertEquals(State(root / "ws").track(d.active_projects(c1, Definitions.any)), SortedSet[ProjectName]())
    repo(mirror, "c1_p1", "README" -> "base")
    repo(mirror, "c1_p2", "README" -> "base")
    assertEquals(State(root / "ws").track(d.active_projects(c1, Definitions.any)), SortedSet(p1, p2))
    courses(mirror, p1_active = false)
    assertEquals(State(root / "ws").track(d.active_projects(c1, Definitions.any)), SortedSet(p2))
  }

  test("a missing course or project fails its settings, with its name") {
    val root = TempDirs.dir()
    val mirror = root / "mirror"
    courses(mirror)
    val d = Definitions(Source(mirror))
    val e = intercept[Throwable](State(root / "ws").track(d.cores(c1, ProjectName("nope"))))
    assert(clue(Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null).map(_.getMessage).mkString(" | ")).contains("c1/nope"))
  }
}
