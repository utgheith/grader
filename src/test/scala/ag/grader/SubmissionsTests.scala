package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.{Source, State}
import munit.FunSuite

import java.time.{Instant, ZoneOffset, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

// The submissions (Submissions), on made-up repos mirrored as LocalMirror does
class SubmissionsTests extends FunSuite {
  import Fixture.*

  private val base_files = Seq("main.c" -> "int main() {}", "t0.cc" -> "base test", "REPORT.txt" -> "fill me in")

  test("a submission is a work repo that's more than the project's starting code") {
    val root = os.temp.dir()
    val (r, subs) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), base_files*)()
    // s1 changed something; s2's repo is still the starting code
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(30), base_files*)()
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "main.c" -> "int main() { return 1; }")()
    val _ = r.commit("c1_p1_s2", test_cutoff.minusDays(30), base_files*)()
    val s = State(root / "ws")
    assertEquals(s.track(subs.enrolled_csids("c1")), SortedSet(s1, CSID("s2")))
    assertEquals(s.track(subs.students_with_submission("c1", "p1")), SortedSet(s1))
    assertEquals(s.track(subs.has_student_report("c1", "p1", s1)), false)
  }

  test("the commit graded: the last before each deadline, master for the latest, or what commit_id_<n> names") {
    val root = os.temp.dir()
    val (r, subs) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), base_files*)()
    val c1 = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "main.c" -> "v1")()
    val c2 = r.commit("c1_p1_s1", code_cutoff.minusDays(1), "main.c" -> "v2")()
    val c3 = r.commit("c1_p1_s1", code_cutoff.plusDays(1), "main.c" -> "v3")()
    r.note("c1_p1_s1", "push_time", c2, s"${at(code_cutoff) - 100}\n${at(code_cutoff)}")
    def graded(phase: Phase) = State(root / "ws").track(subs.graded_commit("c1", "p1", s1, phase))
    assertEquals(graded(Phase.One).map(_.sha), Some(c1))
    assertEquals(graded(Phase.Two).map(_.sha), Some(c2))
    assertEquals(graded(Phase.Latest).map(_.sha), Some(c3))
    assertEquals(graded(Phase.Two).map(_.source), Some(CommitSource.Latest))
    assertEquals(
      graded(Phase.Two).map(_.commit_time),
      Some(ZonedDateTime.ofInstant(Instant.ofEpochSecond(at(code_cutoff.minusDays(1))), ZoneOffset.UTC).nn)
    )
    // the earliest push counts
    assertEquals(graded(Phase.Two).flatMap(_.push_time), Some(Instant.ofEpochSecond(at(code_cutoff) - 100).nn))
    assertEquals(graded(Phase.One).flatMap(_.push_time), None)
    // phase 2 asks for c1 by a short id; one that names nothing is ignored
    val _ = r.commit("c1_p1_s1", code_cutoff.plusDays(2), "commit_id_2" -> s"${c1.take(10)}\n", "commit_id_1" -> "nope\n")()
    assertEquals(graded(Phase.Two).map(g => (g.sha, g.source)), Some((c1, CommitSource.FromFile("commit_id_2", c1.take(10)))))
    assertEquals(graded(Phase.One).map(g => (g.sha, g.source)), Some((c1, CommitSource.Latest)))
    // late: phase 2 is graded at c1 (on time); the latest is late
    assertEquals(State(root / "ws").track(subs.is_late("c1", "p1", s1, Phase.Two)), false)
    assertEquals(State(root / "ws").track(subs.is_late("c1", "p1", s1, Phase.Latest)), true)
  }

  test("prepare: the commit's files, the overrides on top, no tests, and prepared_commit") {
    val root = os.temp.dir()
    val (r, subs) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), base_files*)()
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(30), base_files*)()
    val sha = r.commit("c1_p1_s1", test_cutoff.minusDays(1),
      "main.c" -> "mine", "Makefile" -> "student's", "s1.cc" -> "my test", "dir/x.ok" -> "kept: not at the top")()
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(30), "Makefile" -> "the course's", "t0.ok" -> "an override test")()
    val s = State(root / "ws")
    val dir = s.checkout(s.track(subs.prepare("c1", "p1", s1, sha)).data)
    def read(f: String) = os.read(dir / os.RelPath(f))
    assertEquals(read("main.c"), "mine")
    assertEquals(read("Makefile"), "the course's")
    assertEquals(read("dir/x.ok"), "kept: not at the top")
    for (gone <- Seq("s1.cc", "t0.cc", "t0.ok")) assert(!os.exists(dir / gone), gone)
    val Seq(first, second) = os.read.lines(dir / "prepared_commit").take(2)
    assertEquals(first, sha)
    assertEquals(
      java.time.OffsetDateTime.parse(second).nn.toEpochSecond,
      at(test_cutoff.minusDays(1))
    )
  }

  test("late commits, oldest first; the student's test, as of the test cutoff") {
    val root = os.temp.dir()
    val (r, subs) = setup(root)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), base_files*)()
    val t1 = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "s1.cc" -> "test", "s1.ok" -> "expected")()
    val _ = r.commit("c1_p1_s1", test_cutoff.plusDays(1), "s1.cc" -> "too late to count")()
    val l1 = r.commit("c1_p1_s1", code_cutoff.plusHours(1), "main.c" -> "late")()
    val l2 = r.commit("c1_p1_s1", code_cutoff.plusDays(2), "main.c" -> "later")()
    val s = State(root / "ws")
    val late = s.track(subs.late_commits("c1", "p1", s1))
    assertEquals(late.map(_.hash), Seq(l1, l2))
    assertEquals(late.map(_.delay.toHours), Seq(1L, 48L))
    assertEquals(late.head.message, s"at ${code_cutoff.plusHours(1).toString}")
    val tests = s.track(subs.student_tests_by_csid("c1", "p1"))
    assertEquals(tests.get(s1).map(_.value), Some(t1))
    val dir = s.checkout(tests(s1).data)
    assertEquals(os.read(dir / "s1.cc"), "test")
    assertEquals(os.list(dir).map(_.last).toSet, Set("s1.cc", "s1.ok"))
  }

  test("a commit's subject is git's %s") {
    val subs = Submissions(Definitions(Source(os.temp.dir())))
    assertEquals(subs.subject("\n\n  a  b  \n  c d  \n\nbody"), "  a  b   c d")
    assertEquals(subs.subject(" x\ny"), " x y")
    assertEquals(subs.subject("\t\ttab \nnext"), "\t\ttab next")
  }

  test("a student test can include a directory (a \"dir\" extension)") {
    val root = os.temp.dir()
    val (r, subs0) = setup(root)
    val _ = subs0
    // a project whose tests are <name>.cc and <name>.dir/
    val rc = RawCourseNotSorted(
      active = true,
      notifications = NotificationConfig(false, None, false, false, false, None),
      projects = Map("p2" -> RawProject(
        active = true, cores = 1, code_cutoff = code_cutoff, test_cutoff = test_cutoff,
        ignore_tests = false, phase1_tests = Seq(), phase1_weight = 1,
        phase2_tests = Seq(), phase2_weight = 1, test_weights = Seq(), bad_tests = Seq(),
        docker_file = None, test_extensions = Seq("cc", "dir"), staff = Seq(),
        extensions = Extensions(Map(), Map()), iterations = Iterations(1, 1))),
      staff = SortedSet()
    )
    val _ = r.commit("courses_config", test_cutoff, "courses.json" -> upickle.default.write(Map("c1" -> rc)))()
    val _ = r.commit("c1_p2", test_cutoff.minusDays(30), "main.c" -> "base")()
    val t = r.commit("c1_p2_s1", test_cutoff.minusDays(1), "s1.cc" -> "test", "s1.dir/a/b" -> "deep", "s1.dir/c" -> "top")()
    val subs = Submissions(Definitions(Source(r.mirror)))
    val s = State(root / "ws")
    val tests = s.track(subs.student_tests_by_csid("c1", "p2"))
    assertEquals(tests.get(s1).map(_.value), Some(t))
    val dir = s.checkout(tests(s1).data)
    assertEquals(os.read(dir / "s1.dir" / "a" / "b"), "deep")
    assertEquals(os.read(dir / "s1.dir" / "c"), "top")
    assertEquals(os.read(dir / "s1.cc"), "test")
  }

  test("published results and aliases, read back; unreadable results fail with what rewrites them") {
    val root = os.temp.dir()
    val (r, subs) = setup(root)
    val s = State(root / "ws")
    assertEquals(s.track(subs.aliases("c1", "p1")), SortedMap[CSID, Alias]())
    assertEquals(s.track(subs.student_results("c1", "p1", s1)), None)
    val _ = r.commit("c1_p1__aliases", test_cutoff, "aliases.json" -> upickle.default.write(SortedMap(s1 -> Alias(3))))()
    val _ = r.commit("c1_p1_s1_results", test_cutoff, "the_results.json" -> "{\"not\": \"results\"}")()
    val s2 = State(root / "ws")
    assertEquals(s2.track(subs.aliases("c1", "p1")).get(s1).map(_.value), Some(3))
    assertEquals(s2.track(subs.student_results_readable("c1", "p1", s1)), false)
    val e = intercept[Throwable](s2.track(subs.student_results("c1", "p1", s1)))
    val all = Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null).map(_.getMessage).mkString(" | ")
    assert(clue(all).contains("publish_results -c c1 -p p1 -s s1"))
  }
}
