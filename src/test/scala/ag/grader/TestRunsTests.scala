package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.State
import munit.FunSuite

import scala.collection.{SortedMap, SortedSet}

// The tests and their runs (TestRuns), on made-up repos (see Fixture)
class TestRunsTests extends FunSuite {
  import Fixture.*

  private val s2 = CSID("s2")
  private val base_files = Seq("main.c" -> "int main() {}", "REPORT.txt" -> "fill me in")

  // a Makefile whose <t>.test passes if <t>.cc says "pass", and otherwise
  // leaves no <t>.result
  private val makefile =
    "%.test:\n\t@if grep -q pass $*.cc; then echo pass > $*.result; fi\n\nclean:\n\t@true\n"

  private def setup(root: os.Path, change: RawProject => RawProject = identity) = {
    val (r, subs) = Fixture.setup(root, change)
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), base_files*)()
    (r, TestRuns(subs))
  }

  test("aliases: the published ones, and new ones numbered on from the highest") {
    val root = os.temp.dir()
    val (r, t) = setup(root)
    for (csid <- Seq(s1, s2))
      val _ = r.commit(s"c1_p1_${csid.value}", test_cutoff.minusDays(1), "main.c" -> csid.value)()
    val _ = r.commit("c1_p1__aliases", test_cutoff, "aliases.json" -> upickle.default.write(SortedMap(s2 -> Alias(5))))()
    assertEquals(State(root / "ws").track(t.aliases(c1, p1)), SortedMap(s1 -> Alias(6), s2 -> Alias(5)))
  }

  test("the tests: students' by alias, the override repo's complete ones; the sets; a test's files") {
    val root = os.temp.dir()
    val (r, t) = setup(root, _.copy(phase1_tests = Seq("t0"), phase2_tests = Seq("001", "t0"), bad_tests = Seq("s1")))
    val _ = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "s1.cc" -> "my test", "s1.ok" -> "my output")()
    val _ = r.commit("c1_p1__aliases", test_cutoff, "aliases.json" -> upickle.default.write(SortedMap(s1 -> Alias(1))))()
    // t1 has no .ok: not a test
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(30),
      "Makefile" -> makefile, "t0.cc" -> "pass", "t0.ok" -> "", "t1.cc" -> "x")()
    val s = State(root / "ws")
    val mine = TestId("001", "s1")
    val t0 = TestId("t0", "t0")
    assertEquals(s.track(t.test_ids(c1, p1)), SortedSet(mine, t0))
    assertEquals(s.track(t.test_ids_for(c1, p1, "good")), SortedSet(t0))
    assertEquals(s.track(t.test_ids_for(c1, p1, "phase1")), SortedSet(t0))
    assertEquals(s.track(t.test_ids_for(c1, p1, "phase2")), SortedSet(t0))
    assertEquals(s.track(t.test_ids_for(c1, p1, "chosen")), SortedSet(t0))
    val dir = s.checkout(s.track(t.test_info(c1, p1, mine)).data)
    assertEquals(os.list(dir).map(_.last).sorted, Seq("001.cc", "001.ok"))
    assertEquals(os.read(dir / "001.cc"), "my test")
    assertEquals(s.track(t.docker_image(c1, p1)), None)
  }

  test("a test's links to other tests' files are followed") {
    val root = os.temp.dir()
    val (r, t) = setup(root)
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(30), "t0.cc" -> "pass", "t0.ok" -> "zero", "t1.cc" -> "one")()
    val work = root / "work" / "c1_p1__override"
    os.symlink(work / "t1.ok", os.RelPath("t0.ok"))
    os.symlink(work / "t2.cc", os.RelPath("./t1.cc"))
    os.symlink(work / "t2.ok", os.RelPath("../elsewhere"))
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(29))()
    val s = State(root / "ws")
    def files(id: String) = {
      val dir = s.checkout(s.track(t.test_info(c1, p1, TestId(id, id))).data)
      os.list(dir).map(f => (f.last, os.isLink(f), os.read(f))).sorted
    }
    assertEquals(files("t1"), Seq(("t1.cc", false, "one"), ("t1.ok", false, "zero")))
    // a link out of the tests: no such file
    assertEquals(files("t2"), Seq(("t2.cc", false, "one")))
  }

  test("run_one: in a copy of the prepared commit, without the student's own <test>.*; runs add up to n") {
    val root = os.temp.dir()
    val (r, t) = setup(root)
    // a result of their own for t1 is removed before t1 runs
    val sha = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "main.c" -> "mine", "t1.result" -> "pass")()
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(30),
      "Makefile" -> makefile, "t0.cc" -> "pass", "t0.ok" -> "", "t1.cc" -> "nope", "t1.ok" -> "")()
    def run(id: String, keep_going: Boolean) =
      State(root / "ws").track(t.run_one(keep_going, 2)(c1, p1, s1, sha, TestId(id, id)))
    def statuses(id: String, keep_going: Boolean) = run(id, keep_going).value.outcomes.map(_._1)

    assertEquals(statuses("t0", true), Seq(OutcomeStatus.pass))
    assertEquals(statuses("t0", true), Seq(OutcomeStatus.pass, OutcomeStatus.pass))
    // n runs: no more
    assertEquals(statuses("t0", true), Seq(OutcomeStatus.pass, OutcomeStatus.pass))
    val out = run("t0", true)
    assertEquals(out.value.commit_id, Some(sha))
    val s = State(root / "ws")
    assertEquals(s.list(out.data), Some(Seq("t0.ok", "t0.result")))

    assertEquals(statuses("t1", false), Seq(OutcomeStatus.unknown))
    // not keeping going: it stops at the failure
    assertEquals(statuses("t1", false), Seq(OutcomeStatus.unknown))
    // the prepared commit itself is untouched
    val prepared = s.checkout(s.track(t.subs.prepare(c1, p1, s1, sha)).data)
    assertEquals(os.read(prepared / "t1.result"), "pass")
    assert(!os.exists(prepared / "t0.result"))
  }
}
