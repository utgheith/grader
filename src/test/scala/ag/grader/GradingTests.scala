package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.State
import munit.FunSuite

import scala.collection.{SortedMap, SortedSet}

// Grading (Grading) and publishing the results (Publishers), on made-up
// repos (see Fixture): past both deadlines, so every phase is published, on
// its chosen tests, keeping going after a failure
class GradingTests extends FunSuite {
  import Fixture.*

  // a Makefile whose <t>.test passes if <t>.cc says "pass", and otherwise
  // leaves no <t>.result
  private val makefile =
    "%.test:\n\t@if grep -q pass $*.cc; then echo pass > $*.result; fi\n\nclean:\n\t@true\n"

  private val t0 = TestId("t0", "t0")

  // s1 submits; t0 passes, t1 doesn't; phase 1 is t0, phase 2 both
  private def setup(root: os.Path) = {
    val (r, subs) = Fixture.setup(
      root,
      _.copy(phase1_tests = Seq("t0"), phase2_tests = Seq("t0", "t1"))
    )
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val sha = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "main.c" -> "mine")()
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(30),
      "Makefile" -> makefile, "t0.cc" -> "pass", "t0.ok" -> "", "t1.cc" -> "nope", "t1.ok" -> "")()
    val pubs = Publishers(Grading(TestRuns(subs)), Dropbox(None))
    val ob = Outbox(root / "outbox", r.mirror, root / "ws" / "r3.git")
    (r, pubs, ob, sha)
  }

  private def git(dir: os.Path, args: String*): String =
    os.proc("git" +: args).call(cwd = dir).out.text().trim

  // what `sync` would do for `repo`: the mirror gets the outbox's commit
  private def as_if_synced(r: Repos, ob: Outbox, repo: String): Unit = {
    val m = r.mirror / s"$repo.git"
    if (!os.exists(m)) { val _ = git(r.mirror, "init", "-q", "--bare", m.toString) }
    val _ = git(ob.dir(repo), "push", "-q", m.toString, "master:refs/heads/master")
  }

  private def statuses(r: RedactedStudentResults | StudentResults) = {
    val outcomes = r match {
      case r: RedactedStudentResults => r.outcomes
      case r: StudentResults         => r.outcomes
    }
    outcomes.map((id, o) => (id.external_name, o.outcomes.map(_._1)))
  }

  test("student_results: the phase's tests on the graded commit, by alias") {
    val root = os.temp.dir()
    val (_, pubs, _, sha) = setup(root)
    val g = pubs.grading
    val s = State(root / "ws")
    assertEquals(s.track(g.state("c1", "p1")), ProjectState.AfterSecondDeadline)
    val latest = s.track(g.student_results("c1", "p1", s1, Phase.Latest, 1)).get
    assertEquals(latest.prepare_info.sha, sha)
    assertEquals(latest.alias, Some(Alias(1)))
    assertEquals(
      statuses(latest),
      SortedMap("t0" -> Seq(OutcomeStatus.pass), "t1" -> Seq(OutcomeStatus.unknown))
    )
    // phase 1: just t0
    val one = s.track(g.student_results("c1", "p1", s1, Phase.One, 1)).get
    assertEquals(one.outcomes.keySet.map(_.external_name), SortedSet("t0"))
    // no submission, no results
    assertEquals(s.track(g.student_results("c1", "p1", CSID("s2"), Phase.Latest, 1)), None)
    // scores: the phase 2 tests passed
    assertEquals(
      s.track(g.scores("c1", "p1", Phase.Latest, 1)),
      SortedMap(s1 -> SortedSet(t0))
    )
  }

  test("the results repo: a file per phase; has_work until the results cover n tries") {
    val root = os.temp.dir()
    val (r, pubs, ob, _) = setup(root)
    val g = pubs.grading
    def has_work(n: Int) =
      Seq(Phase.Latest, Phase.One, Phase.Two)
        .exists(ph => State(root / "ws").track(g.has_work("c1", "p1", ph, n)))
    assert(has_work(1))

    val s = State(root / "ws")
    val p = s.track(pubs.results("c1", "p1", 1))
    assertEquals(p.value.create, Some(Create("empty", Seq("@all"), Seq())))
    val dir = s.checkout(p.data)
    assertEquals(
      os.list(dir).map(_.last).sorted,
      Seq("results.json", "results_phase1.json", "results_phase2.json")
    )
    val latest = upickle.default.read[SortedMap[Alias, RedactedStudentResults]](
      os.read(dir / "results.json")
    )
    assertEquals(latest.keySet, SortedSet(Alias(1)))
    assertEquals(
      statuses(latest(Alias(1))),
      SortedMap("t0" -> Seq(OutcomeStatus.pass), "t1" -> Seq(OutcomeStatus.unknown))
    )
    val one = upickle.default.read[SortedMap[Alias, RedactedStudentResults]](
      os.read(dir / "results_phase1.json")
    )
    assertEquals(one(Alias(1)).outcomes.keySet.map(_.external_name), SortedSet("t0"))

    // once published (with the aliases), nothing to do for 1 try; more for 2
    assert(ob.record(s.track(pubs.aliases("c1", "p1"))))
    as_if_synced(r, ob, "c1_p1__aliases")
    assert(ob.record(p))
    as_if_synced(r, ob, "c1_p1__results")
    assert(!has_work(1))
    assert(has_work(2))
  }

  test("a student's results repo: their outcomes and each test's output files") {
    val root = os.temp.dir()
    val (r, pubs, _, sha) = setup(root)
    // what was there before goes, but its dot files
    val _ = r.commit("c1_p1_s1_results", test_cutoff, ".gitignore" -> "*.o\n", "old.result" -> "x")()
    val s = State(root / "ws")
    val p = s.track(pubs.student_results("c1", "p1", s1, 1))
    assertEquals(p.value.create, Some(Create("empty", Seq("s1", "@c1_staff"), Seq())))
    val dir = s.checkout(p.data)
    assertEquals(
      os.list(dir).map(_.last).sorted,
      Seq(".gitignore", "t0.ok", "t0.result", "t1.ok", "the_results.json")
    )
    val mine = upickle.default.read[StudentResults](os.read(dir / "the_results.json"))
    assertEquals((mine.csid, mine.prepare_info.sha), (s1, sha))
    assertEquals(os.read(dir / "t0.result").trim, "pass")
  }
}
