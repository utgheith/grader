package ag.grader.targets

import ag.grader.{Fixture, Milestone, Outbox, ProjectState}

import ag.grader.name.{Alias, CSID}

import ag.grader.json.{StudentResults, RedactedStudentResults, OutcomeStatus, TestId}

import ag.grader.gitolite.Create

import ag.common.TempDirs
import ag.common.given_ReadWriter_SortedMap
import ag.rules.State
import munit.FunSuite

import scala.collection.{SortedMap, SortedSet}

// Grading (Project, Submission) and publishing the results (ag.grader.repos.Grade), on made-up
// repos (see Fixture): past both deadlines, so every milestone is published, on
// its chosen tests, keeping going after a failure
class GradingTests extends FunSuite {
  import ag.grader.Fixture.*

  // a Makefile whose <t>.test passes if <t>.cc says "pass", and otherwise
  // leaves no <t>.result
  private val makefile =
    "%.test:\n\t@if grep -q pass $*.cc; then echo pass > $*.result; fi\n\nclean:\n\t@true\n"

  private val t0 = TestId("t0", "t0")

  // s1 submits; t0 passes, t1 doesn't; m1 is t0, m2 both; and
  // `more` override files (other tests)
  private def setup(root: os.Path, more: (String, String)*) = {
    val (r, source) = Fixture.setup(
      root,
      with_tests(m1_tests = Seq("t0"), m2_tests = Seq("t0", "t1"))
    )
    val _ = r.commit("c1_p1", test_cutoff.minusDays(30), "main.c" -> "x")()
    val sha = r.commit("c1_p1_s1", test_cutoff.minusDays(1), "main.c" -> "mine")()
    val _ = r.commit("c1_p1__override", test_cutoff.minusDays(30),
      (Seq("Makefile" -> makefile, "t0.cc" -> "pass", "t0.ok" -> "", "t1.cc" -> "nope", "t1.ok" -> "") ++ more)*)()
    val ob = Outbox(root / "outbox", r.mirror, root / "ws" / "r3.git")
    (r, source, ob, sha)
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

  test("student_results: the milestone's tests on the graded commit, by alias") {
    val root = TempDirs.dir()
    val (_, source, _, sha) = setup(root)
    val s = State(root / "ws")
    assertEquals(s.track(Fixture.project(source).state), ProjectState(Seq("m1", "m2"), 2))
    val latest = s.track(Fixture.graded(source, Milestone.Live).results(1)).get
    assertEquals(latest.prepare_info.sha, sha)
    assertEquals(latest.alias, Some(Alias(1)))
    assertEquals(
      statuses(latest),
      SortedMap("t0" -> Seq(OutcomeStatus.pass), "t1" -> Seq(OutcomeStatus.unknown))
    )
    // m1: just t0
    val one = s.track(Fixture.graded(source, m1).results(1)).get
    assertEquals(one.outcomes.keySet.map(_.external_name), SortedSet("t0"))
    // no submission, no results
    assertEquals(s.track(Fixture.graded(source, Milestone.Live, CSID("s2")).results(1)), None)
    // scores: m2's tests passed
    assertEquals(
      s.track(Fixture.project(source).milestone(Milestone.Live).scores(1)),
      SortedMap(s1 -> SortedSet(t0))
    )
  }

  test("the results repo: a file per milestone; has_work until the results cover n tries") {
    val root = TempDirs.dir()
    val (r, source, ob, _) = setup(root)
    def has_work(n: Int) =
      Seq(Milestone.Live, m1, m2)
        .exists(milestone => State(root / "ws").track(Fixture.project(source).milestone(milestone).has_work(n)))
    assert(has_work(1))

    val s = State(root / "ws")
    val p = s.track(ag.grader.repos.ResultsRepo(Fixture.project(source)).latest(1))
    assertEquals(p.value.create, Some(Create("empty", Seq("@all"), Seq())))
    val dir = s.checkout(p.data)
    assertEquals(
      os.list(dir).map(_.last).sorted,
      Seq("results_live.json", "results_m1.json", "results_m2.json")
    )
    val latest = upickle.default.read[SortedMap[Alias, RedactedStudentResults]](
      os.read(dir / "results_live.json")
    )
    assertEquals(latest.keySet, SortedSet(Alias(1)))
    assertEquals(
      statuses(latest(Alias(1))),
      SortedMap("t0" -> Seq(OutcomeStatus.pass), "t1" -> Seq(OutcomeStatus.unknown))
    )
    val one = upickle.default.read[SortedMap[Alias, RedactedStudentResults]](
      os.read(dir / "results_m1.json")
    )
    assertEquals(one(Alias(1)).outcomes.keySet.map(_.external_name), SortedSet("t0"))

    // once published (with the aliases), nothing to do for 1 try; more for 2
    assert(ob.record(s.track(ag.grader.repos.AliasesRepo(Fixture.project(source)).latest)))
    as_if_synced(r, ob, "c1_p1__aliases")
    assert(ob.record(p))
    as_if_synced(r, ob, "c1_p1__results")
    assert(!has_work(1))
    assert(has_work(2))
  }

  test("a student's prepared repo: a directory per milestone with a commit to grade; the runs run on it") {
    val root = TempDirs.dir()
    val (_, source, _, sha) = setup(root)
    val s = State(root / "ws")
    val repo = ag.grader.repos.PreparedRepo(Fixture.submission(source))
    val p = s.track(repo.latest)
    assertEquals(p.value.repo, "c1_p1_s1__prepared")
    assertEquals(p.value.create, Some(Create("empty", Seq("s1", "@c1_staff"), Seq("@c1_staff"))))
    assertEquals(os.list(s.checkout(p.data)).map(_.last).sorted, Seq("live", "m1", "m2"))
    // each is the prepared submission's tree, as is: the same runs
    val prepared = s.track(Fixture.submission(source).prepare(sha)).data.id
    for (milestone <- Seq(Milestone.Live, m1, m2))
      assertEquals(s.track(repo.prepared(milestone)).map(_.id), Some(prepared))
    // nothing to grade: no directory, and nothing to run on
    assertEquals(s.track(ag.grader.repos.PreparedRepo(Fixture.submission(source, CSID("s2"))).prepared(Milestone.Live)), None)
    val _ = intercept[Throwable](s.track(Fixture.graded(source, Milestone.Live, CSID("s2")).run_one(true, 1)(t0)))
    // grading publishes it with the student's results
    assert(ag.grader.repos.Grade.all(Fixture.project(source), Seq(s1), 1).map(_.path).contains(repo.latest.path))
    // and, first, what the project publishes: its override, aliases,
    // submitted tests and tests
    assertEquals(ag.grader.repos.Grade.all(Fixture.project(source), Seq(s1), 1).map(_.path).take(4), ag.grader.repos.Grade.project(Fixture.project(source)).map(_.path))
  }

  test("a student's results repo: their outcomes and each test's output files") {
    val root = TempDirs.dir()
    val (r, source, _, sha) = setup(root)
    // what was there before goes, but its dot files
    val _ = r.commit("c1_p1_s1_results", test_cutoff, ".gitignore" -> "*.o\n", "old.result" -> "x")()
    val s = State(root / "ws")
    val p = s.track(ag.grader.repos.StudentResultsRepo(Fixture.submission(source)).latest(1))
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

  test("Live's results show the other good tests, as far as they've run") {
    val root = TempDirs.dir()
    // t2: good but not chosen
    val (_, source, _, _) = setup(root, "t2.cc" -> "pass", "t2.ok" -> "")
    def latest = State(root / "ws")
      .track(Fixture.graded(source, Milestone.Live).results(1))
      .get
      .outcomes
      .keySet
      .map(_.external_name)
    // t2 isn't run for them
    assertEquals(latest, SortedSet("t0", "t1"))
    // once it has run, it's shown (but not in m2)
    val t2 = TestId("t2", "t2")
    val _ = State(root / "ws").track(Fixture.graded(source, Milestone.Live).run_one(false, 1)(t2))
    assertEquals(latest, SortedSet("t0", "t1", "t2"))
    val two = State(root / "ws").track(Fixture.graded(source, m2).results(1)).get
    assertEquals(two.outcomes.keySet.map(_.external_name), SortedSet("t0", "t1"))
  }
}
