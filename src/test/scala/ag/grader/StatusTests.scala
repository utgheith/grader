package ag.grader

import ag.common.given_ReadWriter_SortedMap
import munit.FunSuite
import ag.r2.{create_data, Scope, State, Target, WithData}

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}

// The repos under `root`, standing in for Gitolite.mirror: each is copied
// into its target's data, again in each State (the targets have no
// dependencies)
class LocalRepos(root: os.Path) extends Scope("local") {
  val repo: String => Target[WithData[Boolean]] = fun { (name: String) =>
    target(EmptyTuple) { (producer, _) =>
      create_data(producer)(_.lastOpt.contains(".git")) { dir =>
        val src = root / name
        if (!os.isDir(src / ".git")) false
        else {
          os.list(src).foreach(f => os.copy(f, dir / f.last))
          true
        }
      }
    }
  }
}

// The status pages, on made-up committed repos
class StatusTests extends FunSuite {
  // the pages, with their data worked out by a read-only State
  private def status_of(root: os.Path): Status = {
    val state = State(root / "status-workspace", read_only = true)
    Status(StatusData(LocalRepos(root).repo), () => state)
  }

  private val now = LocalDateTime.now().nn
  private val course = Course("c1")
  private val project = Project(course, "p1")
  private val s1 = CSID("s1")
  private val s2 = CSID("s2")

  private def git(dir: os.Path, args: String*): String =
    os.proc(Seq("git", "-c", "user.name=t", "-c", "user.email=t@x") ++ args)
      .call(cwd = dir, stderr = os.Pipe)
      .out
      .text()
      .trim

  // a repo, committed, with `files`
  private def repo(root: os.Path, name: String, files: (String, String)*): os.Path = {
    val d = root / name
    os.makeDir.all(d)
    if (!os.isDir(d / ".git")) { val _ = git(d, "init", "-q", "-b", "master") }
    for ((f, text) <- files) os.write.over(d / os.RelPath(f), text, createFolders = true)
    val _ = git(d, "add", "-A")
    val _ = git(d, "commit", "-q", "--allow-empty", "-m", s"update $name")
    d
  }

  private def outcome(test: String, runs: (OutcomeStatus, Option[Double])*) =
    RedactedTestId(test) -> RedactedOutcome(project, RedactedTestId(test), runs)

  private def setup(root: os.Path, tweak: RawProject => RawProject = identity): String = {
    val raw = tweak(RawProject(
      active = true, cores = 1,
      code_cutoff = now.minusDays(1), test_cutoff = now.minusDays(3),
      ignore_tests = false, phase1_tests = Seq(), phase1_weight = 1,
      phase2_tests = Seq(), phase2_weight = 1, test_weights = Seq(),
      bad_tests = Seq(), docker_file = None, test_extensions = Seq("ok"),
      staff = Seq(), extensions = Extensions(Map(s1 -> 2), Map()),
      iterations = Iterations(2, 4)
    ))
    val rc = RawCourseNotSorted(
      active = true,
      notifications = NotificationConfig(false, None, false, false, false, None),
      projects = Map("p1" -> raw),
      staff = SortedSet()
    )
    val _ = repo(root, "courses_config", "courses.json" -> upickle.default.write(Map("c1" -> rc)))

    // s1's submission: commit_id_1 names one commit, then another
    val sub = repo(root, "c1_p1_s1", "code.c" -> "v1")
    val first = git(sub, "rev-parse", "HEAD")
    val _ = repo(root, "c1_p1_s1", "code.c" -> "v2", "commit_id_1" -> s"$first\n")
    val second = git(sub, "rev-parse", "HEAD")
    val _ = repo(root, "c1_p1_s1", "commit_id_1" -> s"$second\n")

    val t = ZonedDateTime.of(now.minusDays(4), ZoneOffset.UTC).nn
    def info(sha: String, from_file: Boolean) =
      RedactedPrepareInfo(sha, t, None, false, 0.0, from_file, false)
    val pass = (OutcomeStatus.pass, Some(1.5))
    val fail = (OutcomeStatus.fail, Some(2.0))
    val phase1 = SortedMap(
      Alias(1) -> RedactedStudentResults(Some(Alias(1)), true, info(second, true),
        SortedMap(outcome("t1", pass, pass, pass, pass), outcome("t2", pass, fail)))
    )
    val latest = SortedMap(
      Alias(1) -> RedactedStudentResults(Some(Alias(1)), true, info(second, false),
        SortedMap(outcome("t1", pass, pass), outcome("t2", fail)))
    )
    val _ = repo(root, "c1_p1__results",
      "results.json" -> upickle.default.write(latest),
      "results_phase1.json" -> upickle.default.write(phase1))
    val _ = repo(root, "c1_p1__aliases", "aliases.json" -> upickle.default.write(SortedMap(s1 -> Alias(1), s2 -> Alias(2))))
    val own = StudentResults(s1, Some(Alias(1)), true,
      PrepareInfo(t, second, false, None, CommitSource.Latest), latest(Alias(1)).outcomes)
    val _ = repo(root, "c1_p1_s1_results",
      "the_results.json" -> upickle.default.write(own),
      "t2.out" -> "expected 3, got <4>")
    second
  }

  test("the status pages drill down from courses to a run") {
    val root = os.temp.dir(deleteOnExit = true)
    val second = setup(root)
    val status = status_of(root)
    def page(path: String): String =
      status.page(path).map(_.body).getOrElse(fail(s"no page at $path"))

    assert(page("").contains("""<a href="c1/">c1</a>"""))

    // per phase: runs done, runs to go (max 4 runs; latest stops at a failure
    // while the project is live, but its second deadline has passed)
    val c = page("c1/")
    assert(c.contains("p1/phase1/") && c.contains("p1/latest/"))
    assert(c.contains("after the second deadline"))
    assert(c.contains("<td class=num>6</td><td class=num>2</td>"), "phase 1: 6 runs, 2 to go")
    assert(c.contains("83.3%"), "phase 1: 5 of 6 runs passed")
    assert(c.contains("nothing published"), "phase 2 has no results")

    val ph = page("c1/p1/phase1/")
    assert(ph.contains("""<a href="s1/">s1</a>"""))
    assert(ph.contains("commit id file"))
    assert(ph.contains("s2") && ph.contains("not in the published results"))

    val st = page("c1/p1/phase1/s1/")
    assert(st.contains(second))
    assert(st.contains("commit_id_1 history"))
    assert(st.contains("graded"), "marks the commit the file names that was graded")
    assert(st.contains("with 2 extension days"))
    assert(st.contains("""<a href="t2/">t2</a>"""))

    val t = page("c1/p1/phase1/s1/t2/")
    assert(t.contains("""<a href="1">1</a>""") && t.contains("""<a href="2">2</a>"""))
    assert(t.contains("2 to go"))

    val run = page("c1/p1/phase1/s1/t2/2")
    assert(run.contains("st-fail") && run.contains("2.000 s"))
    assert(run.contains("Only the last run"), "phase 1 keeps no output")

    // the latest results' last run shows the output kept in the student's repo
    val latest_run = page("c1/p1/latest/s1/t2/1")
    assert(latest_run.contains("expected 3, got &lt;4&gt;"))
    assert(page("c1/p1/latest/s1/").contains("the head of master"))

    assertEquals(status.page("nope/"), None)
    assertEquals(status.page("c1/p1/phase1/s1/t2/9"), None)
    assertEquals(status.page("c1/p1/phase9/"), None)
  }

  test("mirror says how mirroring the course and phase pages' repos goes") {
    val root = os.temp.dir(deleteOnExit = true)
    val _ = setup(root)
    val status = status_of(root)
    val lines = java.util.concurrent.ConcurrentLinkedQueue[String]()
    status.mirror(l => { val _ = lines.add(l) })
    val all = lines.toArray.toSeq.map(_.toString)
    assertEquals(all.head, "mirroring courses_config")
    assert(all.contains("mirroring 2 repos for 1 courses"), all)
    assert(all.exists(_.matches("""  \[\d/2\] c1_p1__results: ok \(.*s\)""")), all)
    assert(all.exists(_.matches("""  \[\d/2\] c1_p1__aliases: ok \(.*s\)""")), all)
    assert(all.last.startsWith("mirrored in "), all)
  }

  test("the tests pages: each test's commit, push, choice and who passes it") {
    val root = os.temp.dir(deleteOnExit = true)
    val _ = setup(root, _.copy(phase1_tests = Seq("002"), bad_tests = Seq("t1")))

    // s2's test, committed before the deadline (test_cutoff: 3 days ago) and
    // pushed a minute later; then changed after it
    val sub = root / "c1_p1_s2"
    os.makeDir.all(sub)
    val before = now.minusDays(5).atZone(java.time.ZoneId.systemDefault).nn.toEpochSecond
    def commit(file: String, text: String, secs: Long): String = {
      os.write.over(sub / file, text)
      val _ = os.proc("git", "add", "-A").call(cwd = sub)
      val _ = os
        .proc("git", "-c", "user.name=t", "-c", "user.email=t@x", "commit", "-q", "-m", s"$file")
        .call(cwd = sub, env = Map("GIT_COMMITTER_DATE" -> s"@$secs +0000", "GIT_AUTHOR_DATE" -> s"@$secs +0000"))
      git(sub, "rev-parse", "HEAD")
    }
    val _ = git(sub, "init", "-q", "-b", "master")
    val _ = commit("code.c", "v1", before - 3600)
    val test_sha = commit("s2.ok", "test v1", before)
    val _ = git(sub, "notes", "--ref", "push_time", "add", "-m", s"${before + 60}", test_sha)
    val _ = commit("s2.ok", "test v2", System.currentTimeMillis() / 1000)

    val _ = repo(root, "c1_p1__tests", "002.ok" -> "test", "t1.ok" -> "test")
    // phase 1 ran s2's test (alias 002) on s1 (alias 1): passed some runs
    val pass = (OutcomeStatus.pass, Some(1.0))
    val failed = (OutcomeStatus.fail, Some(1.0))
    val results = upickle.default.read[SortedMap[Alias, RedactedStudentResults]](
      os.read(root / "c1_p1__results" / "results_phase1.json"))
    val more = results.map((a, r) => a -> r.copy(outcomes = r.outcomes ++ Seq(outcome("002", pass, failed))))
    val _ = repo(root, "c1_p1__results", "results_phase1.json" -> upickle.default.write(more))

    val status = status_of(root)
    def page(path: String): String =
      status.page(path).map(_.body).getOrElse(fail(s"no page at $path"))

    assert(page("c1/").contains("p1/phase1/by-test/"))
    assert(page("c1/p1/phase1/").contains("""<a href="by-test/">by test</a>"""))

    val ts = page("c1/p1/phase1/by-test/")
    assert(ts.contains("""<a href="002/">002</a>""") && ts.contains("""<a href="t1/">t1</a>"""))
    assert(ts.contains("""<a href="../s2/">s2</a>"""), "s2 wrote test 002")
    assert(ts.contains(test_sha.take(10)), "the commit that last changed it before the deadline")
    assert(ts.contains("changed 1× later"))
    assert(ts.contains(">bad<"), "t1 is in bad_tests")
    assert(ts.contains("flaky"))
    assert(ts.contains("<span class=muted>course</span>"), "t1 is the course's")
    assert(!ts.contains("after the deadline</span>"), "pushed before the deadline")

    val t = page("c1/p1/phase1/by-test/002/")
    assert(t.contains(test_sha))
    assert(t.contains("for phase 1"))
    assert(t.contains("0 of 1 students") && t.contains("1 flaky"))
    assert(t.contains("""<a href="../../s1/002/">s1</a>"""), "links to s1's runs of it")
    assert(t.contains(fmt_secs(before + 60)), "the push time from its note")
    assertEquals(status.page("c1/p1/phase1/by-test/nope/"), None)
  }

  private def fmt_secs(secs: Long): String =
    java.time.Instant.ofEpochSecond(secs).nn.atZone(java.time.ZoneId.systemDefault).nn
      .format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")).nn

  test("a status target is reused in a new State while its repo is unchanged") {
    val root = os.temp.dir(deleteOnExit = true)
    val _ = setup(root)
    val data = StatusData(LocalRepos(root).repo)
    val history = data.commit_history("c1", "p1", s1, Phase.One)
    def look(): (Option[Seq[StatusData.CommitRow]], Long) = {
      val state = State(root / "status-workspace", read_only = true)
      val rows = state.track(history)
      (rows, os.mtime(state.saved_path(history)))
    }
    val (first, at) = look()
    assertEquals(first.map(_.size), Some(2), "commit_id_1 changed twice")
    Thread.sleep(20)
    val (again, still) = look()
    assertEquals(again, first)
    assertEquals(still, at, "not worked out again")
    // a new commit to the repo: worked out again
    val _ = repo(root, "c1_p1_s1", "commit_id_1" -> "")
    Thread.sleep(20)
    val (changed, later) = look()
    assertEquals(changed.map(_.size), Some(3))
    assert(later > at)
  }

  test("a late push is a red flag; a commit after its push is just noted") {
    val root = os.temp.dir(deleteOnExit = true)
    val _ = setup(root)
    // phase 1: pushed 2 hours after the commit; latest: 2 hours before it
    def pushed(file: String, hours: Long): Unit = {
      val f = root / "c1_p1__results" / file
      val rs = upickle.default.read[SortedMap[Alias, RedactedStudentResults]](os.read(f))
      val moved = rs.map((a, r) =>
        a -> r.copy(prepare_info = r.prepare_info.copy(
          push_time = Some(r.prepare_info.commit_time.toInstant.nn.plusSeconds(hours * 3600)),
          push_gap = true)))
      val _ = repo(root, "c1_p1__results", file -> upickle.default.write(moved))
    }
    pushed("results_phase1.json", 2)
    pushed("results.json", -2)
    val status = status_of(root)
    def page(path: String): String =
      status.page(path).map(_.body).getOrElse(fail(s"no page at $path"))

    val late = page("c1/p1/phase1/")
    assert(late.contains("""<span class="tag bad" title="pushed 2h 0m after it was committed"""), late)
    assert(page("c1/p1/phase1/s1/").contains(">pushed 2h 0m after the commit</span>"))

    val ahead = page("c1/p1/latest/")
    assert(ahead.contains(""">clock ahead</span>"""))
    assert(!ahead.contains("tag bad"))
    assert(page("c1/p1/latest/s1/").contains(">committed 2h 0m after the push</span>"))
  }

  test("a course drills down by project or by student; a project by student or by test") {
    val root = os.temp.dir(deleteOnExit = true)
    val _ = setup(root)
    val status = status_of(root)
    def page(path: String): String =
      status.page(path).map(_.body).getOrElse(fail(s"no page at $path"))

    val cs = page("")
    assert(cs.contains("""<a href="c1/">by project</a>""") && cs.contains("""<a href="c1/by-student/">by student</a>"""))

    // the course by project links each project's page; tabs switch views
    val c = page("c1/")
    assert(c.contains("""<a href="p1/">p1</a>"""))
    assert(c.contains("""<span class=on>by project</span><a href="./by-student/">by student</a>"""))

    // the course by student: everyone with an alias, latest results per project
    val cst = page("c1/by-student/")
    assert(cst.contains("""<span class=on>by student</span>"""))
    assert(cst.contains("""<a href="s1/">s1</a>""") && cst.contains("""<a href="s2/">s2</a>"""))
    assert(cst.contains("""<a href="../p1/latest/s1/">1/2</a>"""), "s1 passes t1 of t1, t2 in the latest results")

    // one student across the course: each project's phases
    val one = page("c1/by-student/s1/")
    assert(one.contains("""<a href="../../p1/phase1/s1/">phase 1</a>"""))
    assert(one.contains("alias 001"))
    assert(one.contains("not in the published results"), "phase 2 has none")

    // the project: its phases, then by student or by test
    val pr = page("c1/p1/")
    assert(pr.contains("""<span class=on>phases</span>"""))
    assert(pr.contains("""<a href="phase1/">phase 1</a>"""))

    val pst = page("c1/p1/by-student/")
    assert(pst.contains("""<a href="../../by-student/s1/">s1</a>"""))
    assert(pst.contains("""<a href="../phase1/s1/">1/2</a>"""), "phase 1: passes t1, flaky on t2")
    assert(pst.contains("""<a href="../latest/s1/">1/2</a>"""))

    val pt = page("c1/p1/by-test/")
    assert(pt.contains("<td>t1</td>") && pt.contains("<td>t2</td>"))
    assert(pt.contains("""<a href="../phase1/by-test/t2/">0/1</a>"""))
    assert(pt.contains("flaky"))

    // the phase pages have tabs too, and a crumb for the project
    val ph = page("c1/p1/phase1/")
    assert(ph.contains("""<span class=on>by student</span><a href="by-test/">by test</a>"""))
    assert(ph.contains("""<a href="../">p1</a>"""))
    assert(page("c1/p1/phase1/by-test/").contains("""<a href="../">by student</a><span class=on>by test</span>"""))
  }
}
