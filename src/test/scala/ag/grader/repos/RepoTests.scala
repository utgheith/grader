package ag.grader.repos

import ag.common.{TempDirs, ToRelPath}
import ag.grader.{Create, Fixture, Notice}
import ag.r3.{State, Target, WithData}
import ag.grader.Publish
import munit.FunSuite

// The publishing policies, on made-up repos (see Fixture): each writes the
// files it's given into repo `k`
class RepoTests extends FunSuite {
  import Fixture.*

  private def env(root: os.Path): (Repos, Env) = {
    val (r, subs) = Fixture.setup(root)
    (r, Env(subs.defs.source))
  }

  private def put(files: Seq[(String, String)], dir: os.Path): Seq[Notice] = {
    for ((f, text) <- files) os.write.over(dir / f, text)
    Seq()
  }

  final case class Owned(k: String, files: Seq[(String, String)], env: Env)
      extends OwnedRepo[Owned] derives ToRelPath {
    def repo_name = k
    def create = Some(Create("empty", Seq(), Seq()))
    val message = "test"
    lazy val latest = owned(_ => files)((in, _, dir) => put(in, dir))
  }
  final case class Kept(k: String, files: Seq[(String, String)], env: Env)
      extends OwnedRepo[Kept] derives ToRelPath {
    def repo_name = k
    def create = Some(Create("empty", Seq(), Seq()))
    val message = "test"
    override protected def replaces_all = false
    lazy val latest = owned(_ => files)((in, _, dir) => put(in, dir))
  }
  final case class Added(k: String, files: Seq[(String, String)], drop: Option[String], env: Env)
      extends AdditiveRepo[Added] derives ToRelPath {
    def repo_name = k
    def create = Some(Create("empty", Seq(), Seq()))
    val message = "test"
    lazy val latest = additive(_ => files) { (in, _, dir) =>
      for (f <- drop) { val _ = os.remove(dir / f) }
      put(in, dir)
    }
  }
  final case class HandedOver(k: String, files: Seq[(String, String)], env: Env)
      extends HandedOverRepo[HandedOver] derives ToRelPath {
    def repo_name = k
    def create = Some(Create("empty", Seq(), Seq()))
    val message = "test"
    lazy val latest = handed_over(_ => files)((in, _, dir) => put(in, dir))
  }

  private def files(s: State, t: Target[WithData[Publish]]): Seq[String] =
    PublishedRepo.files(s.checkout(s.track(t).data)).map(_.toString).sorted

  test("owned: replaces what's there (but dot files), or adds to it if it keeps the rest") {
    val root = TempDirs.dir()
    val (r, e) = env(root)
    val _ = r.commit("x", test_cutoff, ".gitignore" -> "", "old" -> "o")()
    val s = State(root / "ws")
    assertEquals(files(s, Owned("x", Seq("new" -> "n"), e).latest), Seq("new"))
    assertEquals(files(s, Kept("x", Seq("new" -> "n"), e).latest), Seq("new", "old"))
  }

  test("additive: adds and changes, fails if a file would go") {
    val root = TempDirs.dir()
    val (r, e) = env(root)
    val _ = r.commit("x", test_cutoff, "a" -> "1")()
    val s = State(root / "ws")
    assertEquals(files(s, Added("x", Seq("a" -> "2", "b" -> "3"), None, e).latest), Seq("a", "b"))
    val err = intercept[Throwable](s.track(Added("x", Seq(), Some("a"), e).latest))
    assert(clue(err.getMessage).contains("x is only added to, but a would go"))
  }

  test("handed over: made with its initial files, then left as it is") {
    val root = TempDirs.dir()
    val (r, e) = env(root)
    val s = State(root / "ws")
    val made = s.track(HandedOver("x", Seq("a" -> "1"), e).latest)
    assertEquals((made.value.exists, made.value.create.isDefined), (false, true))
    assertEquals(PublishedRepo.files(s.checkout(made.data)).map(_.toString), Seq("a"))
    val _ = r.commit("x", test_cutoff, "theirs" -> "t")()
    assertEquals(files(State(root / "ws2"), HandedOver("x", Seq("a" -> "1"), e).latest), Seq("theirs"))
  }

  test("a target is named by where it's defined, then every field of its instance (but env), then fun's arguments") {
    val root = TempDirs.dir()
    val (_, e) = env(root)
    val t = StudentResultsRepo(c1, p1, s1, e).latest(3)
    assertEquals(t.path, os.RelPath("ag/grader/repos/StudentResultsRepo/latest/c1/p1/s1/3"))
    assertEquals(StudentResultsRepo(c1, p1, s1, e).repo_name, "c1_p1_s1_results")
    assertEquals(ProjectConfig(c1, p1, e).deadline(s1, ag.grader.Phase.One).path,
      os.RelPath("ag/grader/repos/ProjectConfig/deadline/c1/p1/s1/phase1"))
  }
}
