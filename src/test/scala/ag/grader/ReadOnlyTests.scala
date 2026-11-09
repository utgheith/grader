package ag.grader

import ag.r2.{Scope, State, Target}
import ag.rules.{read_only, refuse_if_read_only, ReadOnlyException}
import munit.FunSuite

import scala.collection.SortedSet
import scala.util.Try

// A read-only State's targets can update its workspace, but whatever would
// act beyond it throws instead
class ReadOnlyTests extends FunSuite {
  private val scope = Scope("read_only")

  // counts its runs; tries to act beyond the workspace when `act`
  private var runs = 0
  private var act = true
  // (with a dependency: a target with none always runs again)
  private lazy val constant: Target[Int] = scope.target(EmptyTuple)((_, _) => 1)
  private lazy val acting: Target[Int] = scope.target((c = constant)) { (_, _) =>
    runs += 1
    if (act) refuse_if_read_only("act")
    runs
  }

  test("only a read-only State's targets are read-only") {
    assert(!read_only.get, "not outside any target")
    val ws = os.temp.dir(deleteOnExit = true)
    val flag: Target[Boolean] = scope.target(EmptyTuple)((_, _) => read_only.get)
    assert(State(ws / "a", read_only = true).track(flag))
    assert(!State(ws / "b").track(flag))
  }

  test("refusing fails the target, and nothing is saved as if it acted") {
    val ws = os.temp.dir(deleteOnExit = true)
    runs = 0
    act = true
    val e = intercept[Exception](State(ws, read_only = true).track(acting))
    assert(Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null).exists(_.isInstanceOf[ReadOnlyException]), e)
    // a run that may act runs it again: the failure wasn't kept as a value
    assertEquals(State(ws).track(acting), 2)
    // and once it's done, a read-only State reuses it
    act = false
    assertEquals(State(ws, read_only = true).track(acting), 2)
  }

  // the effects themselves refuse before doing anything
  private def refused(what: String)(f: => Any): Unit = {
    val ws = os.temp.dir(deleteOnExit = true)
    val t: Target[String] = scope.target(EmptyTuple) { (_, _) =>
      Try(f).failed.toOption.collect { case e: ReadOnlyException => e.getMessage }.getOrElse("not refused")
    }
    val message = State(ws, read_only = true).track(t)
    assert(message.contains(what), message)
  }

  test("running a test is refused") {
    val dir = os.temp.dir(deleteOnExit = true)
    refused("won't run t1") {
      doit(
        project = Project(Course("c"), "p"),
        csid = CSID("s"),
        cores = 1,
        prepared_path = dir,
        test_id = TestId("t1", "t1"),
        test_path = dir,
        test_extensions = SortedSet("ok"),
        n = 1,
        out_path = dir,
        docker_image = None
      )
    }
    assert(os.list(dir).isEmpty, "nothing ran")
  }

  test("deploying a site is refused") {
    val site = os.temp.dir(deleteOnExit = true) / "site"
    os.makeDir.all(site)
    refused("won't run wrangler") {
      CloudflareConfig(enabled = true, id = "x", api_key = "y", domain = None).deploy(site, "p")
    }
  }
}
