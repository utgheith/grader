package ag.r2

import ag.rules.check

import munit.FunSuite

object GraphThings extends Scope("graph") {
  // no dependencies: runs every time, with a command, but its value is fixed
  val leaf: Target[Int] = target(EmptyTuple) { (_, _) =>
    os.proc("true").check()
    1
  }
  val other: Target[Int] = target(EmptyTuple) { (_, _) => 2 }
  val mid: Target[Int] = target((l = leaf)) { (_, deps) =>
    os.proc("echo", "mid").check()
    deps.l + 1
  }
  val top: Target[Int] =
    target((m = mid, o = other.peek)) { (_, deps) => deps.m + deps.o }
  val broken: Target[Int] =
    target((l = leaf)) { (_, _) => throw Exception("broken on purpose") }
  val above_broken: Target[Int] =
    target((b = broken)) { (_, deps) => deps.b }
}

class RunGraphTests extends FunSuite {
  import GraphThings.*

  // the graph that the run left in `workspace`, by target path
  private def graph(workspace: os.Path): Map[String, ujson.Value] = {
    val js = os.read(workspace / "graph" / "data.js")
    val json = ujson.read(js.stripPrefix("const GRAPH = ").trim.stripSuffix(";"))
    json("nodes").arr.map(n => n("path").str -> n).toMap
  }

  test("each run leaves a graph of what it did") {
    val workspace = os.temp.dir(deleteOnExit = true)
    def run(): Map[String, ujson.Value] = {
      val _ = State.run(workspace) { root =>
        val _ = root.track(top)
        intercept[Exception](root.track(above_broken))
      }
      graph(workspace)
    }
    val first = run()
    // the viewer, with its style and script filled in
    val page = os.read(workspace / "graph" / "index.html")
    assert(page.contains("draw_graph") && !page.contains("/*JS*/"))
    val (l, m, t) = (leaf.path.toString, mid.path.toString, top.path.toString)
    for (p <- Seq(l, m, t)) assertEquals(first(p)("outcome").str, "ran", p)
    assertEquals(first(l)("reason").str, "it has no saved value")
    assertEquals(first(l)("kind").str, "ag/r2/GraphThings/leaf")

    // the run itself depends on what it tracked, top on mid and a peek at other
    val deps = (p: String) => first(p)("deps").arr.map(d => d(0).str -> d(1).bool).toMap
    assertEquals(deps(RunGraph.root).keySet, Set(t, above_broken.path.toString))
    assertEquals(deps(t), Map(m -> false, other.path.toString -> true))

    // commands belong to the target whose function ran them
    val commands = (p: String) => first(p)("commands").arr.map(_("command").arr.map(_.str).toSeq).toSeq
    assertEquals(commands(l), Seq(Seq("true")))
    assertEquals(commands(m), Seq(Seq("echo", "mid")))
    assertEquals(commands(t), Seq())
    assert(first(m)("commands")(0)("out").strOpt.isDefined, "links to stdout")

    // failures, and what failed because of them
    assertEquals(first(broken.path.toString)("outcome").str, "failed")
    assert(first(broken.path.toString)("error").str.contains("broken on purpose"))
    assertEquals(first(above_broken.path.toString)("outcome").str, "failed")

    // the second time, leaf runs again (it has no dependencies) with the same
    // value, so what uses it is reused
    val second = run()
    assertEquals(second(l)("outcome").str, "ran")
    assertEquals(second(l)("reason").str, "it has no dependencies")
    assertEquals(second(m)("outcome").str, "reused")
    assertEquals(second(t)("outcome").str, "reused")
    // it saved nothing, so it can only start over
    assertEquals(second(broken.path.toString)("reason").str, "it has no saved value")
  }

  test("the page loads on a thread whose context loader can't see it") {
    // like the web server's threads under `sbt run`
    @volatile var page: Either[Throwable, String] = Left(Exception("not run"))
    val t = Thread.ofPlatform().unstarted { () =>
      page = scala.util.Try(RunGraph.load_page()).toEither
    }
    t.setContextClassLoader(ClassLoader.getPlatformClassLoader)
    t.start()
    t.join()
    assert(page.exists(_.contains("draw_graph")), page.toString)
  }

  test("a target that reruns says which dependencies changed") {
    val workspace = os.temp.dir(deleteOnExit = true)
    val s = Scope("graph_changed")
    var value = 1
    val input: Target[Int] = s.target(EmptyTuple)((_, _) => value)
    val fixed: Target[Int] = s.target(EmptyTuple)((_, _) => 0)
    val user: Target[Int] =
      s.target((i = input, f = fixed))((_, deps) => deps.i + deps.f)
    val _ = State.run(workspace)(root => root.track(user))
    value = 2
    val _ = State.run(workspace)(root => root.track(user))
    val g = graph(workspace)
    assertEquals(g(user.path.toString)("reason").str, "dependencies changed")
    assertEquals(
      g(user.path.toString)("changed").arr.map(_.str).toSeq,
      Seq(input.path.toString)
    )
  }
}
