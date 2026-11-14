package ag.r3

import ag.common.TempDirs
import munit.FunSuite

// State.run's browse: what a run used, linked by path
class BrowseTests extends FunSuite {
  test("each target used is linked by its path to its value and data; a failed run links too") {
    val dir = TempDirs.dir()
    val s = Scope("browse")
    val in = s.target(EmptyTuple)((_, _) => 7)
    val out = s.target((i = in)) { (producer, deps) =>
      create_data(producer) { d => os.write(d / "f.txt", deps.i.toString); deps.i * 2 }
    }
    val _ = State.run(dir, browse = true)(_.track(out))
    val links = dir / "browse"
    val o = links / Store.rel_path(out.path)
    assert(os.isLink(o))
    assertEquals(os.read(o / "value.json").contains("14"), true)
    assertEquals(os.read(o / "data" / "f.txt"), "7")
    assertEquals(os.read(links / Store.rel_path(in.path) / "value.json"), "7")
    // a run that fails still links what it got to
    val _ = intercept[Exception](State.run(dir, browse = true) { st =>
      val _ = st.track(in)
      throw Exception("boom")
    })
    assert(os.isLink(links / Store.rel_path(in.path)))
    assert(!os.exists(o), "only what that run used")
  }
}
