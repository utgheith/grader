package ag.grader

import ag.common.TempDirs
import ag.rules.State
import munit.FunSuite

import scala.util.Try

// A locked mirror (as LocalMirror leaves it) reads as before, and refuses
// writes
class MirrorLockTests extends FunSuite {
  import Fixture.c1

  test("a locked mirror reads the same, and nothing can be made in it") {
    val root = TempDirs.dir()
    val (_, source) = Fixture.setup(root)
    def read(ws: os.Path) = {
      val s = State(ws)
      (
        s.track(Course(c1, source).enrolled_csids),
        s.track(Course(c1, source).project_names),
        s.track(SubmissionMilestone(Submission(Fixture.project(source), Fixture.s1), Fixture.m1).deadline)
      )
    }
    val before = read(root / "ws1")
    LocalMirror.lock(source.root)
    try {
      assertEquals(read(root / "ws2"), before)
      val repo = source.dir("courses_config")
      for (d <- Seq(source.root, repo, repo / "refs" / "heads"))
        assert(Try(os.write(d / "stray", "x")).isFailure, d)
    } finally
      for (d <- source.root +: os.walk(source.root) if os.isDir(d, followLinks = false))
        os.perms.set(d, "rwxr-xr-x")
  }
}
