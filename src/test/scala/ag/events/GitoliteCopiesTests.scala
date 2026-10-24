package ag.events

import munit.FunSuite

// The test server's commands and hooks are copies of the real server's
// (gitolite-admin/local); see src/test/gitolite/README.md. Compared with the
// grader's own mirror of gitolite-admin (pulled whenever the grader runs, so
// it's what the server has), else a checkout in ~/src; skipped without either.
// Only local/ is read: gitolite-admin's keydir/ and conf/ hold student data.
class GitoliteCopiesTests extends FunSuite {
  private val real: Option[os.Path] = Seq(
    os.pwd / "workspace" / "targets" / "ag" / "grader" / "Gitolite" / "mirror" /
      "gitolite-admin" / "data" / "local",
    os.home / "src" / "gitolite-admin" / "local"
  ).find(os.isDir)
  private val copies = os.pwd / "src" / "test" / "gitolite"

  private def files(root: os.Path): Set[os.RelPath] =
    os.walk(root).filter(os.isFile).map(_.relativeTo(root)).toSet

  test("the test server runs exactly the real server's commands and hooks") {
    assume(real.isDefined, "no copy of gitolite-admin here")
    val server = real.get
    val expected = files(server)
    val ours = files(copies).filter(p =>
      p.segments.head == "commands" || p.segments.head == "hooks"
    )
    assertEquals(ours, expected, "the same files")
    for (f <- expected)
      assert(
        os.read.bytes(copies / f).sameElements(os.read.bytes(server / f)),
        s"${f.toString} differs from the server's (${server.toString}); copy it over (see src/test/gitolite/README.md)"
      )
  }
}
