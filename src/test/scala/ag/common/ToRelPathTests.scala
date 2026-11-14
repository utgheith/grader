package ag.common

import munit.FunSuite

class ToRelPathTests extends FunSuite {
  test("ToRelPath[Boolean]") {
    assert(clue(ToRelPath(true)) ==  clue(os.RelPath("yes")))
    assert(clue(ToRelPath(false)) ==  clue(os.RelPath("no")))
  }

  test("ToRelPath[sourcecode.FullName]") {
    val fn = sourcecode.FullName("a.b.c")
    assert(clue(ToRelPath(fn)) ==  clue(os.RelPath("a/b/c")))
  }

  test("ToRelPath[os.RelPath]") {
    val p = os.RelPath("a/b")
    assert(clue(ToRelPath(p)) ==  clue(p))
  }

  test("ToRelPath[Regex]: one component, whatever the pattern") {
    assert(clue(ToRelPath("abc".r)) ==  clue(os.RelPath("re=abc")))
    val patterns = Seq("", ".", "..", ".*", "a/b", "a%2Fb", "/x", "a|b")
    val paths = patterns.map(p => ToRelPath(p.r))
    for (p <- paths) assertEquals(clue(p).segments.size, 1)
    assertEquals(paths.distinct.size, patterns.size)
  }

  test("ToRelPath[Option[T]]") {
    assert(clue(ToRelPath(Option("a"))) ==  clue(os.RelPath("a")))
    assert(clue(ToRelPath(Option.empty[String])) ==  clue(os.RelPath(".")))
  }

  test("ToRelPath[Seq[T]]") {
    assert(clue(ToRelPath(Seq("a", "b"))) ==  clue(os.RelPath("a/b")))
    assert(clue(ToRelPath(Seq.empty[String])) ==  clue(os.RelPath(".")))
  }

  test("ToRelPath[String]") {
    assert(clue(ToRelPath("a/b")) ==  clue(os.RelPath("a/b")))
  }

  test("ToRelPath[Numeric]") {
    assert(clue(ToRelPath(123)) ==  clue(os.RelPath("123")))
    assert(clue(ToRelPath(12.34)) ==  clue(os.RelPath("12.34")))
  }

  test("ToRelPath[(A, B)]") {
    assert(clue(ToRelPath(("a", 1))) ==  clue(os.RelPath("a/1")))
  }

  test("derived: a component per field, never running into each other; Omitted fields left out") {
    final class Env
    given ToRelPath[Env] = ToRelPath.Omitted()
    final case class K(a: String, b: Option[String], n: Int, env: Env) derives ToRelPath
    val env = Env()
    assertEquals(ToRelPath(K("x", Some("y"), 3, env)), os.RelPath("x/y/3"))
    val keys = Seq(K("x", None, 1, env), K("", Some("x"), 1, env), K("a/b", None, 1, env), K("a%2Fb", None, 1, env), K("%", None, 1, env))
    val paths = keys.map(ToRelPath(_))
    for (p <- paths) assertEquals(clue(p).segments.size, 3)
    assertEquals(paths.distinct.size, keys.size)
    intercept[IllegalArgumentException](ToRelPath(K("..", None, 1, env)))
  }
}
