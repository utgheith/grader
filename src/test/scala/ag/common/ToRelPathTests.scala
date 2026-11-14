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
}
