package ag.common

import munit.FunSuite

class TallyTests extends FunSuite {

  test("counts each stage across nested loops, marking the first empty one") {
    val f = Tally()
    val out = for {
      a <- f("outer", Seq(1, 2, 3))
      if f.keep("odd", a % 2 == 1)
      b <- f("inner", Seq.fill(a)(a))
      if f.keep("big", b > 5)
      c <- f("never reached", Seq(b))
    } yield c
    assertEquals(out, Seq())
    assertEquals(
      f.lines("x", 0),
      Seq(
        "x: none found",
        "  outer       3",
        "  odd         2",
        "  inner       4",
        "  big         0   <- nothing left here"
      )
    )
  }

  test("shown when nothing is found, or when asked for") {
    def shown(found: Int, verbose: Boolean): Seq[String] = {
      val f = Tally()
      val _ = f("s", Seq(1))
      val said = scala.collection.mutable.Buffer[String]()
      f.report("x", found, verbose, said += _)
      said.toSeq
    }
    assertEquals(shown(1, false), Seq())
    assertEquals(shown(1, true), Seq("x: 1 found", "  s       1"))
    assertEquals(shown(0, false), Seq("x: none found", "  s       1"))
  }
}
