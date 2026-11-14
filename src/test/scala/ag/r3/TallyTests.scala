package ag.r3

import ag.common.TempDirs
import munit.FunSuite

import scala.collection.SortedSet

// Target.tally: the run's tally counts what's measured, by target kind
class TallyTests extends FunSuite {
  private def ws(): os.Path = TempDirs.dir()

  test(
    "a measured target's value is counted under its kind, summed across calls"
  ) {
    val s = Scope("f")
    val names = s.target(EmptyTuple)((_, _) => SortedSet("a", "bb", "ccc"))
    val chars = s.fun((n: String) =>
      s.target(EmptyTuple)((_, _) => n.toSeq.map(_.toString))
    )
    val found = s.fun((n: String) =>
      s.target(EmptyTuple)((_, _) => Option.when(n.length > 1)(n))
    )
    val plain = s.target(EmptyTuple)((_, _) => 42)
    val st = State(ws())
    val got = for {
      n <- names.tally(st).toSeq
      c <- chars(n).tally(st)
      f <- found(n + c).tally(st).toSeq
    } yield f
    assertEquals(got.size, 6)
    assertEquals(plain.tally(st), 42) // no Measure[Int]: tracked, not counted
    assertEquals(
      st.tally.lines("x", got.size),
      Seq(
        "x: 6 found",
        "  TallyTests.names       3",
        "  TallyTests.chars       6",
        "  TallyTests.found       6"
      )
    )
  }
}
