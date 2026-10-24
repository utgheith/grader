package ag.common

import munit.FunSuite

class NoNullsTests extends FunSuite {
  case class Inner(name: String, n: Int)
  case class Outer(inner: Inner, extra: Option[String], by: Map[String, Inner])

  // a null where the type doesn't allow one -- how upickle produces them
  private def sneaky[A]: A = null.asInstanceOf[A]

  private val clean = Outer(Inner("a", 1), None, Map("k" -> Inner("b", 2)))

  test("clean values pass, including None") {
    assertEquals(NoNulls.find(clean), None)
    assertEquals(NoNulls.find(Left[String, Int]("x")), None)
  }

  test("a null is found wherever it is, with where") {
    assertEquals(NoNulls.find(null), Some("$"))
    assertEquals(
      NoNulls.find(clean.copy(inner = Inner(sneaky, 1))),
      Some("$.inner.name")
    )
    assertEquals(
      NoNulls.find(clean.copy(by = Map("k" -> sneaky[Inner]))),
      Some("$.by[k]")
    )
    assertEquals(NoNulls.find(Right[String, Inner](sneaky)), Some("$.right"))
    assertEquals(NoNulls.find(Seq(Inner("a", 1), sneaky[Inner])), Some("$[1]"))
  }
}
