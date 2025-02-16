package ag.grader

import upickle.default.{read, write, writeJs}

class AliasTests extends munit.FunSuite {
  test("simple") {
    val a = Alias(666)
    assert(clue(a.toString) == "666")
    assert(clue(Alias(a.toString)) == clue(a))
    
    assert(clue(Alias(42)) < clue(Alias(666)))
    assert(clue(Alias(10)) > clue(Alias(4)))
    assert(clue(Alias(12)) >= clue(Alias(0)))
    
    val js = writeJs(a)
    assert(clue(js.num) == clue(666))
    assert(clue(write(js)) == clue("666"))
    assert(clue(read[Alias](js)) == clue(a))
    
    for (i <- Seq(-1, 1000, 1001)) {
      try {
        val _ = Alias(i).toString
        assert(false)
      } catch {
        case e: IllegalArgumentException => assert(clue(e.getMessage) == clue(i.toString))
      }
    }
  }
}
