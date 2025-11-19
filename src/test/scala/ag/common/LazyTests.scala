package ag.common

import munit.FunSuite

class LazyTests extends FunSuite {
  test("lazy computation") {
    var count = 0
    val l = Lazy {
      count += 1
      count
    }

    assert(clue(count) == 0)
    assert(clue(l.get()) == 1)
    assert(clue(count) == 1)
    assert(clue(l.get()) == 1)
    assert(clue(count) == 1)
  }

  test("lazy map") {
    var count = 0
    val l = Lazy {
      count += 1
      1
    }
    val l2 = l.map(_ + 1)

    assert(clue(count) == 0)
    assert(clue(l2.get()) == 2)
    assert(clue(count) == 1)
    assert(clue(l2.get()) == 2)
    assert(clue(count) == 1)
  }

  test("lazy flatMap") {
    var count = 0
    val l = Lazy {
      count += 1
      1
    }
    val l2 = l.flatMap(x => Lazy(x + 1))

    assert(clue(count) == 0)
    assert(clue(l2.get()) == 2)
    assert(clue(count) == 1)
    assert(clue(l2.get()) == 2)
    assert(clue(count) == 1)
  }
}
