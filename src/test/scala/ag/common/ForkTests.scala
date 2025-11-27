package ag.common

import munit.FunSuite
import language.experimental.saferExceptions

case class MyEx(msg: String) extends Exception(msg)

class ForkTests extends FunSuite {

  test("Fork.success returns the value") {
    val f = Fork.success(42)
    assertEquals(f.result, Right(42))
    val res =
      try {
        f.join
      } catch {
        case _: Exception => fail("Should not throw")
      }
    assertEquals(res, 42)
  }

  test("Fork.failure returns the exception") {
    val ex = MyEx("boom")
    val f = Fork.failure(ex)
    assertEquals(f.result, Left(ex))

    try {
      val _ = f.join
      fail("Should have thrown")
    } catch {
      case e: MyEx      => assertEquals(e, ex)
      case _: Throwable => fail("Wrong exception type")
    }
  }

  test("Fork.apply returns success asynchronously") {
    val f = Fork {
      Thread.sleep(10)
      100
    }
    assertEquals(f.result, Right(100))
    assertEquals(f.join(using CanThrow[Exception]), 100)
  }

  test("Fork.apply captures exception") {
    val ex = MyEx("async boom")

    val f = Fork[MyEx, Int] {
      Thread.sleep(10)
      throw ex
    }

    assertEquals(f.result, Left(ex))
    try {
      val res = f.join
      assertEquals(res, 100)
      fail("Should have thrown")
    } catch {
      case e: MyEx => assertEquals(e, ex)
    }
  }
}
