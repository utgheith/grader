package ag.common

import munit.FunSuite
import scala.collection.mutable.ListBuffer

class LoggingTests extends FunSuite {
  test("logging") {
    val logs = ListBuffer[Any]()
    given logger: Logging = logs += _
    log("test message")
    log(5)
    assert(clue(logs.toList) == List[String | Int]("test message", 5))
  }
}
