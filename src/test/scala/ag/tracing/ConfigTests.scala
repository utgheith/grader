package ag.tracing

import munit.FunSuite
import scala.util.Try

class ConfigTests extends FunSuite {

  def withFile[A](f: os.Path => A): A = {
    val file = os.temp(prefix = "Trace", deleteOnExit = true)
    try {
      f(file)
    } finally {
      val _ = Try {
        os.remove.all(file)
      }
    }
  }

  def withConfig[A](file: os.Path, threadName: Boolean, timestamp: Boolean)(
      f: => A
  ): A =
    tracingConfig.set(
      Config(Some(file.relativeTo(os.pwd)), threadName, timestamp)
    ) {
      f
    }

  test("trace to file") {
    withFile { file =>
      withConfig(file, false, false) {
        trace("hello")
      }
      assert(clue(os.read(file)) == "hello\n")
    }
  }

  test("include thread name") {
    withFile { file =>
      val threadName = Thread.currentThread.getName
      withConfig(file, true, false) {
        trace("hello")
      }
      assert(clue(os.read(file)) == s"[$threadName] hello\n")
    }
  }
}
