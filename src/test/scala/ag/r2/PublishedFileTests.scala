package ag.r2

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try

// The pattern Project uses for results.json: a file written outside the
// workspace, a check of whether it's readable, and a writer that depends on
// that check.
object Published extends Scope("published") {
  lazy val file: os.Path = os.temp(deleteOnExit = true)
  val writes = new AtomicInteger(0)

  // no dependencies: re-checked on every run
  val readable: Target[Boolean] = target() {
    Try(upickle.default.read[Map[String, Int]](os.read(file))).isSuccess
  }

  val writer: Target[Int] = target(readable) { _ =>
    os.write.over(file, upickle.default.write(Map("score" -> 90)))
    writes.incrementAndGet()
  }
}

class PublishedFileTests extends munit.FunSuite {
  test(
    "a published file that stops being readable is rewritten, then left alone"
  ) {
    def run_once(): Unit = State.run(config.get_test_dir) {
      val _ = Published.writer.track.block
    }
    def writes = Published.writes.get

    run_once() // first write (the file didn't parse yet)
    run_once() // the file now parses: the writer runs once more, harmlessly
    val settled = writes
    run_once()
    assertEquals(writes, settled, "settled: nothing to do")

    os.write.over(Published.file, """{"score": "ninety"}""") // an old format
    run_once()
    assertEquals(writes, settled + 1, "rewritten")
    assert(
      Try(
        upickle.default.read[Map[String, Int]](os.read(Published.file))
      ).isSuccess
    )

    run_once()
    run_once()
    assertEquals(writes, settled + 2, "settled again after one more pass")
  }
}
