package ag.r2

import java.util.concurrent.atomic.AtomicInteger

object NullThings extends Scope("nulls") {
  val runs = new AtomicInteger(0)
  val input: Target[Int] = target(EmptyTuple) {(_, _) =>  1 }
  val either: Target[Either[String, Int]] = target((i = input)) { (_, deps) =>
    val _ = runs.incrementAndGet()
    Right(deps.i)
  }
}

class NullStateTests extends munit.FunSuite {
  test("a saved value that reads back as null is recomputed, not returned") {
    def run_once(): Either[String, Int] =
      State.run(config.get_test_dir) { root =>
        root.track(NullThings.either).block
      }

    assertEquals(run_once(), Right(1))
    assertEquals(NullThings.runs.get, 1)

    // what an older version saved for a None, where the type is now an Either
    val saved = State(config.get_test_dir).saved_path(NullThings.either)
    val json = ujson.read(os.read(saved))
    json("result")("value") = ujson.Null
    os.write.over(saved, ujson.write(json))

    assertEquals(run_once(), Right(1))
    assertEquals(NullThings.runs.get, 2, "recomputed")
  }
}
