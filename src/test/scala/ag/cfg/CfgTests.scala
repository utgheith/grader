package ag.cfg

import munit.FunSuite
import upickle.default.{ReadWriter, macroRW}

case class TestConfig(foo: String, bar: Int)
object TestConfig {
  implicit val rw: ReadWriter[TestConfig] = macroRW
}

class CfgTests extends FunSuite {
  test("load config from scoped directory") {
    val tmpDir = os.temp.dir(prefix = "CfgTests")
    val configContent = """{"foo": "baz", "bar": 42}"""

    // The config loader uses the class name as the file name
    val configPath = tmpDir / "ag.cfg.TestConfig"
    os.write(configPath, configContent)

    ag.cfg.baseDir.set(tmpDir) {
      val loaded = ag.cfg.load[TestConfig]
      assertEquals(loaded, TestConfig("baz", 42))
    }
  }
}
