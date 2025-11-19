package ag.cfg

import munit.FunSuite
import upickle.default.{ReadWriter, write}
import ag.common.runWhere

case class TestConfig(name: String, value: Int) derives ReadWriter

class CfgTests extends FunSuite {
  test("load config") {

    val configContent = TestConfig("test", 42)

    // create test config
    val temp_dir = {
      // temporary directory
      val dir = os.temp.dir(
        os.temp
          .dir(dir = os.root / "tmp", prefix = "CfgTest", deleteOnExit = true)
      )

      // store the config
      os.write.over(
        dir / classOf[TestConfig].getName,
        write(configContent),
        createFolders = true
      )

      dir
    }

    configBase.runWhere(temp_dir) {
      val loaded = ag.cfg.load[TestConfig]
      assertEquals(loaded, TestConfig("test", 42))
    }

  }
}
