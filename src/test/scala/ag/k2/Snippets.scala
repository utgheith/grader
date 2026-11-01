package ag.k2

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter

// Compiles code the way ag.k2 is compiled, against the test classpath, for
// tests about what doesn't compile. (munit's compileErrors stops after the
// typer, before capture checking.)
object Snippets {
  // written by the build (see build.sbt)
  private lazy val classpath: String =
    String(
      getClass
        .getResourceAsStream("/snippet-classpath.txt")
        .nn
        .readAllBytes()
        .nn,
      java.nio.charset.StandardCharsets.UTF_8
    )

  // the compiler's errors for `code`, one file with capture checking and
  // safer exceptions on: empty if it compiles
  def errors(code: String): Seq[String] = {
    val dir = os.temp.dir()
    val src = dir / "Snippet.scala"
    os.write(
      src,
      "import language.experimental.captureChecking\n" +
        "import language.experimental.saferExceptions\n" +
        "import ag.k2.*\n" + code
    )
    val reporter = StoreReporter()
    val _ = Driver().process(
      Array(
        "-classpath",
        classpath,
        "-d",
        dir.toString,
        "-experimental",
        "-Yexplicit-nulls",
        src.toString
      ),
      reporter
    )
    reporter.allErrors.map(_.message)
  }
}
