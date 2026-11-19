package ag.grader.main

import ag.grader.main.misc.{CommonArgs, AliasSortMode, command_line, given}

import ag.grader.Milestone
import mainargs.{ParserForMethods, arg, main}

// The sub-commands: each one's options (they make its --help), its work
// in cmd.<Name>
object Main {
  @main
  def dropbox(commonArgs: CommonArgs): Unit =
    cmd.Dropbox(commonArgs)

  @main
  def bad_tests(commonArgs: CommonArgs): Unit =
    cmd.BadTests(commonArgs)

  @main
  def publish_keys(commonArgs: CommonArgs): Unit =
    cmd.PublishKeys(commonArgs)

  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit =
    cmd.PublishEnrollment(commonArgs)

  @main
  def enrollment(commonArgs: CommonArgs): Unit =
    cmd.Enrollment(commonArgs)

  @main
  def create_grades_repos(commonArgs: CommonArgs): Unit =
    cmd.CreateGradesRepos(commonArgs)

  @main
  def courses(commonArgs: CommonArgs): Unit =
    cmd.Courses(commonArgs)

  @main
  def history(commonArgs: CommonArgs): Unit =
    cmd.History(commonArgs)

  @main
  def projects(commonArgs: CommonArgs): Unit =
    cmd.Projects(commonArgs)

  @main
  def students(commonArgs: CommonArgs): Unit =
    cmd.Students(commonArgs)

  @main
  def aliases(
      commonArgs: CommonArgs,
      @arg(
        name = "sort",
        doc = "How to sort the aliases; 'alias' or 'csid'.  Defaults to 'csid'."
      )
      sortMode: AliasSortMode = AliasSortMode.CSID
  ): Unit =
    cmd.Aliases(commonArgs, sortMode)

  @main
  def milestone_tests(
      commonArgs: CommonArgs,
      @arg(doc =
        "a milestone or live (default: the current one, see ProjectState.current)"
      )
      milestone: Option[Milestone] = None
  ): Unit =
    cmd.MilestoneTests(commonArgs, milestone)

  @main
  def publish_work_repos(commonArgs: CommonArgs): Unit =
    cmd.PublishWorkRepos(commonArgs)

  @main
  def outbox(commonArgs: CommonArgs): Unit =
    cmd.Outbox(commonArgs)

  @main
  def run(
      commonArgs: CommonArgs,
      @arg(doc =
        "the most tries a test gets (default: as the project's iterations say)"
      )
      tries: Option[Int] = None,
      @arg(doc =
        "run just this milestone: a milestone or live (default: every milestone the project publishes)"
      )
      milestone: Option[Milestone] = None
  ): Unit =
    cmd.Run(commonArgs, tries, milestone)

  @main
  def grade(
      commonArgs: CommonArgs,
      @arg(doc =
        "the most tries a test gets (default: as the project's iterations say)"
      )
      tries: Option[Int] = None
  ): Unit =
    cmd.Grade(commonArgs, tries)

  @main
  def scores(
      commonArgs: CommonArgs,
      @arg(short = 'n', doc = "how many tries")
      tries: Int = 1,
      @arg(doc =
        "a milestone or live (default: the current one, see ProjectState.current)"
      )
      milestone: Option[Milestone] = None
  ): Unit =
    cmd.Scores(commonArgs, tries, milestone)

  @main
  def sync(workspace: os.Path = os.pwd / "workspace"): Unit =
    cmd.Sync(workspace)

  @main
  def push_to_cloudflare(commonArgs: CommonArgs): Unit =
    cmd.PushToCloudflare(commonArgs)

  @main
  def status(workspace: os.Path = os.pwd / "workspace"): Unit =
    cmd.Status(workspace)

  @main
  def hash_password(): Unit =
    cmd.HashPassword()

  // A command that fails ends the process (exit 1) with its full stack
  // trace, causes included, as the last thing it prints, set off so it's
  // seen at once (mainargs would print it among its own messages). The
  // loop around it (loop.sh) starts the next round.
  def main(args: Array[String]): Unit = {
    command_line = s"run ${args.mkString(" ")}"
    val parser = ParserForMethods(this)
    if (args.headOption.contains("--help")) {
      val _ = parser.runOrExit(args.toIndexedSeq)
    } else
      parser.runRaw0(args.toIndexedSeq) match {
        case Right((_, mainargs.Result.Success(_)))           => ()
        case Right((_, mainargs.Result.Failure.Exception(e))) =>
          System.out.flush()
          val line = "=" * 72
          System.err.println(
            s"\n$line\nFAILED: $command_line\n$line\n${stack_trace(e)}$line"
          )
          System.exit(1)
        case Right((m, f: mainargs.Result.Failure)) =>
          System.err.println(
            mainargs.Renderer.renderResult(
              m,
              f,
              totalWidth = 100,
              printHelpOnError = true,
              docsOnNewLine = false,
              customName = None,
              customDoc = None,
              sorted = true,
              nameMapper = mainargs.Util.kebabCaseNameMapper
            )
          )
          System.exit(1)
        case Left(early) =>
          System.err.println(mainargs.Renderer.renderEarlyError(early))
          System.exit(1)
      }
  }

  private def stack_trace(e: Throwable): String = {
    val w = java.io.StringWriter()
    e.printStackTrace(java.io.PrintWriter(w, true))
    w.toString
  }
}
