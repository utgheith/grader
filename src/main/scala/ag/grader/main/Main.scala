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
  def mirror(workspace: os.Path = os.pwd / "workspace"): Unit =
    cmd.Mirror(workspace)

  @main
  def status(workspace: os.Path = os.pwd / "workspace"): Unit =
    cmd.Status(workspace)

  @main
  def hash_password(): Unit =
    cmd.HashPassword()

  def main(args: Array[String]): Unit = {
    command_line = s"run ${args.mkString(" ")}"
    val _ = ParserForMethods(this).runOrExit(args.toIndexedSeq)
  }
}
