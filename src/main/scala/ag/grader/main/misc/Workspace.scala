package ag.grader.main.misc

import ag.common.RunLock
import ag.grader.repos.CoursesConfig
import ag.grader.targets.{Course, Project}

// What the sub-commands share: where a workspace keeps things, its lock,
// and what the arguments pick

// the engine's workspace, inside the run's: its state (r3.git,
// checkouts, ...)
private[main] def r3_workspace(workspace: os.Path): os.Path = workspace / "r3"

private[main] def outbox_of(workspace: os.Path): ag.grader.Outbox =
  ag.grader.Outbox(
    workspace / "outbox",
    mirror_root(workspace),
    r3_workspace(workspace) / "r3.git"
  )

// what the r3 targets work from: the workspace's local mirror and the
// dropbox
private[main] def source_of(workspace: os.Path): ag.rules.Source =
  ag.rules.Source(mirror_root(workspace))

// the active courses -c picks
private[main] def active_courses(
    s: ag.rules.State,
    commonArgs: CommonArgs,
    source: ag.rules.Source
): Seq[Course] = {
  val config = CoursesConfig(source)
  config.active_courses(commonArgs.courses_).tally(s).toSeq.map(config.course)
}

// the active projects -p picks of the active courses -c picks
private[main] def active_projects(
    s: ag.rules.State,
    commonArgs: CommonArgs,
    source: ag.rules.Source
): Seq[Project] =
  for {
    course <- active_courses(s, commonArgs, source)
    p <- course.active_projects(commonArgs.projects_).tally(s).toSeq
  } yield course.project(p)

// the students' keys, as config.json's dropbox_path says
private[main] def dropbox_of(): ag.grader.targets.Dropbox =
  ag.grader.targets.Dropbox(ag.grader.Config.dropbox_root)

// The local mirror of the git server: kept up to date by `mirror`, read
// by the status pages.
private[main] def mirror_root(workspace: os.Path): os.Path =
  workspace / "mirror"

// The grading status, from the local mirror (see ag.grader.web.Status). Its
// data are r3 targets (ag.grader.web.StatusData) on the mirror, worked out by
// read-only States in the workspace's status/ directory, which these pages
// lock for as long as the program runs: a workspace of their own, so they
// can go on beside runs. They never update the mirror. None if another
// program has them.
private[main] def status_pages(
    workspace: os.Path
): Option[ag.grader.web.Status] = {
  val dir = workspace / "status"
  os.makeDir.all(dir)
  ag.common.RunLock.acquire(dir, s"status pages for $command_line") match {
    case Right(_) =>
      val states = ag.grader.web.ReadOnlyStates(dir, ttl_ms = 60 * 1000)
      Some(
        ag.grader.web.Status(
          ag.grader.web.StatusData(ag.rules.Source(mirror_root(workspace))),
          () => states()
        )
      )
    case Left(holder) =>
      println(s"no status pages: ${dir.toString} is in use by $holder")
      None
  }
}

// the command line, for telling a second run who holds the lock
private[main] var command_line = ""

// Every command works in its workspace holding the workspace's lock (two
// runs sharing a workspace would corrupt each other's state), with the
// command output in it, and the status pages served while it runs (if
// config.json has "web"). `f` gets an r3 State in the workspace's r3/,
// which --force makes rebuild what the command asks for.
private[main] def in_r3_workspace[A](
    args: CommonArgs
)(f: ag.rules.State => A): A = {
  val workspace = args.workspace
  os.makeDir.all(workspace)
  RunLock(workspace, command_line) {
    ag.rules.use_std_out_err(workspace / "std_out_err")
    val server = for {
      web <- ag.grader.Config.web
      pages <- status_pages(workspace)
      server <- ag.web.WebServer.start(workspace, web, Some(pages))
    } yield server
    try
      ag.rules.State.run(
        r3_workspace(workspace),
        args.force.value,
        args.browse.value
      )(f)
    finally server.foreach(_.stop())
  }
}
