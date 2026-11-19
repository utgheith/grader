package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  publishing,
  in_r3_workspace,
  run_projects
}

import ag.grader.gitolite.Publish
import ag.grader.targets.Project
import ag.rules.{Target, WithData}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try

// One round: runs the picked projects (as run does: with no -p, one round
// of one project) and records what they publish in the outbox: what the
// project publishes (override, aliases, submitted tests, tests), each
// student's own results and prepared code, and its course's site with
// everyone's results as far as they've run (see repos.Grade). If
// config.json's can_push_repo allows it, each is pushed as soon as it's
// recorded (see main.misc.Publishing); else it waits in the outbox for `sync`.
//
// Each is recorded as soon as it can be: what the project publishes as it
// starts running, a student's own as soon as their runs are done, the site
// when all are. Then what the round didn't run is recorded too, as far as
// it has run (no test runs again: see repos.Grade.so_far), with runs not
// saved here taken from the published results (see TestRun.runs). Each is
// recorded once: the mirror isn't updated after a push (its base would be
// out of date), and the next round, which fetches it, records it again.
//
// With -s or -t, only the students and tests they pick are run (as run's),
// and only the picked students' own repos are recorded, after the runs.
//
// Then each course's site is served as it's published (see
// cloudflare.SiteDeployment), if config.json's cloudflare is enabled.
//
// A failed push ends the round (before the sites are served): what's left stays in the outbox, test runs
// not yet started don't start, and the round fails with the push's error.
object Grade {
  def apply(commonArgs: CommonArgs, tries: Option[Int]): Unit =
    in_r3_workspace(commonArgs) { s =>
      val picking =
        commonArgs.students_.regex != ".*" || commonArgs.tests_.regex != ".*"
      val projects = publishing(commonArgs.workspace) { pub =>
        val to_push = AtomicInteger()
        def record(t: Target[WithData[Publish]]): Unit =
          if (pub.record(s.track(t))) {
            val _ = to_push.incrementAndGet()
          }

        // what's recorded while the runs go on, each on a thread of its own
        val threads = ConcurrentLinkedQueue[Thread]()
        val failures = ConcurrentLinkedQueue[Throwable]()
        def beside(f: => Unit): Unit = {
          val _ = threads.add(
            Thread
              .ofVirtual()
              .nn
              .start { () =>
                try f
                catch { case e: Throwable => val _ = failures.add(e) }
              }
          )
        }
        def join(): Unit = threads.forEach(_.join())

        def students_of(project: Project) =
          s.track(project.students_with_submission).toSeq

        val outcome = Try {
          val projects = run_projects(
            s,
            commonArgs,
            tries,
            picking,
            None,
            // (not when picking: then it's recorded after the runs)
            (project, milestones, n) => {
              beside(ag.grader.repos.Grade.project(project).foreach(record))
              for (csid <- students_of(project))
                beside {
                  val submission = project.submission(csid)
                  val _ = s.track_all(
                    milestones.map(m => submission.milestone(m).results(n))
                  )
                  ag.grader.repos.Grade
                    .own(project, Seq(csid), 0)
                    .foreach(record)
                }
            }
          )
          join()
          // the rest, as far as everyone has run, in order
          for (project <- projects) {
            val students = students_of(project)
            ag.grader.repos.Grade
              .so_far(
                project,
                // as run_projects picked them (its report counts them)
                if (picking)
                  students
                    .filter(csid => commonArgs.students_.matches(csid.value))
                else students
              )
              .foreach(record)
          }
          projects
        }
        join()
        for (projects <- outcome)
          println(
            s"${projects.size} projects, ${pub.count} repos, ${to_push.get} to push"
          )
        Option(failures.peek()).foreach(e => throw e)
        outcome.get
      }
      // then the courses' sites, as published, if config.json's cloudflare
      // is enabled
      val host = ag.grader.Config.cloudflare.filter(_.enabled)
      for (course <- projects.map(_.course).distinctBy(_.c)) {
        val at = host.map(h =>
          s.track(ag.grader.cloudflare.SiteDeployment(course, h).deployed)
        )
        println(
          s"${course.c}: site " + at.fold(
            "not served (cloudflare isn't enabled in config.json)"
          )(u => s"served at $u")
        )
      }
    }
}
