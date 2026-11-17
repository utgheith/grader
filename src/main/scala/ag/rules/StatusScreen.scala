package ag.rules

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.control.NonFatal

// A full-screen view of a State's Progress, redrawn every second while `f`
// runs: how many targets are in each phase and how the finished ones went,
// then, filling the rest of the screen, the targets that have been running
// longest. For example:
//
//   4m12s: 1234 targets started
//   in flight: 31 checking, 2 queued, 5 running
//   finished: 1000 reused, 195 ran, 1 failed
//
//   running, longest first:
//      1m23s  t3/t3 for c1_p3_s1 (up to 1 runs)
//        12s  ag/grader/targets/SubmissionMilestone/results/c1/p3/s2/m1/3
//
// What's printed meanwhile (println, shout) is held and printed when the
// screen is gone (say.txt still logs it as it goes), then a line with the
// counts. Only on a terminal, and only after the first second, so a quick
// command shows just its output.
object StatusScreen {
  import Progress.{Phase, Snapshot}

  private val csi = "\u001b["

  def apply[A](progress: Progress, enabled: Boolean)(f: => A): A = {
    val console = System.console()
    if (!enabled || console == null || !console.isTerminal) f
    else show(progress)(f)
  }

  private def show[A](progress: Progress)(f: => A): A = {
    val tty = System.out
    val held = ByteArrayOutputStream()
    val hold = PrintStream(held, true, UTF_8)
    val start = System.currentTimeMillis()
    val shown = AtomicBoolean(false)
    val ended = AtomicBoolean(false)

    // leaves the screen (once), then prints what was held
    def end(): Unit = tty.synchronized {
      if (!ended.getAndSet(true)) {
        if (shown.get) tty.print(s"$csi?25h$csi?1049l")
        hold.flush()
        tty.print(held.toString(UTF_8))
        if (shown.get) tty.println(summary(progress.snapshot, start))
        tty.flush()
      }
    }

    // a ^C ends the program without unwinding `f`
    val hook = Thread(() => end())
    Runtime.getRuntime.addShutdownHook(hook)
    val drawer = Thread.ofPlatform().daemon(true).start { () =>
      try {
        Thread.sleep(1000)
        while (!ended.get) {
          val (width, height) = size()
          val lines =
            frame(
              progress.snapshot,
              start,
              System.currentTimeMillis(),
              width,
              height
            )
          tty.synchronized {
            if (!ended.get) {
              if (!shown.getAndSet(true)) tty.print(s"$csi?1049h$csi?25l")
              tty.print(
                s"${csi}H" + lines
                  .map(_ + s"${csi}K")
                  .mkString("\r\n") + s"${csi}J"
              )
              tty.flush()
            }
          }
          Thread.sleep(1000)
        }
      } catch { case _: InterruptedException => () }
    }
    System.setOut(hold)
    try Console.withOut(hold)(f)
    finally {
      drawer.interrupt()
      drawer.join()
      System.setOut(tty)
      end()
      try { val _ = Runtime.getRuntime.removeShutdownHook(hook) }
      catch { case _: IllegalStateException => () } // already shutting down
    }
  }

  // the terminal's (columns, rows)
  private def size(): (Int, Int) =
    try {
      val out = os
        .proc("stty", "size")
        .call(stdin = os.PathRedirect(os.Path("/dev/tty")), check = false)
      out.out.text().trim.split(" ").toSeq match {
        case Seq(rows, cols) if rows.toInt > 0 && cols.toInt > 0 =>
          (cols.toInt, rows.toInt)
        case _ => (80, 24)
      }
    } catch { case NonFatal(_) => (80, 24) }

  private[rules] def duration(ms: Long): String = {
    val s = ms / 1000
    if (s < 60) s"${s.toString}s"
    else if (s < 3600) f"${s / 60}%dm${s % 60}%02ds"
    else f"${s / 3600}%dh${s / 60 % 60}%02dm"
  }

  // `s` in at most `width` characters: its start and end, if it must
  private def fit(s: String, width: Int): String =
    if (s.length <= width) s
    else if (width < 10) s.take(width)
    else {
      val keep = width - 1
      s.take(keep / 3) + "…" + s.takeRight(keep - keep / 3)
    }

  private[rules] def frame(
      s: Snapshot,
      start: Long,
      now: Long,
      width: Int,
      height: Int
  ): Seq[String] = {
    val running =
      s.in_flight.filter(_.phase == Phase.Running).sortBy(_.since)
    val header = Seq(
      s"${duration(now - start)}: ${s.started.toString} targets started",
      s"in flight: ${s.count(Phase.Checking).toString} checking, " +
        s"${s.count(Phase.Queued).toString} queued, " +
        s"${running.size.toString} running",
      s"finished: ${s.reused.toString} reused, ${s.ran.toString} ran, ${s.failed.toString} failed",
      "",
      if (running.isEmpty) "nothing running" else "running, longest first:"
    )
    val room = math.max(0, height - header.size)
    val shown =
      if (running.size <= room) running
      else running.take(math.max(0, room - 1))
    val rows = shown.map { r =>
      val d = duration(now - r.since)
      s"  ${" " * math.max(0, 6 - d.length)}$d  ${r.display}"
    }
    val more =
      if (shown.size < running.size)
        Seq(s"  … and ${(running.size - shown.size).toString} more")
      else Seq()
    (header ++ rows ++ more).take(height).map(fit(_, width))
  }

  private def summary(s: Snapshot, start: Long): String =
    s"targets: ${s.started.toString} started, ${s.reused.toString} reused, " +
      s"${s.ran.toString} ran, ${s.failed.toString} failed in " +
      duration(System.currentTimeMillis() - start)
}
