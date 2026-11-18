package ag.grader

import ag.rules.{Scope, Target}

import java.time.{Instant, LocalDateTime, ZoneId}

// The course's time: what deadlines are measured against. The wall clock,
// unless config.json names a clock file (Config.clock_file): then the epoch
// seconds in it, which a simulation (gitolite-mirror/sim) moves, jumping
// ahead when nothing happens, and its server's push hook reads too. How long
// things take (runs, timeouts, caches) stays on the wall clock.
object Clock extends Scope("clock") {

  def now_ms: Long =
    Config.clock_file.fold(System.currentTimeMillis())(f =>
      os.read(f).trim.toLong * 1000
    )

  def now: LocalDateTime =
    LocalDateTime
      .ofInstant(Instant.ofEpochMilli(now_ms), ZoneId.systemDefault())
      .nn

  // now, to the minute (as ag.rules.periodic, but on the course's time):
  // what depends on it is worked out again as the course's time passes, and
  // reruns only when it changes
  lazy val minute: Target[Long] =
    target(EmptyTuple) { (_, _) => (now_ms / 60000) * 60000 }
}
