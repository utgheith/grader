package ag.common

import java.util.concurrent.Semaphore

// Whether `say` prints as well as logging: toggled by hitting enter
object Noise {
  private var noise: Boolean = false

  def apply(): Boolean = noise

  // Stops at end of input: read() then returns -1 at once, which would spin
  // and toggle noise at random.
  Thread.ofPlatform().daemon(true).start { () =>
    var reading = true
    while (reading) {
      println("-- hit enter to toggle noise --")
      if (System.in.read() < 0) reading = false
      else noise = !noise
    }
  }
}

// The log, say.txt in the current directory (started afresh by each run)
object Say {
  private val lock: Semaphore = new Semaphore(1)

  lazy val path: os.Path = {
    val p = os.pwd / "say.txt"
    os.remove.all(p)
    p
  }

  // `prefix` is put before the thread's name (ag.r2's nesting dots), `where`
  // after it (the target being worked on)
  def line(prefix: String, where: Option[String], msg: => Any): Unit = {
    val t = if (msg == null) "<null>" else msg.toString
    val thread_name = Thread.currentThread().getName
    val out =
      s"$prefix[$thread_name]${where.map(w => s" [$w]").getOrElse("")} $t\n"
    lock.down(1) {
      if (Noise()) print(out)
      os.write.append(path, out)
    }
  }
}

// logs `msg` (printing it too if noise is on)
def say(msg: => Any): Unit = Say.line("", None, msg)

// logs and prints `msg`
def shout(msg: => Any): Unit = {
  say(msg)
  println(msg)
}
