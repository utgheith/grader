package ag.common

import java.util.concurrent.ConcurrentLinkedQueue
import scala.util.control.NonFatal

// Temporary directories for the tests, removed with everything in them when
// the JVM exits. (os.temp.dir's deleteOnExit is java.io.File's, which leaves
// a directory that isn't empty: test runs used up /tmp's inodes.) r3's
// checkouts are read-only, so they're made writable first.
object TempDirs {
  private val made = ConcurrentLinkedQueue[os.Path]()

  Runtime.getRuntime.nn.addShutdownHook(Thread(() => made.forEach(remove)))

  private def remove(d: os.Path): Unit =
    try {
      for (p <- os.walk(d, includeTarget = true) if os.isDir(p, followLinks = false))
        val _ = p.toIO.setWritable(true)
      os.remove.all(d)
    } catch {
      case NonFatal(e) => System.err.println(s"TempDirs: can't remove ${d.toString}: ${e.toString}")
    }

  def dir(prefix: String = "test"): os.Path = {
    val d = os.temp.dir(prefix = prefix, deleteOnExit = false)
    val _ = made.add(d)
    d
  }
}
