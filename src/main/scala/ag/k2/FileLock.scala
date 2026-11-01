package ag.k2

import language.experimental.captureChecking

import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.{CREATE, WRITE}
import java.util.concurrent.ConcurrentHashMap

// Runs code holding both this process's and the OS's lock on a lock file, so
// threads and processes sharing a local file take turns.
object FileLock {
  private val locks = ConcurrentHashMap[os.Path, Object]()

  def apply[A](lock_file: os.Path)(f: => A): A =
    locks.computeIfAbsent(lock_file, _ => Object()).nn.synchronized {
      os.makeDir.all(lock_file / os.up)
      val channel = FileChannel.open(lock_file.toNIO, CREATE, WRITE).nn
      try {
        val lock = channel.lock().nn
        try f
        finally lock.release()
      } finally channel.close()
    }
}
