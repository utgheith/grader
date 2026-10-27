package ag.common

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, OverlappingFileLockException}
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.{CREATE, READ, WRITE}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

// One grader at a time per workspace: two runs sharing one would corrupt
// each other's state.
//
// The lock is the operating system's, on `<dir>/.grader.lock`: it goes away
// when its holder exits, however that happens, so it can't go stale. The
// file just records who holds it, for the error message.
object RunLock {

  // Right(release) if acquired; Left(who holds it) if not.
  def acquire(dir: os.Path, what: String): Either[String, () => Unit] = {
    val file = dir / ".grader.lock"
    os.makeDir.all(dir)
    val channel = FileChannel.open(file.toNIO, CREATE, READ, WRITE).nn
    val lock =
      try Option(channel.tryLock())
      catch { case _: OverlappingFileLockException => None } // held by us
    lock match {
      case Some(l) =>
        val started = LocalDateTime
          .now()
          .nn
          .format(
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
          )
        val _ = channel.truncate(0)
        val _ = channel.write(
          ByteBuffer.wrap(
            s"pid ${ProcessHandle.current().nn.pid()}, started $started: $what"
              .getBytes(StandardCharsets.UTF_8)
          )
        )
        channel.force(false)
        Right(() => {
          l.release()
          channel.close()
        })
      case None =>
        channel.close()
        val holder =
          try os.read(file).trim
          catch { case NonFatal(_) => "" }
        Left(if (holder.isEmpty) "unknown" else holder)
    }
  }

  // Runs `f` holding the lock, or exits with an explanation if another run
  // holds it.
  def apply[A](dir: os.Path, what: String)(f: => A): A =
    acquire(dir, what) match {
      case Right(release) =>
        try f
        finally release()
      case Left(holder) =>
        System.err.println(
          s"""Another grader is already using the workspace ${dir.toString}:
             |  $holder
             |Wait for it to finish (or stop it), then try again.""".stripMargin
        )
        sys.exit(2)
    }
}
