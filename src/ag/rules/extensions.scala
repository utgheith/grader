package ag.rules

import upickle.default.{ReadWriter, readwriter}

import java.nio.charset.{Charset, StandardCharsets}
import java.security.MessageDigest
import scala.collection.{SortedMap, SortedSet}
import java.time.{Duration, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicLong
import scala.util.control.NonFatal
import java.time.ZoneId
import java.time.ZonedDateTime

///// ReadWriter for SortedMap ////

given [K: Ordering: ReadWriter, V: ReadWriter]: ReadWriter[SortedMap[K, V]] =
  readwriter[Seq[(K, V)]]
    .bimap[SortedMap[K, V]](sm => sm.to(Seq), s => s.to(SortedMap))

////// ReadWriter for SortedSet //////

given [K: Ordering: ReadWriter]: ReadWriter[SortedSet[K]] =
  readwriter[Seq[K]]
    .bimap[SortedSet[K]](ss => ss.toSeq, s => s.to(SortedSet))

/////// ReadWriter for os.RelPath /////

given ReadWriter[os.RelPath] = readwriter[String]
  .bimap[os.RelPath](_.toString, s => os.RelPath(s))

given ReadWriter[os.Path] = readwriter[String]
  .bimap[os.Path](_.toString, s => os.Path(s))

//////// ReadWriter for LocalDateTime /////

given ReadWriter[LocalDateTime] = {
  val dateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy:MM:dd:HH:mm:ss").nn

  readwriter[String].bimap[LocalDateTime](
    ins => ins.format(dateTimeFormatter).nn,
    str => LocalDateTime.parse(str, dateTimeFormatter).nn
  )
}

//////// ReadWriter for ZoneId ////////

given ReadWriter[ZoneId] = readwriter[String].bimap[ZoneId](
  zid => zid.getId,
  str => ZoneId.of(str)
)

//////// ReadWriter for ZonedDateTime //////

given ReadWriter[ZonedDateTime] =
  readwriter[(LocalDateTime, ZoneId)].bimap[ZonedDateTime](
    zdt => (zdt.toLocalDateTime, zdt.getZone),
    pair => ZonedDateTime.of(pair._1, pair._2)
  )

//////// ReadWriter for ZonedDateTime //////

given ReadWriter[Duration] = readwriter[String]
  .bimap[Duration](dur => dur.toString, str => Duration.parse(str))

/** **************
  */
/* MessageDigest */
/** **************
  */

extension (md: MessageDigest) {
  def update(s: String, charset: Charset = StandardCharsets.UTF_8.nn): Unit =
    md.update(s.getBytes(charset))
}

def say(msg: Any): Unit = say.synchronized {
  println(s"[${Thread.currentThread().nn.getName}] $msg")
}

/** ********
  */
/* os.proc */
/** ********
  */

val next_std_id = new AtomicLong(0)

lazy val std_dir: os.Path = {
  val d = os.pwd / "std_out_err"
  os.remove.all(d)
  os.makeDir(d)
  d
}

def trace(msg: Any): Unit = {
  val file = std_dir / s"${Thread.currentThread().nn.getName.nn}.trace"
  os.write.append(file, s"${String.valueOf(msg).nn}\n", createFolders = true)
}

case class ProcException(
    std_out_err: os.Path,
    cmd_id: Long,
    thread_id: String,
    rc: Int
) extends Exception {
  override def getMessage: String =
    s"[$thread_id] cmd_id:[$cmd_id] rc:[$rc] std_out_err:[$std_out_err]"
}

val default_timeout: Int = 5 * 60 * 1000
val default_check: Boolean = true
lazy val default_cwd = os.pwd
val default_stdin: os.ProcessInput = ""

extension (p: os.proc) {
  def run(
      cwd: os.Path = default_cwd,
      check: Boolean = default_check,
      timeout: Int = default_timeout,
      stdin: os.ProcessInput = default_stdin
  ): (Int, os.Path, Option[os.Path]) = {
    val id = next_std_id.getAndIncrement()
    val stdout = std_dir / s"$id.out"
    val stderr = std_dir / s"$id.err"
    val cmd = std_dir / s"$id.cmd"
    val rc = std_dir / s"$id.rc"
    val thread = std_dir / s"$id.thread"

    // say(s"---> $id:${p.commandChunks.mkString("[", ",", "]")} @ ${cwd.relativeTo(os.pwd)}")

    val thread_id = Thread.currentThread().nn.getName.nn

    os.write(thread, thread_id)
    os.write(cmd, p.commandChunks.toString)
    trace(p.commandChunks)

    // say(s"cmd#$id: ${p.commandChunks}")

    val out = p.call(
      stdout = os.PathRedirect(stdout),
      stderr = os.PathRedirect(stderr),
      check = false,
      cwd = cwd,
      timeout = timeout,
      stdin = stdin
    )
    val exit_code = out.exitCode
    if (exit_code != 0) {
      // say(s"---> $id:rc=${exit_code}")
      os.write(rc, exit_code.toString)
      if (check)
        throw ProcException(
          std_out_err = std_dir,
          rc = exit_code,
          cmd_id = id,
          thread_id = thread_id
        )
    }
    val err = if (os.size(stderr) == 0) {
      os.remove.all(stderr)
      None
    } else {
      Some(stderr)
    }
    (exit_code, stdout, err)
  }

  def check(
      cwd: os.Path = default_cwd,
      timeout: Int = default_timeout,
      stdin: os.ProcessInput = default_stdin
  ): Unit = {
    val _ = run(cwd, check = true, timeout = timeout, stdin = stdin)
  }

  def lines(cwd: os.Path = os.pwd): Seq[String] = {
    val (_, stdout, _) = run(cwd = cwd)
    os.read.lines(stdout)
  }
}

// Semaphore extensions //

extension (s: Semaphore) {
  def down[A](permits: Int)(f: => A): A = try {
    s.acquire(permits)
    f
  } finally {
    s.release(permits)
  }

  def down[A](f: => A): A = down(1)(f)
}

// os.Path

extension (path: os.Path) {
  def is_git_repo: Boolean = os.isDir(path / ".git")
  def is_working_repo: Boolean = is_git_repo && path != os.pwd
}

// () => A

extension [A](fa: Function0[A]) {
  def ||[B](fb: Function0[B]): Function0[A | B] = { () =>
    try {
      fa()
    } catch {
      case NonFatal(_) =>
        fb()
    }
  }

  def retry(f2: Function0[?]): Function0[A] = { () =>
    try {
      fa()
    } catch {
      case NonFatal(_) =>
        f2()
        fa()
    }
  }
}

def timed[A](f: => A): (Long, A) = {
  val start = System.currentTimeMillis()
  val out = f
  (System.currentTimeMillis() - start, out)
}

def human(ms: Long): String = {
  if (ms < 1000) {
    s"${ms}ms"
  } else if (ms < 60 * 1000) {
    f"${ms / 1000.0}%.03fs"
  } else {
    f"${ms / (1000.0 * 60.0)}%.03fm"
  }
}

// Extractor to convert nullable regex matches to Scala Options
// Originally sourced from https://stackoverflow.com/a/1843127
object Optional {
  def unapply[T](a: T) = if (null == a) Some(None) else Some(Some(a))
}
