package ag.grader

import ag.common.{given_ReadWriter_Instant, given_ReadWriter_SortedMap}
import ag.r2.{Producer, Scope, State, Target, WithData}
import upickle.default.ReadWriter

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.collection.SortedMap
import scala.util.Try

// The status pages' data (see Status), as targets: all of it read from
// committed repos, which `repo` checks out (Gitolite.mirror, for the pages;
// local repos, for tests), so each is worked out again only when a repo it
// reads changes. Meant for a read-only State (see ReadOnlyStates): nothing
// here acts beyond its workspace.
final class StatusData(val repo: String => Target[WithData[Boolean]])
    extends Scope("status") {
  import StatusData.*

  def project_repo(c: String, p: String): String = s"${c}_$p"
  def work_repo(c: String, p: String, csid: CSID): String =
    s"${project_repo(c, p)}_${csid.value}"

  private def checkout(
      producer: Producer[?],
      m: WithData[Boolean]
  ): Option[os.Path] =
    Option.when(m.value)(m.get_data_path(producer))

  lazy val courses: Target[SortedMap[String, RawCourse]] =
    target((m = repo("courses_config"))) { (producer, deps) =>
      checkout(producer, deps.m)
        .map(_ / "courses.json")
        .filter(os.isFile(_))
        .map(f =>
          upickle.default
            .read[Map[String, RawCourseNotSorted]](os.read(f))
            .view
            .mapValues(_.sorted)
            .to(SortedMap)
        )
        .getOrElse(SortedMap())
    }

  lazy val project: (String, String) => Target[Option[RawProject]] = fun {
    (c: String, p: String) =>
      target((cs = courses))((_, deps) =>
        deps.cs.get(c).flatMap(_.projects.get(p))
      )
  }

  // a phase's published results, by alias, and when they were committed
  lazy val phase_results: (String, String, Phase) => Target[PhaseResults] =
    fun { (c: String, p: String, phase: Phase) =>
      target((m = repo(s"${project_repo(c, p)}__results"))) {
        (producer, deps) =>
          checkout(producer, deps.m)
            .map(d => (d, d / phase.results_file))
            .filter((_, f) => os.isFile(f)) match {
            case None         => PhaseResults(None, None, None)
            case Some((d, f)) =>
              Try(
                upickle.default.read[SortedMap[Alias, RedactedStudentResults]](
                  os.read(f)
                )
              ).toEither match {
                case Left(e) =>
                  PhaseResults(
                    None,
                    None,
                    Some(
                      s"${phase.results_file} can't be read: ${e.getMessage}"
                    )
                  )
                case Right(r) =>
                  PhaseResults(
                    Some(r),
                    git_out(
                      d,
                      "log",
                      "-1",
                      "--format=%cI",
                      "--",
                      phase.results_file
                    ).map(_.trim),
                    None
                  )
              }
          }
      }
    }

  lazy val aliases: (String, String) => Target[SortedMap[CSID, Alias]] = fun {
    (c: String, p: String) =>
      target((m = repo(s"${project_repo(c, p)}__aliases"))) {
        (producer, deps) =>
          checkout(producer, deps.m)
            .map(_ / "aliases.json")
            .filter(os.isFile(_))
            .flatMap(f =>
              Try(
                upickle.default.read[SortedMap[CSID, Alias]](os.read(f))
              ).toOption
            )
            .getOrElse(SortedMap[CSID, Alias]())
      }
  }

  def own_results_repo(c: String, p: String, csid: CSID): String =
    s"${work_repo(c, p, csid)}_results"

  // a student's own results (the latest); its tests' output is beside them,
  // in the checkout of own_results_repo
  lazy val own_results
      : (String, String, CSID) => Target[Option[StudentResults]] = fun {
    (c: String, p: String, csid: CSID) =>
      target((m = repo(own_results_repo(c, p, csid)))) { (producer, deps) =>
        for {
          d <- checkout(producer, deps.m)
          f = d / "the_results.json"
          if os.isFile(f)
          r <- Try(upickle.default.read[StudentResults](os.read(f))).toOption
        } yield r
      }
  }

  // the names of the tests in the tests repo
  lazy val published_tests: (String, String) => Target[Seq[String]] = fun {
    (c: String, p: String) =>
      target((m = repo(s"${project_repo(c, p)}__tests"), rp = project(c, p))) {
        (producer, deps) =>
          val extensions = deps.rp.toSeq.flatMap(_.test_extensions)
          checkout(producer, deps.m).toSeq
            .flatMap { d =>
              os.list(d).map(_.last).flatMap { f =>
                extensions.collectFirst {
                  case e if f.endsWith(s".$e") => f.stripSuffix(s".$e")
                }
              }
            }
            .distinct
            .sorted
      }
  }

  // `csid`'s test's commit, from their submission repo (see TestCommit);
  // None if they have no test
  lazy val test_commit: (String, String, CSID) => Target[Option[TestCommit]] =
    fun { (c: String, p: String, csid: CSID) =>
      target((m = repo(work_repo(c, p, csid)), rp = project(c, p))) {
        (producer, deps) =>
          for {
            rp <- deps.rp
            d <- checkout(producer, deps.m)
            tc <- find_test_commit(d, rp, csid)
          } yield tc
      }
    }

  // When `phase`'s commit id file changed, and what it named; for the latest
  // results, master's last commits. None if the submission repo can't be
  // read.
  lazy val commit_history
      : (String, String, CSID, Phase) => Target[Option[Seq[CommitRow]]] = fun {
    (c: String, p: String, csid: CSID, phase: Phase) =>
      target((m = repo(work_repo(c, p, csid)))) { (producer, deps) =>
        checkout(producer, deps.m).map { d =>
          def rows(args: String*): Seq[(String, String, String)] =
            git_out(d, args*).toSeq
              .flatMap(_.linesIterator)
              .filter(_.nonEmpty)
              .map { l =>
                val Array(sha, when, subject) =
                  (l.split("\t", 3).padTo(3, ""): @unchecked)
                (sha, when, subject)
              }
          phase.commit_id_file match {
            case Some(file) =>
              rows("log", "--format=%H%x09%cI%x09%s", "--", file).map {
                (sha, when, subject) =>
                  val content = git_out(d, "show", s"$sha:$file").map(
                    _.linesIterator.nextOption().getOrElse("").trim
                  )
                  val named = content.flatMap(id =>
                    if (id.isEmpty) None
                    else
                      git_out(d, "rev-parse", "-q", "--verify", s"$id^{commit}")
                        .map(_.trim)
                  )
                  CommitRow(sha, when, subject, content, named)
              }
            case None =>
              rows(
                "log",
                "-n",
                "10",
                "--first-parent",
                "--format=%H%x09%cI%x09%s",
                "master"
              ).map { (sha, when, subject) =>
                CommitRow(sha, when, subject, None, None)
              }
          }
        }
      }
  }
}

object StatusData {
  @upickle.implicits.allowUnknownKeys(false)
  case class PhaseResults(
      results: Option[SortedMap[Alias, RedactedStudentResults]],
      // when the results file was last committed
      committed: Option[String],
      error: Option[String]
  ) derives ReadWriter

  // A test's commit, from its author's submission repo: what student_test
  // takes (the last commit on master before test_cutoff), and the commit that
  // last changed the test's files by then
  @upickle.implicits.allowUnknownKeys(false)
  case class TestCommit(
      rev: String,
      sha: String,
      commit_time: String,
      // the first push that brought it to the server
      push_time: Option[Instant],
      // commits on master after `rev` that change the test: ignored
      later_changes: Int
  ) derives ReadWriter

  // a commit, and for a commit id file's history, what the file held then
  // (None: deleted) and the commit that names (None: none)
  @upickle.implicits.allowUnknownKeys(false)
  case class CommitRow(
      sha: String,
      when: String,
      subject: String,
      content: Option[String],
      named: Option[String]
  ) derives ReadWriter

  private def find_test_commit(
      d: os.Path,
      rp: RawProject,
      csid: CSID
  ): Option[TestCommit] = {
    val until = ZonedDateTime
      .of(rp.test_cutoff, ZoneId.systemDefault)
      .nn
      .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      .nn
    val files = rp.test_extensions.map(e => s"${csid.value}.$e")
    def lines(args: String*): Seq[String] =
      git_out(d, args*).toSeq
        .flatMap(_.linesIterator)
        .map(_.trim)
        .filter(_.nonEmpty)
    for {
      rev <- lines(
        "rev-list",
        "-n",
        "1",
        "--first-parent",
        "--until",
        until,
        "master"
      ).headOption
      if lines(
        Seq("ls-tree", "--name-only", rev, "--") ++ files*
      ).size == files.size
      line <- lines(
        Seq(
          "log",
          "-1",
          "--first-parent",
          "--format=%H%x09%cI",
          rev,
          "--"
        ) ++ files*
      ).headOption
      Array(sha, when) <- Some(line.split("\t", 2)).filter(_.length == 2)
    } yield {
      // the push hook notes each commit a push leaves master on; the first
      // push with this commit noted one that has it
      val with_it = sha +: lines("rev-list", "--ancestry-path", s"$sha..master")
      val notes = lines(
        Seq(
          "log",
          "--no-walk",
          "--notes=push_time",
          "--notes=commits",
          "--format=%N"
        ) ++ with_it*
      )
      TestCommit(
        rev = rev,
        sha = sha,
        commit_time = when,
        push_time = PrepareInfo.push_time_from(notes),
        later_changes = lines(
          Seq(
            "log",
            "--first-parent",
            "--format=%H",
            s"$rev..master",
            "--"
          ) ++ files*
        ).size
      )
    }
  }

  // git, read-only, in a local checkout; None if it fails
  private[grader] def git_out(dir: os.Path, args: String*): Option[String] =
    Try(
      os.proc("git" +: args).call(cwd = dir, check = false, stderr = os.Pipe)
    ).toOption.filter(_.exitCode == 0).map(_.out.text())
}

// A read-only State in `workspace` for the status pages, a new one every
// `ttl_ms`: a State works out each target once, so a new one is how they
// notice new commits. Whoever makes these holds `workspace`'s RunLock: two
// States mustn't share a workspace.
final class ReadOnlyStates(workspace: os.Path, ttl_ms: Long) {
  private var current: Option[(Long, State)] = None

  def apply(): State = synchronized {
    val now = System.currentTimeMillis()
    current match {
      case Some((at, s)) if now - at < ttl_ms => s
      case _                                  =>
        val s = State(workspace, read_only = true)
        current = Some((now, s))
        s
    }
  }
}
