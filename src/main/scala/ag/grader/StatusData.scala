package ag.grader

import ag.common.{given_ReadWriter_Instant, given_ReadWriter_SortedMap}
import ag.r3.{Commit, CommitInfo, Context, Scope, Source, State, Target, Tree}
import upickle.default.ReadWriter

import java.time.{Instant, OffsetDateTime, ZoneId, ZonedDateTime}
import scala.collection.SortedMap
import scala.util.Try

// The status pages' data (see Status), as r3 targets on `source`: a local
// mirror of the git server, kept up to date by something else (`grader
// mirror`). Each target depends on the refs it reads and reads only at the
// commits they name, so it's worked out again only when one of those refs
// moves. Meant for a read-only State (see ReadOnlyStates): nothing here acts
// beyond its workspace, and nothing writes to `source`.
final class StatusData(val source: Source) extends Scope("status") {
  import StatusData.*

  def project_repo(c: String, p: String): String = s"${c}_$p"
  def work_repo(c: String, p: String, csid: CSID): String =
    s"${project_repo(c, p)}_${csid.value}"
  def own_results_repo(c: String, p: String, csid: CSID): String =
    s"${work_repo(c, p, csid)}_results"

  private def master(repo: String): Target[Option[Commit]] = source.master(repo)

  // `file` in commit `c`'s files, if it's there
  private def text(ctx: Context[?], c: Commit, file: String): Option[String] =
    ctx.read_text(ctx.commit(c).tree, os.RelPath(file))

  // what the courses and projects are: shared with grading
  val definitions: Definitions = Definitions(source)
  def courses: Target[SortedMap[String, RawCourse]] = definitions.courses
  def project(c: String, p: String): Target[Option[RawProject]] =
    definitions.find_project(c, p)

  // a phase's published results, by alias, and when they were committed
  lazy val phase_results: (String, String, Phase) => Target[PhaseResults] =
    fun { (c: String, p: String, phase: Phase) =>
      target((m = master(s"${project_repo(c, p)}__results"))) {
        (producer, deps) =>
          (for {
            m <- deps.m
            json <- text(producer, m, phase.results_file)
          } yield Try(
            upickle.default.read[SortedMap[Alias, RedactedStudentResults]](json)
          ).toEither match {
            case Left(e) =>
              PhaseResults(
                None,
                None,
                Some(s"${phase.results_file} can't be read: ${e.getMessage}")
              )
            case Right(r) =>
              PhaseResults(
                Some(r),
                producer
                  .history(m, first_parent = false)
                  .find(changed(producer, _, Seq(phase.results_file)))
                  .map(_.committed),
                None
              )
          }).getOrElse(PhaseResults(None, None, None))
      }
    }

  lazy val aliases: (String, String) => Target[SortedMap[CSID, Alias]] = fun {
    (c: String, p: String) =>
      target((m = master(s"${project_repo(c, p)}__aliases"))) {
        (producer, deps) =>
          (for {
            m <- deps.m
            json <- text(producer, m, "aliases.json")
            a <- Try(
              upickle.default.read[SortedMap[CSID, Alias]](json)
            ).toOption
          } yield a).getOrElse(SortedMap[CSID, Alias]())
      }
  }

  // a student's own results (the latest), and the files beside them: its
  // tests' output
  lazy val own_results
      : (String, String, CSID) => Target[Option[(StudentResults, Tree)]] = fun {
    (c: String, p: String, csid: CSID) =>
      target((m = master(own_results_repo(c, p, csid)))) { (producer, deps) =>
        for {
          m <- deps.m
          json <- text(producer, m, "the_results.json")
          r <- Try(upickle.default.read[StudentResults](json)).toOption
        } yield (r, producer.commit(m).tree)
      }
  }

  // the names of the tests in the tests repo
  lazy val published_tests: (String, String) => Target[Seq[String]] = fun {
    (c: String, p: String) =>
      target(
        (m = master(s"${project_repo(c, p)}__tests"), rp = project(c, p))
      ) { (producer, deps) =>
        val extensions = deps.rp.toSeq.flatMap(_.test_extensions)
        (for {
          m <- deps.m.toSeq
          f <- producer.list(producer.commit(m).tree).getOrElse(Seq())
          name <- extensions.collectFirst {
            case e if f.endsWith(s".$e") => f.stripSuffix(s".$e")
          }
        } yield name).distinct.sorted
      }
  }

  // `csid`'s test's commit, from their submission repo (see TestCommit);
  // None if they have no test
  lazy val test_commit: (String, String, CSID) => Target[Option[TestCommit]] =
    fun { (c: String, p: String, csid: CSID) =>
      val repo = work_repo(c, p, csid)
      target(
        (
          m = master(repo),
          push_times = source.ref(repo, "refs/notes/push_time"),
          commits = source.ref(repo, "refs/notes/commits"),
          rp = project(c, p)
        )
      ) { (producer, deps) =>
        for {
          rp <- deps.rp
          m <- deps.m
          tc <- find_test_commit(
            producer,
            m,
            Seq(deps.push_times, deps.commits).flatten,
            rp,
            csid
          )
        } yield tc
      }
    }

  // When `phase`'s commit id file changed, and what it named; for the latest
  // results, master's last commits. None if there's no submission repo.
  lazy val commit_history
      : (String, String, CSID, Phase) => Target[Option[Seq[CommitRow]]] = fun {
    (c: String, p: String, csid: CSID, phase: Phase) =>
      target((m = master(work_repo(c, p, csid)))) { (producer, deps) =>
        deps.m.map { m =>
          phase.commit_id_file match {
            case Some(file) =>
              for {
                ci <- producer.history(m, first_parent = false)
                if changed(producer, ci, Seq(file))
              } yield {
                val content = producer
                  .read_text(ci.tree, os.RelPath(file))
                  .map(_.linesIterator.nextOption().getOrElse("").trim)
                val named = content
                  .filter(_.nonEmpty)
                  .flatMap(id => producer.resolve(m, id))
                  .map(_.id)
                CommitRow(ci.id, ci.committed, ci.subject, content, named)
              }
            case None =>
              producer
                .history(m, first_parent = true, limit = 10)
                .map(ci =>
                  CommitRow(ci.id, ci.committed, ci.subject, None, None)
                )
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

  // Whether `ci` changed any of `files` from its first parent (as `git log
  // -- files` shows it: linear history, which these repos have)
  private def changed(
      ctx: Context[?],
      ci: CommitInfo,
      files: Seq[String]
  ): Boolean = {
    val at = ci.tree.at.getOrElse("")
    val before = ci.parents.headOption.map(p => ctx.commit(Commit(at, p)).tree)
    files.exists(f =>
      ctx.id_at(ci.tree, os.RelPath(f)) != before.flatMap(
        ctx.id_at(_, os.RelPath(f))
      )
    )
  }

  private def find_test_commit(
      ctx: Context[?],
      master: Commit,
      notes: Seq[Commit],
      rp: RawProject,
      csid: CSID
  ): Option[TestCommit] = {
    val until =
      ZonedDateTime.of(rp.test_cutoff, ZoneId.systemDefault).nn.toInstant.nn
    def time(ci: CommitInfo): Instant =
      OffsetDateTime.parse(ci.committed).nn.toInstant.nn
    val files = rp.test_extensions.map(e => s"${csid.value}.$e")
    val chain = ctx.history(master, first_parent = true)
    for {
      // the last commit on master before the cutoff (rev-list --until)
      rev <- chain.find(time(_).compareTo(until) <= 0)
      if files.forall(f => ctx.id_at(rev.tree, os.RelPath(f)).isDefined)
      // the commit that last changed the test by then
      sha <- ctx
        .history(Commit(master.at, rev.id), first_parent = true)
        .find(changed(ctx, _, files))
    } yield {
      // the push hook notes each commit a push leaves master on; the first
      // push with this commit noted one that has it
      val with_it = Commit(master.at, sha.id) +:
        ctx.ancestry_path(Commit(master.at, sha.id), master)
      val lines = for {
        c <- with_it
        n <- notes
        text <- ctx.note(n, c.id).toSeq
        line <- text.linesIterator
      } yield line
      TestCommit(
        rev = rev.id,
        sha = sha.id,
        commit_time = sha.committed,
        push_time = PrepareInfo.push_time_from(lines),
        later_changes =
          chain.takeWhile(_.id != rev.id).count(changed(ctx, _, files))
      )
    }
  }
}

// A read-only r3 State in `workspace` for the status pages, a new one every
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
