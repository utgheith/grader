package ag.grader.web

import ag.grader.Milestone

import ag.grader.name.{Alias, CSID, CourseName, ProjectName}

import ag.grader.json.{
  RawCourse,
  RawProject,
  StudentResults,
  RedactedStudentResults,
  PrepareInfo
}

import ag.common.{given_ReadWriter_Instant, given_ReadWriter_SortedMap}
import ag.rules.{
  Commit,
  CommitInfo,
  Context,
  Scope,
  Source,
  State,
  Target,
  Tree
}
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

  def project_repo(c: CourseName, p: ProjectName): String = s"${c}_$p"
  // a student's repo, and their branches in it (see repos.StudentRepo)
  def student_repo(c: CourseName, csid: CSID): String =
    ag.grader.repos.StudentRepo.name_of(c, csid)
  private def work(c: CourseName, p: ProjectName, csid: CSID) =
    source.branch(
      student_repo(c, csid),
      ag.grader.repos.StudentRepo.work_branch(p)
    )
  private def own_results_branch(c: CourseName, p: ProjectName, csid: CSID) =
    source.branch(
      student_repo(c, csid),
      ag.grader.repos.StudentRepo.results_branch(p)
    )

  private def master(repo: String): Target[Option[Commit]] = source.master(repo)

  // `file` in commit `c`'s files, if it's there
  private def text(ctx: Context[?], c: Commit, file: String): Option[String] =
    ctx.read_text(ctx.commit(c).tree, os.RelPath(file))

  // what the courses and projects are: shared with grading
  def courses: Target[SortedMap[CourseName, RawCourse]] =
    ag.grader.repos.CoursesConfig(source).courses
  def project(c: CourseName, p: ProjectName): Target[Option[RawProject]] =
    ag.grader.repos.CoursesConfig(source).course(c).project(p).find

  // a milestone's published results, by alias, and when they were committed
  lazy val milestone_results
      : (CourseName, ProjectName, Milestone) => Target[MilestoneResults] =
    fun { (c: CourseName, p: ProjectName, milestone: Milestone) =>
      target((m = master(s"${project_repo(c, p)}__results"))) {
        (producer, deps) =>
          (for {
            m <- deps.m
            json <- text(producer, m, milestone.results_file)
          } yield Try(
            upickle.default.read[SortedMap[Alias, RedactedStudentResults]](json)
          ).toEither match {
            case Left(e) =>
              MilestoneResults(
                None,
                None,
                Some(
                  s"${milestone.results_file} can't be read: ${e.getMessage}"
                )
              )
            case Right(r) =>
              MilestoneResults(
                Some(r),
                producer
                  .history(m, first_parent = false)
                  .find(changed(producer, _, Seq(milestone.results_file)))
                  .map(_.committed),
                None
              )
          }).getOrElse(MilestoneResults(None, None, None))
      }
    }

  lazy val aliases
      : (CourseName, ProjectName) => Target[SortedMap[CSID, Alias]] = fun {
    (c: CourseName, p: ProjectName) =>
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

  // a student's own results (the current milestone's), and the files beside them: its
  // tests' output
  lazy val own_results: (CourseName, ProjectName, CSID) => Target[
    Option[(StudentResults, Tree)]
  ] = fun { (c: CourseName, p: ProjectName, csid: CSID) =>
    target((m = own_results_branch(c, p, csid))) { (producer, deps) =>
      for {
        m <- deps.m
        json <- text(producer, m, "the_results.json")
        r <- Try(upickle.default.read[StudentResults](json)).toOption
      } yield (r, producer.commit(m).tree)
    }
  }

  // the names of the tests in the tests repo
  lazy val published_tests: (CourseName, ProjectName) => Target[Seq[String]] =
    fun { (c: CourseName, p: ProjectName) =>
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

  // `csid`'s test's commit for `milestone`, from their submission repo (see
  // TestCommit); None if they have no test
  lazy val test_commit: (CourseName, ProjectName, CSID, Milestone) => Target[
    Option[TestCommit]
  ] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, milestone: Milestone) =>
      val repo = student_repo(c, csid)
      target(
        (
          m = work(c, p, csid),
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
            csid,
            milestone
          )
        } yield tc
      }
    }

  // When `milestone`'s commit id file changed, and what it named; for Live
  // results, master's last commits. None if there's no submission repo.
  lazy val commit_history: (CourseName, ProjectName, CSID, Milestone) => Target[
    Option[Seq[CommitRow]]
  ] = fun { (c: CourseName, p: ProjectName, csid: CSID, milestone: Milestone) =>
    target((m = work(c, p, csid))) { (producer, deps) =>
      deps.m.map { m =>
        milestone.commit_id_file match {
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
              .map(ci => CommitRow(ci.id, ci.committed, ci.subject, None, None))
        }
      }
    }
  }
}

object StatusData {
  @upickle.implicits.allowUnknownKeys(false)
  case class MilestoneResults(
      results: Option[SortedMap[Alias, RedactedStudentResults]],
      // when the results file was last committed
      committed: Option[String],
      error: Option[String]
  ) derives ReadWriter

  // A test's commit for a milestone, from its author's submission repo: what
  // SubmissionMilestone.test takes (the last commit on master before the
  // milestone's deadline), and the commit that last changed the test's
  // files by then
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
      csid: CSID,
      milestone: Milestone
  ): Option[TestCommit] = {
    val until =
      ZonedDateTime
        .of(rp.deadline(milestone), ZoneId.systemDefault)
        .nn
        .toInstant
        .nn
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
