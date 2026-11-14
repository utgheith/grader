package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.{Many, Producer, Scope, Target, WithData, create_data}
import upickle.default.ReadWriter

import scala.NamedTuple.NamedTuple
import scala.collection.SortedMap

// How the server makes a repo that isn't there: fork `fork_from`, and give
// `readers` and `writers` access (as RepoInfo.update)
@upickle.implicits.allowUnknownKeys(false)
final case class Create(
    fork_from: String,
    readers: Seq[String],
    writers: Seq[String]
) derives ReadWriter

// What a repo on the server should hold: its files (as the data of the
// target that worked it out), on top of `base`, the commit they were worked
// out from: the repo's master on the mirror, or, if there's no such repo
// (`exists`), the master of the repo `create` forks it from, which is where
// the fork starts (None: no master). The Outbox records it, and `sync`
// makes the repo if needed (it can't without `create`) and pushes it, then
// sends its `notices`.
@upickle.implicits.allowUnknownKeys(false)
final case class Publish(
    repo: String,
    exists: Boolean,
    base: Option[String],
    message: String,
    create: Option[Create],
    notices: Seq[Notice] = Seq()
) derives ReadWriter

// The repos the grader keeps up to date, as r3 targets on the local mirror:
// each works out what its repo should hold, from the mirror and the targets
// it depends on, and nothing more (see Outbox). The r3 form of Project's
// gitolite_targets.
final class Publishers(val grading: Grading, val dropbox: Dropbox)
    extends Scope("publish") {
  private val runs = grading.runs
  private val subs = runs.subs
  private val defs = subs.defs
  private val source = defs.source

  // what the repo definitions (ag.grader.repos) work from
  val env: repos.Env = repos.Env(source, dropbox)

  private def staff(c: CourseName): String = s"@${c}_staff"

  // A target that works out `repo`'s files: `f` gets a directory with the
  // files of the repo's master on the mirror (if it has one), or, with no
  // repo, of what it would be forked from (as RepoInfo.update), to change.
  private def publisher[N <: Tuple, V <: Tuple](
      repo: String,
      message: String,
      create: Option[Create]
  )(targets: NamedTuple[N, V])(
      f: (Producer[?], Target.Values[N, V], os.Path) => Unit
  )(using
      Tuple.Union[V] <:< (Target[?] | Many[?]),
      sourcecode.FullName
  ): Target[WithData[Publish]] =
    publisher_with_notices(repo, message, create)(targets) { (pr, v, dir) =>
      f(pr, v, dir)
      Seq()
    }

  // The same, but `f` also says who to tell once it's synced
  private def publisher_with_notices[N <: Tuple, V <: Tuple](
      repo: String,
      message: String,
      create: Option[Create]
  )(targets: NamedTuple[N, V])(
      f: (Producer[?], Target.Values[N, V], os.Path) => Seq[Notice]
  )(using
      Tuple.Union[V] <:< (Target[?] | Many[?]),
      sourcecode.FullName
  ): Target[WithData[Publish]] =
    complex_target[WithData[Publish]] { tracker =>
      val exists = tracker.track(source.exists(repo))
      val base = tracker.track(
        create.filterNot(_ => exists) match {
          case Some(c) => source.master(c.fork_from)
          case None    => source.master(repo)
        }
      )
      val values = tracker.track_all(targets)
      tracker.run_if_needed { producer =>
        create_data(producer) { dir =>
          base.foreach(b => producer.copy_to(producer.commit(b).tree, dir))
          val notices = f(producer, values, dir)
          Publish(repo, exists, base.map(_.id), message, create, notices)
        }
      }
    }

  // everything in `dir` but its dot files (e.g. .initialized)
  private def clear(dir: os.Path): Unit =
    for (f <- os.list(dir) if !f.last.startsWith(".")) os.remove.all(f)

  // the project's aliases (see repos.AliasesRepo)
  def aliases(c: CourseName, p: ProjectName): Target[WithData[Publish]] =
    repos.AliasesRepo(c, p, env).latest

  // the override repo (see repos.OverrideRepo)
  def override_repo(c: CourseName, p: ProjectName): Target[WithData[Publish]] =
    repos.OverrideRepo(c, p, env).latest

  // the students' own tests, for the staff (as
  // Project.publish_submitted_tests)
  lazy val submitted_tests
      : (CourseName, ProjectName) => Target[WithData[Publish]] =
    fun { (c: CourseName, p: ProjectName) =>
      publisher(
        s"${subs.project_repo(c, p)}__submitted_tests",
        "tests",
        Some(Create("empty", Seq(staff(c)), Seq()))
      )(
        (
          tests = subs.student_tests_by_csid(c, p),
          aliases = runs.aliases(c, p),
          ext = defs.test_extensions(c, p)
        )
      ) { (producer, deps, dir) =>
        clear(dir)
        repos.TestsRepo.copy_student_tests(
          producer,
          deps.tests,
          deps.aliases,
          deps.ext,
          dir
        )
      }
    }

  // every test, for everyone (see repos.TestsRepo)
  def tests(c: CourseName, p: ProjectName): Target[WithData[Publish]] =
    repos.TestsRepo(c, p, env).latest

  // what a project publishes, in the order to sync it: the override repo
  // first, since the tests come from it
  def all(c: CourseName, p: ProjectName): Seq[Target[WithData[Publish]]] =
    Seq(override_repo(c, p), aliases(c, p), submitted_tests(c, p), tests(c, p))

  // ---- results ----------------------------------------------------------

  // the files a run leaves that are published with a student's results, by
  // extension (as Project.copy_results)
  // A student's latest results (see repos.StudentResultsRepo)
  def student_results(
      c: CourseName,
      p: ProjectName,
      csid: CSID,
      n: Int
  ): Target[WithData[Publish]] =
    repos.StudentResultsRepo(c, p, csid, env).latest(n)

  // Every student's results, by alias, for each phase the project publishes
  // (Phase.results_file), for the report (as Project.publish_results). One
  // publisher for every phase's file: a repo has one thing pending at a time.
  lazy val results
      : (CourseName, ProjectName, Int) => Target[WithData[Publish]] = fun {
    (c: CourseName, p: ProjectName, n: Int) =>
      publisher(
        subs.results_repo(c, p),
        "update results",
        Some(Create("empty", Seq("@all"), Seq()))
      )((files = grading.results_files(c, p, n))) { (_, deps, dir) =>
        for ((name, data) <- deps.files)
          os.write.over(dir / name, upickle.default.write(data, indent = 2))
      }
  }

  // what grading a project to `n` tries publishes, for `students` (those
  // with a submission), in the order to sync it: what the project
  // publishes (see all: its override, aliases, submitted tests and tests,
  // as publish_results published them, so a new student's alias and tests
  // go out with their first results), then the results repo, and each
  // student's own results and prepared code (what the tests ran on)
  def graded(
      c: CourseName,
      p: ProjectName,
      students: Iterable[CSID],
      n: Int
  ): Seq[Target[WithData[Publish]]] =
    all(c, p) ++ (results(c, p, n) +: students.toSeq.flatMap(csid =>
      Seq(
        student_results(c, p, csid, n),
        repos.PreparedRepo(c, p, csid, env).latest
      )
    ))

  // ---- the courses' repos ----------------------------------------------

  // every active course's students' keys, and how it tells them about
  // changes, by course
  private lazy val keys_by_course: Target[
    Seq[(CourseName, SortedMap[CSID, String], NotificationConfig)]
  ] = complex_target[
    Seq[(CourseName, SortedMap[CSID, String], NotificationConfig)]
  ] { tracker =>
    val courses = tracker.track(defs.active_courses(Definitions.any)).toSeq
    val keys = tracker.track_all(courses.map(dropbox.keys))
    val notifications = tracker.track_all(courses.map(defs.notifications))
    tracker.run_if_needed(_ =>
      courses.lazyZip(keys).lazyZip(notifications).toSeq
    )
  }

  // The students' keys in gitolite-admin, as they are in the dropbox: added
  // or changed, never removed (as Course.publish_keys, for every course at
  // once, since they share the repo); each student whose key is added or
  // changed is told (for the first course that has it), as their course
  // says. It's never made.
  lazy val keys: Target[WithData[Publish]] =
    publisher_with_notices("gitolite-admin", "updated keys", None)(
      (courses = keys_by_course)
    ) { (_, deps, dir) =>
      for {
        (c, keys, n) <- deps.courses
        (csid, key) <- keys.toSeq
        f = dir / "keydir" / s"${csid.value}.pub"
        if !os.exists(f) || os.read(f).trim.nn != key
        notice <- {
          os.write.over(f, key, createFolders = true)
          if (n.key_update)
            Notice.routed(n, csid, Notice.What.KeyUpdated(c, csid))
          else Seq()
        }
      } yield notice
    }

  // the course's enrollment: its students, as the dropbox has their keys
  // (as Course.publish_enrollment). Left as it is if the dropbox has no
  // directory for the course: that would unenroll everyone.
  lazy val enrollment: CourseName => Target[WithData[Publish]] = fun {
    (c: CourseName) =>
      publisher(
        subs.enrollment_repo(c),
        "enrolling students",
        Some(Create("empty", Seq(staff(c)), Seq()))
      )((keys = dropbox.course_keys(c))) { (producer, deps, dir) =>
        deps.keys match {
          case Some(keys) =>
            os.write.over(
              dir / "enrollment.json",
              upickle.default.write(keys, indent = 2)
            )
          case None =>
            producer.log(
              s"no dropbox directory for $c: enrollment left alone\n"
            )
        }
      }
  }

  // the staff's grading repo (see repos.GradesRepo)
  def grades(c: CourseName): Target[WithData[Publish]] =
    repos.GradesRepo(c, env).latest

  // a student's repo for a project (see repos.WorkRepo)
  def work_repo(
      c: CourseName,
      p: ProjectName,
      csid: CSID
  ): Target[WithData[Publish]] =
    repos.WorkRepo(c, p, csid, env).latest

  // the course's report site (see repos.SiteRepo)
  def site(c: CourseName): Target[WithData[Publish]] =
    repos.SiteRepo(c, env).latest
}
