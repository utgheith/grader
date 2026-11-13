package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.{Many, Producer, Scope, Target, Tree, WithData, create_data}
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
// makes the repo if needed (it can't without `create`) and pushes it.
@upickle.implicits.allowUnknownKeys(false)
final case class Publish(
    repo: String,
    exists: Boolean,
    base: Option[String],
    message: String,
    create: Option[Create]
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

  private def staff(c: String): String = s"@${c}_staff"

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
          f(producer, values, dir)
          Publish(repo, exists, base.map(_.id), message, create)
        }
      }
    }

  // A repo that's only ever made, never changed (e.g. a student's): its
  // data is the tree it has or would start with, as is. No copy is made, so
  // nothing in it is touched (a copy can't keep everything: submodules, say).
  private def creator(
      repo: String,
      message: String,
      create: Create
  )(using sourcecode.FullName): Target[WithData[Publish]] =
    complex_target[WithData[Publish]] { tracker =>
      val exists = tracker.track(source.exists(repo))
      val base =
        tracker.track(source.master(if (exists) repo else create.fork_from))
      tracker.run_if_needed { producer =>
        WithData(
          Publish(repo, exists, base.map(_.id), message, Some(create)),
          base.fold(Tree(Outbox.empty_tree))(b => producer.commit(b).tree)
        )
      }
    }

  // everything in `dir` but its dot files (e.g. .initialized)
  private def clear(dir: os.Path): Unit =
    for (f <- os.list(dir) if !f.last.startsWith(".")) os.remove.all(f)

  // the project's aliases (as Project.publish_aliases)
  lazy val aliases: (String, String) => Target[WithData[Publish]] = fun {
    (c: String, p: String) =>
      publisher(
        subs.aliases_repo(c, p),
        "more aliases",
        Some(Create("empty", Seq(staff(c)), Seq()))
      )((aliases = runs.aliases(c, p))) { (_, deps, dir) =>
        os.write.over(
          dir / "aliases.json",
          upickle.default.write(deps.aliases, indent = 2)
        )
      }
  }

  // The override repo, which the course's staff keep: made once from the
  // project repo's tests and Makefiles (as Project.publish_override_repo),
  // then left to them
  lazy val override_repo: (String, String) => Target[WithData[Publish]] = fun {
    (c: String, p: String) =>
      publisher(
        subs.override_repo(c, p),
        "update override",
        Some(Create("empty", Seq("@all"), Seq(staff(c))))
      )(
        (
          project = source.master(subs.project_repo(c, p)),
          ext = defs.test_extensions(c, p)
        )
      ) { (producer, deps, dir) =>
        val marker = dir / ".initialized"
        if (!os.exists(marker)) {
          for (m <- deps.project) {
            val from = producer.checkout(producer.commit(m).tree)
            def add(f: os.Path): Unit = {
              val to = dir / f.relativeTo(from)
              if (!os.exists(to, followLinks = false))
                os.copy(
                  from = f,
                  to = to,
                  followLinks = false,
                  createFolders = true,
                  copyAttributes = true
                )
            }
            // its tests
            for {
              f <- os.list(from)
              if !f.last.startsWith(".")
              if deps.ext.contains(f.ext)
            } add(f)
            // its Makefiles, anywhere
            os.walk
              .stream(from, followLinks = false)
              .foreach(f =>
                if (os.isFile(f) && automatic_overrides.contains(f.last)) add(f)
              )
          }
          os.write(marker, "")
        }
      }
  }

  private val automatic_overrides = Set("Makefile", "makefile")

  // the students' tests, by alias, at <alias>.<ext>
  private def copy_student_tests(
      producer: Producer[?],
      tests: scala.collection.SortedMap[CSID, WithData[String]],
      aliases: scala.collection.SortedMap[CSID, Alias],
      ext: Iterable[String],
      dir: os.Path
  ): Unit =
    for {
      (csid, t) <- tests.toSeq
      alias <- aliases.get(csid).toSeq
      e <- ext
    } {
      val _ = producer.copy_path(
        t.data,
        os.RelPath(s"${csid.value}.$e"),
        dir / s"${alias.toString}.$e"
      )
    }

  // the students' own tests, for the staff (as
  // Project.publish_submitted_tests)
  lazy val submitted_tests: (String, String) => Target[WithData[Publish]] =
    fun { (c: String, p: String) =>
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
        copy_student_tests(producer, deps.tests, deps.aliases, deps.ext, dir)
      }
    }

  // every test, for everyone: the students' and the override repo's (as
  // Project.publish_tests)
  lazy val tests: (String, String) => Target[WithData[Publish]] = fun {
    (c: String, p: String) =>
      publisher(
        s"${subs.project_repo(c, p)}__tests",
        "tests",
        Some(Create("empty", Seq("@all"), Seq()))
      )(
        (
          tests = subs.student_tests_by_csid(c, p),
          aliases = runs.aliases(c, p),
          ext = defs.test_extensions(c, p),
          names = runs.override_test_names(c, p),
          ov = source.master(subs.override_repo(c, p))
        )
      ) { (producer, deps, dir) =>
        clear(dir)
        copy_student_tests(producer, deps.tests, deps.aliases, deps.ext, dir)
        for {
          o <- deps.ov.toSeq
          tree = producer.commit(o).tree
          n <- deps.names.toSeq
          e <- deps.ext
        } {
          val _ = producer.copy_path(
            tree,
            os.RelPath(s"$n.$e"),
            dir / s"$n.$e"
          )
        }
      }
  }

  // what a project publishes, in the order to sync it: the override repo
  // first, since the tests come from it
  def all(c: String, p: String): Seq[Target[WithData[Publish]]] =
    Seq(override_repo(c, p), aliases(c, p), submitted_tests(c, p), tests(c, p))

  // ---- results ----------------------------------------------------------

  // the files a run leaves that are published with a student's results, by
  // extension (as Project.copy_results)
  private val result_files =
    Seq("ok", "out", "err", "result", "time", "stdout", "stderr")

  // A student's latest results, with each test's output, in their own
  // results repo (as Project.publish_student_results), graded to `n` tries
  lazy val student_results: (String, String, CSID, Int) => Target[
    WithData[Publish]
  ] = fun { (c: String, p: String, csid: CSID, n: Int) =>
    publisher(
      subs.student_results_repo(c, p, csid),
      "updated results",
      Some(Create("empty", Seq(csid.value, staff(c)), Seq()))
    )(
      (
        results = grading.student_results(c, p, csid, Phase.Latest, n),
        outcomes = grading.outcomes(c, p, csid, Phase.Latest, n)
      )
    ) { (producer, deps, dir) =>
      clear(dir)
      for {
        o <- deps.outcomes
        ext <- result_files
      } {
        val name = s"${o.value.test_id.external_name}.$ext"
        val _ = producer.copy_file(o.data, os.RelPath(name), dir / name)
      }
      for (r <- deps.results)
        os.write.over(dir / "the_results.json", upickle.default.write(r))
    }
  }

  // Every student's results, by alias, for each phase the project publishes
  // (Phase.results_file), for the report (as Project.publish_results). One
  // publisher for every phase's file: a repo has one thing pending at a time.
  lazy val results: (String, String, Int) => Target[WithData[Publish]] = fun {
    (c: String, p: String, n: Int) =>
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
  // with a submission): the results repo, and each student's own
  def graded(
      c: String,
      p: String,
      students: Iterable[CSID],
      n: Int
  ): Seq[Target[WithData[Publish]]] =
    results(c, p, n) +: students.toSeq.map(student_results(c, p, _, n))

  // ---- the courses' repos ----------------------------------------------

  // every active course's students' keys (a later course's key for the same
  // student wins)
  private lazy val all_keys: Target[SortedMap[CSID, String]] =
    complex_target[SortedMap[CSID, String]] { tracker =>
      val courses = tracker.track(defs.active_courses).toSeq
      val keys = tracker.track_all(courses.map(dropbox.keys))
      tracker.run_if_needed(_ =>
        keys.foldLeft(SortedMap[CSID, String]())(_ ++ _)
      )
    }

  // The students' keys in gitolite-admin, as they are in the dropbox: added
  // or changed, never removed (as Course.publish_keys, for every course at
  // once, since they share the repo). It's never made.
  lazy val keys: Target[WithData[Publish]] =
    publisher("gitolite-admin", "updated keys", None)((keys = all_keys)) {
      (_, deps, dir) =>
        for ((csid, key) <- deps.keys) {
          val f = dir / "keydir" / s"${csid.value}.pub"
          if (!os.exists(f) || os.read(f).trim.nn != key)
            os.write.over(f, key, createFolders = true)
        }
    }

  // the course's enrollment: its students, as the dropbox has their keys
  // (as Course.publish_enrollment)
  lazy val enrollment: String => Target[WithData[Publish]] = fun {
    (c: String) =>
      publisher(
        subs.enrollment_repo(c),
        "enrolling students",
        Some(Create("empty", Seq(staff(c)), Seq()))
      )((keys = dropbox.keys(c))) { (_, deps, dir) =>
        os.write.over(
          dir / "enrollment.json",
          upickle.default.write(deps.keys, indent = 2)
        )
      }
  }

  // the staff's grading repo: made with a README, then theirs (as
  // Course.create_grades_repo)
  lazy val grades: String => Target[WithData[Publish]] = fun { (c: String) =>
    val repo = s"${c}__grades"
    publisher(
      repo,
      "creating grades repo",
      Some(Create("empty", Seq(staff(c)), Seq(staff(c))))
    )((exists = source.exists(repo))) { (_, deps, dir) =>
      if (!deps.exists)
        os.write.over(
          dir / "README.md",
          f"""
                 |# Grading data for `$c`
                 |
                 |This is a per-course workspace for synchronizing grading data
                 |used by the grading scripts between TAs. The grader itself
                 |automatically creates this repo, but does not otherwise use
                 |it; that may change in the future.
                 |""".stripMargin
        )
    }
  }

  // A student's repo for a project: made by forking the project's repo,
  // then theirs, never touched (as Project.publish_work_repo, without its
  // email)
  lazy val work_repo: (String, String, CSID) => Target[WithData[Publish]] =
    fun { (c: String, p: String, csid: CSID) =>
      creator(
        subs.work_repo(c, p, csid),
        "create",
        Create(
          subs.project_repo(c, p),
          Seq(csid.value, staff(c)),
          Seq(csid.value)
        )
      )
    }

  // The report pages of the course's active projects, and the front page
  // listing every project's page in the repo (as Course.publish_site): a
  // page is rewritten only when what it shows changes, and pages of
  // projects no longer active stay.
  lazy val site: String => Target[WithData[Publish]] = fun { (c: String) =>
    publisher(
      s"${c}__site",
      "update site",
      Some(Create("empty", Seq("@all"), Seq()))
    )((inputs = grading.site_inputs(c))) { (producer, deps, dir) =>
      val now = java.time.LocalDateTime.now().nn
      for {
        i <- deps.inputs
        data <- HtmlGen.reports(i, now, why => producer.log(why + "\n"))
      } {
        val _ = HtmlGen.write_page(dir, data)
      }
      val pages = os
        .list(dir)
        .map(_.last)
        .filter(f =>
          f.endsWith(".html") && f != "index.html" && !HtmlGen.is_phase_page(f)
        )
      os.write.over(dir / "index.html", HtmlGen.render_index(c, pages))
    }
  }
}
