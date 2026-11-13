package ag.grader

import ag.common.{down, given_ReadWriter_SortedMap, given_ReadWriter_SortedSet}
import ag.r2.shout
import ag.r3.{Producer, Scope, Target, Tree, WithData, create_data}
import ag.rules.run

import scala.collection.{SortedMap, SortedSet}
import scala.util.control.NonFatal

// The project's tests, and running them on the students' submissions, as r3
// targets on the local mirror: the r3 form of the tests and run_one in
// Project. Nothing is published from here: the tests and aliases are worked
// out from what's on the mirror (the outbox is to publish them).
//
// A test runs in a scratch directory of its own, a fresh copy of the
// prepared submission with the test's files added, so runs never share a
// directory.
final class TestRuns(val subs: Submissions) extends Scope("test_runs") {
  private val defs = subs.defs
  private val source = defs.source

  // ---- aliases ----------------------------------------------------------

  // The project's aliases as they should be: the published ones, and a new
  // one for each student with a submission who has none, numbered on from
  // the highest (as Project.publish_aliases).
  lazy val aliases: (String, String) => Target[SortedMap[CSID, Alias]] = fun {
    (c: String, p: String) =>
      target(
        (
          published = subs.aliases(c, p),
          csids = subs.students_with_submission(c, p)
        )
      ) { (_, deps) =>
        val fresh = deps.csids.toSeq.filterNot(deps.published.contains)
        val next = deps.published.values.foldLeft(0)(_ max _.value) + 1
        deps.published ++ fresh.zipWithIndex.map((csid, i) =>
          (csid, Alias(next + i))
        )
      }
  }

  // ---- the tests --------------------------------------------------------

  // a name's base and extension, as os.Path's baseName and ext
  private def split(name: String): (String, String) =
    name.lastIndexOf('.') match {
      case -1 => (name, "")
      case i  => (name.take(i), name.drop(i + 1))
    }

  // the override repo's tests: the names it has a file (or directory) of
  // for every test extension
  lazy val override_test_names: (String, String) => Target[SortedSet[String]] =
    fun { (c: String, p: String) =>
      target(
        (
          ov = source.master(subs.override_repo(c, p)),
          ext = defs.test_extensions(c, p)
        )
      ) { (producer, deps) =>
        val found = for {
          o <- deps.ov.toSeq
          n <- producer.list(producer.commit(o).tree).getOrElse(Seq())
          if !n.startsWith(".")
          (base, ext) = split(n)
          if deps.ext.contains(ext)
        } yield (base, ext)
        found
          .groupBy(_._1)
          .filter(_._2.size == deps.ext.size)
          .keySet
          .to(SortedSet)
      }
    }

  // Every test, and the tree with its files, named <internal name>.<ext>:
  // each student's own (their alias outside, their CSID inside) and the
  // override repo's (as Project.publish_tests: one of those wins over a
  // student's with the same id)
  lazy val tests: (String, String) => Target[SortedMap[TestId, Tree]] = fun {
    (c: String, p: String) =>
      target(
        (
          students = subs.student_tests_by_csid(c, p),
          aliases = aliases(c, p),
          names = override_test_names(c, p),
          ov = source.master(subs.override_repo(c, p))
        )
      ) { (producer, deps) =>
        val theirs = for {
          (csid, t) <- deps.students.toSeq
          alias <- deps.aliases.get(csid).toSeq
        } yield (TestId(alias.toString, csid.value), t.data)
        val ours = for {
          o <- deps.ov.toSeq
          tree = producer.commit(o).tree
          n <- deps.names.toSeq
        } yield (TestId(n, n), tree)
        (theirs ++ ours).to(SortedMap)
      }
  }

  lazy val test_ids: (String, String) => Target[SortedSet[TestId]] = fun {
    (c: String, p: String) =>
      target((tests = tests(c, p)))((_, deps) => deps.tests.keySet)
  }

  private def named(ids: SortedSet[TestId], names: SortedSet[String]) =
    ids.filter(t =>
      names.contains(t.external_name) || names.contains(t.internal_name)
    )

  lazy val phase1_test_ids: (String, String) => Target[SortedSet[TestId]] =
    fun { (c: String, p: String) =>
      target((ids = test_ids(c, p), chosen = defs.phase1_tests(c, p))) {
        (_, deps) => named(deps.ids, deps.chosen)
      }
    }

  lazy val phase2_test_ids: (String, String) => Target[SortedSet[TestId]] =
    fun { (c: String, p: String) =>
      target((ids = test_ids(c, p), chosen = defs.phase2_tests(c, p))) {
        (_, deps) => named(deps.ids, deps.chosen)
      }
    }

  // all but the tests marked ignored, by external or internal name
  lazy val good_test_ids: (String, String) => Target[SortedSet[TestId]] =
    fun { (c: String, p: String) =>
      target((ids = test_ids(c, p), bad = defs.bad_tests(c, p))) { (_, deps) =>
        deps.ids.filterNot(named(deps.ids, deps.bad).contains)
      }
    }

  // the tests the correctness score counts: (phase 1 + phase 2) - ignored
  lazy val chosen_test_ids: (String, String) => Target[SortedSet[TestId]] =
    fun { (c: String, p: String) =>
      target(
        (
          one = phase1_test_ids(c, p),
          two = phase2_test_ids(c, p),
          good = good_test_ids(c, p)
        )
      ) { (_, deps) => (deps.one ++ deps.two).intersect(deps.good) }
    }

  // each phase's chosen tests, less the ignored ones
  private lazy val good_phase1_test_ids
      : (String, String) => Target[SortedSet[TestId]] = fun {
    (c: String, p: String) =>
      target((a = phase1_test_ids(c, p), b = good_test_ids(c, p))) {
        (_, deps) => deps.a.intersect(deps.b)
      }
  }
  private lazy val good_phase2_test_ids
      : (String, String) => Target[SortedSet[TestId]] = fun {
    (c: String, p: String) =>
      target((a = phase2_test_ids(c, p), b = good_test_ids(c, p))) {
        (_, deps) => deps.a.intersect(deps.b)
      }
  }

  def test_ids_for(
      c: String,
      p: String,
      tests: TestSet
  ): Target[SortedSet[TestId]] = tests match {
    case "all"    => test_ids(c, p)
    case "good"   => good_test_ids(c, p)
    case "chosen" => chosen_test_ids(c, p)
    case "phase1" => good_phase1_test_ids(c, p)
    case "phase2" => good_phase2_test_ids(c, p)
  }

  // Where `path` in `tree` leads, its links followed (as Project.copy_test's
  // os.followLink: tests share files that way, t1.dir -> t0.dir); None if
  // it leads out of the tree, or round in circles.
  private def follow(
      ctx: Producer[?],
      tree: Tree,
      path: os.RelPath,
      hops: Int = 40
  ): Option[os.RelPath] =
    ctx.link_target(tree, path) match {
      case None                         => Some(path)
      case Some(_) if hops == 0         => None
      case Some(t) if t.startsWith("/") => None
      case Some(t)                      =>
        val segments = t
          .split('/')
          .filter(s => s.nonEmpty && s != ".")
          .foldLeft(Option(path.segments.dropRight(1).toList)) {
            case (Some(at), "..") => Option.when(at.nonEmpty)(at.dropRight(1))
            case (Some(at), s)    => Some(at :+ s)
            case (None, _)        => None
          }
        segments.flatMap(ss =>
          follow(ctx, tree, os.RelPath(ss.mkString("/")), hops - 1)
        )
    }

  // A test's files, named <external name>.<ext>, as the Makefile runs them,
  // links to other files in the tests followed. Fails if there's no such
  // test.
  lazy val test_info: (String, String, TestId) => Target[WithData[Unit]] =
    fun { (c: String, p: String, id: TestId) =>
      target((tests = tests(c, p), ext = defs.test_extensions(c, p))) {
        (producer, deps) =>
          val tree = deps.tests.getOrElse(
            id,
            throw IllegalArgumentException(
              s"no test ${id.external_name} in ${c}_$p"
            )
          )
          create_data(producer) { dir =>
            for (e <- deps.ext) {
              for {
                at <- follow(
                  producer,
                  tree,
                  os.RelPath(s"${id.internal_name}.$e")
                )
              } {
                val _ =
                  producer.copy_path(tree, at, dir / s"${id.external_name}.$e")
              }
            }
          }
      }
    }

  // ---- running them -----------------------------------------------------

  // The image the project's tests run in, if it has a docker file: named by
  // the file's contents, which are read on every run (the file is in the
  // grader's directory, not on the mirror). It's built when a test needs it.
  lazy val docker_image: (String, String) => Target[Option[String]] = fun {
    (c: String, p: String) =>
      complex_target[Option[String]] { tracker =>
        val image = tracker
          .track(defs.docker_file(c, p))
          .map(f => tracker.track(docker_image_of(f)))
        tracker.run_if_needed(_ => image)
      }
  }

  // no dependencies: it runs every time
  private lazy val docker_image_of: String => Target[String] = fun {
    (f: String) =>
      target(EmptyTuple)((_, _) => Docker.image_name(os.pwd / os.RelPath(f)))
  }

  // Runs a test on a commit's prepared submission (as Project.run_one): the
  // outcome of each run so far, the latest run's output files as its data.
  // It runs again while it has fewer than `n` runs, but stops at a failure
  // unless `keep_going`; and from scratch when what it depends on changes.
  def run_one(
      keep_going: Boolean,
      n: Int
  ): (String, String, CSID, String, TestId) => Target[WithData[Outcome]] =
    fun { (c: String, p: String, csid: CSID, sha: String, id: TestId) =>
      complex_target[WithData[Outcome]] { tracker =>
        val deps = tracker.track_all(
          (
            test = test_info(c, p, id),
            prepared = subs.prepare(c, p, csid, sha),
            cores = defs.cores(c, p),
            docker_file = defs.docker_file(c, p),
            image = docker_image(c, p)
          )
        )
        tracker.run { (producer, saved) =>
          val history = saved.toSeq.flatMap(_.value.value.outcomes)
          saved match {
            case s @ Some(_)
                if history.size >= n || !(keep_going || history.forall(
                  _._1 == OutcomeStatus.pass
                )) =>
              s
            case _ =>
              () =>
                create_data(producer) { out =>
                  val res = run_test(
                    producer,
                    s"${c}_${p}_${csid.value}",
                    id,
                    deps.prepared.data,
                    deps.test.data,
                    deps.cores,
                    deps.docker_file,
                    history.size + 1,
                    out
                  )
                  Outcome(
                    project = Project(Course(c), p),
                    csid = csid,
                    test_id = id,
                    outcomes = history :+ res,
                    commit_id = Some(sha)
                  )
                }
          }
        }
      }
    }

  // a run's directory, gone: whatever the test left in it (read-only files
  // or directories too); a failure to is only logged
  private def remove(work: os.Path): Unit =
    try {
      val _ = os.proc("chmod", "-R", "u+rwx", work).call(check = false)
      os.remove.all(work)
    } catch {
      case NonFatal(e) =>
        shout(s"can't remove ${work.toString}: ${e.getMessage}")
    }

  // the files a run leaves that are kept, by extension
  private val kept =
    Seq("ok", "out", "err", "result", "time", "stdout", "stderr")

  // One run (as doit): a copy of the prepared submission, less anything
  // named like the test (a student's own <test>.result, say), and the test's
  // files; then `make -k clean` and `make -k <test>.test` there. Its output
  // files go to `out`.
  private def run_test(
      producer: Producer[?],
      what: String,
      id: TestId,
      prepared: Tree,
      test: Tree,
      cores: Int,
      docker_file: Option[String],
      n: Int,
      out: os.Path
  ): (OutcomeStatus, Option[Double]) = {
    ag.rules.refuse_if_read_only(
      s"run ${id.external_name} on $what (${n.toString})"
    )
    val _ = started_runs.incrementAndGet()
    try {
      if (cores > limit)
        throw Exception(s"need $cores cores, limit is $limit")
      governor.down(cores) {
        val root = producer.state.scratch_root
        os.makeDir.all(root)
        val work = os.temp.dir(dir = root, deleteOnExit = false)
        try {
          producer.copy_to(prepared, work)
          val tn = id.external_name
          for (f <- os.list(work) if f.baseName == tn) os.remove.all(f)
          producer.copy_to(test, work)
          val image = docker_file.map(f => Docker.of(os.pwd / os.RelPath(f)))

          val m = s"$tn/${id.internal_name} for $what"
          shout(f"running#$n $m on $cores cores (Docker: ${image.toString})")

          val start = System.currentTimeMillis()
          val _ = os.proc("make", "-k", "clean").run(cwd = work, check = false)
          val (_, stdout, stderr) = os
            .proc(Docker.dockerCommand(work, image), "make", "-k", s"$tn.test")
            .run(cwd = work, check = false)
          val run_time =
            Some((System.currentTimeMillis() - start).toDouble / 1000)

          def first_line(ext: String): Option[String] = {
            val f = work / s"$tn.$ext"
            if (os.isFile(f)) os.read.lines(f).headOption else None
          }
          val result = first_line("result")
          val time = first_line("time")
          val outcome = (
            result.map(_.toLowerCase.nn),
            time.map(_.toLowerCase.nn)
          ) match {
            case (_, Some("timeout")) => OutcomeStatus.timeout
            case (Some("pass"), _)    => OutcomeStatus.pass
            case (Some("fail"), _)    => OutcomeStatus.fail
            case (_, _)               => OutcomeStatus.unknown
          }

          val how_long = run_time.map(t => f"$t%.2f")
          val shown = (if (outcome.isHappy) fansi.Color.Green
                       else fansi.Color.Red) (outcome.toString)
          shout(
            s"    [${finished_runs.get().toString}/${started_runs.get().toString}] finished [${shown.toString}] $m in ${how_long.toString} seconds"
          )

          // make's own output (not the test's .out/.err), when there is
          // some, its first 10K: stdout only if the test didn't pass
          val max_std_size = 10 * 1024
          val kept_stdout = if (outcome.isHappy) None else Some(stdout)
          for {
            (f, ext) <- Seq(kept_stdout -> "stdout", stderr -> "stderr")
            f <- f
            size = os.size(f)
            if size > 0
          } os.write.over(
            out / s"$tn.$ext",
            os.read.bytes(f, 0, math.min(size, max_std_size.toLong).toInt),
            createFolders = true
          )
          for {
            ext <- kept
            src = work / s"$tn.$ext"
            if os.isFile(src)
          } os.copy.over(
            from = src,
            to = out / s"$tn.$ext",
            followLinks = false,
            createFolders = true
          )

          (outcome, Project.parseTime(time).orElse(run_time))
        } finally remove(work)
      }
    } finally { val _ = finished_runs.incrementAndGet() }
  }
}
