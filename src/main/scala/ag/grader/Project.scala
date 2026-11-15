package ag.grader

import ag.common.{
  NoNulls,
  ToRelPath,
  given_ReadWriter_SortedMap,
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedSet
}
import ag.grader.repos.{CoursesConfig, Env, Instance, ProjectRepo}
import ag.rules.{
  Commit,
  Context,
  Producer,
  Target,
  Tree,
  WithData,
  create_data,
  periodic
}

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import scala.collection.{SortedMap, SortedSet}
import scala.util.Try

// A project's targets: its settings, from courses_config (see
// CoursesConfig), which fail if courses.json doesn't have the project
// (`find` says if it's there), and what's worked out from them.
final case class Project(c: CourseName, p: ProjectName, env: Env)
    extends Instance[Project] derives ToRelPath {

  lazy val find: Target[Option[RawProject]] =
    target((cs = CoursesConfig(env).courses))((_, deps) =>
      deps.cs.get(c).flatMap(_.projects.get(p.value))
    )

  private lazy val project: Target[RawProject] =
    target((rp = find))((_, deps) =>
      deps.rp.getOrElse(throw Exception(s"no project $c/$p in courses_config"))
    )

  // the course and the project are active, and the project's repo is there
  lazy val active: Target[Boolean] =
    target(
      (
        course = Course(c, env).active,
        rp = project,
        repo = ProjectRepo(c, p, env).has_master
      )
    )((_, deps) => deps.course && deps.rp.active && deps.repo)

  lazy val phase1_tests: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.phase1_tests.to(SortedSet))
  lazy val phase1_weight: Target[Int] =
    target((rp = project))((_, d) => d.rp.phase1_weight)
  lazy val phase2_tests: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.phase2_tests.to(SortedSet))
  lazy val phase2_weight: Target[Int] =
    target((rp = project))((_, d) => d.rp.phase2_weight)
  lazy val test_weights: Target[Seq[Weight]] =
    target((rp = project))((_, d) => d.rp.test_weights)
  lazy val bad_tests: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.bad_tests.to(SortedSet))
  lazy val cores: Target[Int] =
    target((rp = project))((_, d) => d.rp.cores)
  lazy val iterations: Target[Iterations] =
    target((rp = project))((_, d) => d.rp.iterations)
  lazy val test_cutoff: Target[LocalDateTime] =
    target((rp = project))((_, d) => d.rp.test_cutoff)
  lazy val code_cutoff: Target[LocalDateTime] =
    target((rp = project))((_, d) => d.rp.code_cutoff)
  lazy val extensions: Target[Extensions] =
    target((rp = project))((_, d) => d.rp.extensions)
  lazy val test_extensions: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.test_extensions.to(SortedSet))
  // the name of its docker file (building the image is grading's business)
  lazy val docker_file: Target[Option[String]] =
    target((rp = project))((_, d) => d.rp.docker_file)
  lazy val staff: Target[SortedSet[CSID]] =
    target((rp = project))((_, d) => d.rp.staff.to(SortedSet))

  // a student's deadline for grading their code in a phase, with any
  // extension they have (as Project.deadline)
  lazy val deadline: (CSID, Phase) => Target[LocalDateTime] = fun {
    (csid: CSID, phase: Phase) =>
      target(
        (first = test_cutoff, second = code_cutoff, ext = extensions)
      ) { (_, deps) =>
        phase
          .deadline(deps.first, deps.second)
          .getOrElse(deps.second)
          .plusDays(deps.ext.days(csid, phase))
          .nn
      }
  }

  // ---- submissions, and what the grader published, read back -------

  // the enrolled students' submissions
  lazy val submissions: Target[SortedMap[CSID, Commit]] =
    complex_target[SortedMap[CSID, Commit]] { tracker =>
      val csids = tracker.track(Course(c, env).enrolled_csids).toSeq
      val subs =
        tracker.track_all(
          csids.map(csid => Submission(c, p, csid, env).submission)
        )
      tracker.run_if_needed(_ =>
        csids
          .zip(subs)
          .collect { case (csid, Some(s)) => (csid, s) }
          .to(SortedMap)
      )
    }

  lazy val students_with_submission: Target[SortedSet[CSID]] =
    target((s = submissions))((_, deps) => deps.s.keySet)

  lazy val student_tests_by_csid: Target[SortedMap[CSID, WithData[String]]] =
    complex_target[SortedMap[CSID, WithData[String]]] { tracker =>
      val csids = tracker.track(students_with_submission).toSeq
      val tests =
        tracker.track_all(csids.map(csid => Submission(c, p, csid, env).test))
      tracker.run_if_needed(_ =>
        (for {
          (csid, t) <- csids.zip(tests)
          rev <- t.value.toSeq
        } yield (csid, WithData(rev, t.data))).to(SortedMap)
      )
    }

  lazy val csids_with_tests: Target[SortedSet[CSID]] =
    target((t = student_tests_by_csid))((_, deps) => deps.t.keySet)

  // The project's aliases as they should be: the published ones, and a new
  // one for each student with a submission who has none, numbered on from
  // the highest (as Project.publish_aliases).
  lazy val aliases: Target[SortedMap[CSID, Alias]] =
    target(
      (published = published_aliases, csids = students_with_submission)
    ) { (_, deps) =>
      val fresh = deps.csids.toSeq.filterNot(deps.published.contains)
      val next = deps.published.values.foldLeft(0)(_ max _.value) + 1
      deps.published ++ fresh.zipWithIndex.map((csid, i) =>
        (csid, Alias(next + i))
      )
    }

  // the project's aliases (none if the repo or file isn't there, or can't
  // be read)
  lazy val published_aliases: Target[SortedMap[CSID, Alias]] =
    target((m = env.source.master(repos.AliasesRepo.name_of(c, p)))) {
      (producer, deps) =>
        Project
          .text_on(producer, deps.m, "aliases.json")
          .flatMap(json =>
            Try(upickle.default.read[SortedMap[CSID, Alias]](json)).toOption
          )
          .getOrElse(SortedMap[CSID, Alias]())
    }

  lazy val results_readable: Phase => Target[Boolean] = fun { (phase: Phase) =>
    target((m = env.source.master(repos.ResultsRepo.name_of(c, p)))) {
      (producer, deps) =>
        Project.readable[SortedMap[Alias, RedactedStudentResults]](
          Project.text_on(producer, deps.m, phase.results_file),
          phase.results_file
        )
    }
  }

  // A phase's published results, by alias (as Project.results): Right(None)
  // if there are none yet, Left(why) if they can't be read by this version
  // of the grader
  lazy val published_results: Phase => Target[
    Either[String, Option[SortedMap[Alias, RedactedStudentResults]]]
  ] = fun { (phase: Phase) =>
    target((m = env.source.master(repos.ResultsRepo.name_of(c, p)))) {
      (producer, deps) =>
        val out: Either[
          String,
          Option[SortedMap[Alias, RedactedStudentResults]]
        ] = Project.text_on(producer, deps.m, phase.results_file) match {
          case None       => Right(None)
          case Some(json) =>
            Try(
              NoNulls.check(
                upickle.default
                  .read[SortedMap[Alias, RedactedStudentResults]](json),
                phase.results_file
              )
            ).toEither.left
              .map(e =>
                s"${repos.ResultsRepo.name_of(c, p)}'s ${phase.results_file} isn't readable by this version " +
                  s"of the grader; `run grade -c $c -p $p` rewrites it (${String.valueOf(e.getMessage)})"
              )
              .map(Some(_))
        }
        out
    }
  }

  // ---- the tests -----------------------------------------------------

  // the override repo's tests: the names it has a file (or directory) of
  // for every test extension
  lazy val override_test_names: Target[SortedSet[String]] =
    target(
      (
        ov = env.source.master(repos.OverrideRepo.name_of(c, p)),
        ext = test_extensions
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

  // Every test, and the tree with its files, named <internal name>.<ext>:
  // each student's own (their alias outside, their CSID inside) and the
  // override repo's (as Project.publish_tests: one of those wins over a
  // student's with the same id)
  lazy val tests: Target[SortedMap[TestId, Tree]] =
    target(
      (
        students = student_tests_by_csid,
        aliases = this.aliases,
        names = override_test_names,
        ov = env.source.master(repos.OverrideRepo.name_of(c, p))
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

  lazy val test_ids: Target[SortedSet[TestId]] =
    target((tests = tests))((_, deps) => deps.tests.keySet)

  lazy val phase1_test_ids: Target[SortedSet[TestId]] =
    target((ids = test_ids, chosen = phase1_tests)) { (_, deps) =>
      named(deps.ids, deps.chosen)
    }

  lazy val phase2_test_ids: Target[SortedSet[TestId]] =
    target((ids = test_ids, chosen = phase2_tests)) { (_, deps) =>
      named(deps.ids, deps.chosen)
    }

  // all but the tests marked ignored, by external or internal name
  lazy val good_test_ids: Target[SortedSet[TestId]] =
    target((ids = test_ids, bad = bad_tests)) { (_, deps) =>
      deps.ids.filterNot(named(deps.ids, deps.bad).contains)
    }

  // the tests the correctness score counts: (phase 1 + phase 2) - ignored
  lazy val chosen_test_ids: Target[SortedSet[TestId]] =
    target(
      (
        one = phase1_test_ids,
        two = phase2_test_ids,
        good = good_test_ids
      )
    ) { (_, deps) => (deps.one ++ deps.two).intersect(deps.good) }

  // each phase's chosen tests, less the ignored ones
  private lazy val good_phase1_test_ids: Target[SortedSet[TestId]] =
    target((a = phase1_test_ids, b = good_test_ids)) { (_, deps) =>
      deps.a.intersect(deps.b)
    }

  private lazy val good_phase2_test_ids: Target[SortedSet[TestId]] =
    target((a = phase2_test_ids, b = good_test_ids)) { (_, deps) =>
      deps.a.intersect(deps.b)
    }

  def test_ids_for(tests: TestSet): Target[SortedSet[TestId]] = tests match {
    case "all"    => test_ids
    case "good"   => good_test_ids
    case "chosen" => chosen_test_ids
    case "phase1" => good_phase1_test_ids
    case "phase2" => good_phase2_test_ids
  }
  // A test's files, named <external name>.<ext>, as the Makefile runs them,
  // links to other files in the tests followed. Fails if there's no such
  // test.
  lazy val test_info: TestId => Target[WithData[Unit]] = fun { (id: TestId) =>
    target((tests = tests, ext = test_extensions)) { (producer, deps) =>
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

  // The image the project's tests run in, if it has a docker file: named by
  // the file's contents, which are read on every run (the file is in the
  // grader's directory, not on the mirror). It's built when a test needs it.
  lazy val docker_image: Target[Option[String]] =
    complex_target[Option[String]] { tracker =>
      val image = tracker
        .track(docker_file)
        .map(f => tracker.track(DockerImage(f, env).image))
      tracker.run_if_needed(_ => image)
    }

  // ---- grading -------------------------------------------------------

  // Where the project is relative to its deadlines (as Project.state):
  // worked out again every minute as time passes; what depends on it reruns
  // only when it changes.
  lazy val state: Target[ProjectState] =
    target(
      (
        first = test_cutoff,
        second = code_cutoff,
        clock = periodic(60 * 1000L)
      )
    ) { (_, deps) =>
      ProjectState.at(LocalDateTime.now().nn, deps.first, deps.second)
    }

  // Every student's results for `phase`, by alias, as published (as
  // Project.publish_results): late is from each student's own deadline for
  // the phase; students without an alias are left out.
  lazy val phase_results
      : (Phase, Int) => Target[SortedMap[Alias, RedactedStudentResults]] = fun {
    (phase: Phase, n: Int) =>
      complex_target[SortedMap[Alias, RedactedStudentResults]] { tracker =>
        val students = tracker.track(students_with_submission).toSeq
        val results =
          tracker.track_all(
            students.map(csid => Submission(c, p, csid, env).results(phase, n))
          )
        val deadlines =
          tracker.track_all(students.map(deadline(_, phase)))
        val aliases =
          tracker.track_all(
            students.map(csid => Submission(c, p, csid, env).alias)
          )
        tracker.run_if_needed { _ =>
          (for {
            ((r, d), a) <- results.zip(deadlines).zip(aliases)
            r <- r.toSeq
            a <- a.toSeq
          } yield a -> r.redacted(ZonedDateTime.of(d, ZoneId.systemDefault).nn))
            .to(SortedMap)
        }
      }
  }

  // The results files of the phases the project publishes now (see
  // ProjectState.publishes), by file name (Phase.results_file)
  lazy val results_files: Int => Target[
    SortedMap[String, SortedMap[Alias, RedactedStudentResults]]
  ] = fun { (n: Int) =>
    complex_target[
      SortedMap[String, SortedMap[Alias, RedactedStudentResults]]
    ] { tracker =>
      val st = tracker.track(state)
      val phases =
        Seq(Phase.Latest, Phase.One, Phase.Two).filter(st.publishes)
      val results = tracker.track_all(phases.map(phase_results(_, n)))
      tracker.run_if_needed(_ =>
        phases.map(_.results_file).zip(results).to(SortedMap)
      )
    }
  }

  // Whether grading `phase` to `n` tries has tests to run, as far as the
  // published results tell (as Project.has_work): a student's graded commit
  // isn't in them, they don't cover the tests the phase runs (or have ones
  // it doesn't show), or a test it runs has tries to go. Missing or
  // unreadable results are work too.
  lazy val has_work: (Phase, Int) => Target[Boolean] = fun {
    (phase: Phase, n: Int) =>
      complex_target[Boolean] { tracker =>
        val st = tracker.track(state)
        val keep_going = st.keeps_going(phase)
        val students = tracker.track(students_with_submission).toSeq
        val published = tracker.track(published_results(phase))
        val ids =
          tracker
            .track(test_ids_for(st.tests(phase)))
            .map(_.redacted)
        // the tests that may be shown besides, with the runs they have:
        // never work
        val shown =
          tracker
            .track(test_ids_for(st.shows(phase)))
            .map(_.redacted)
        val infos =
          tracker.track_all(
            students.map(csid =>
              Submission(c, p, csid, env).prepare_info(phase)
            )
          )
        val aliases =
          tracker.track_all(
            students.map(csid => Submission(c, p, csid, env).alias)
          )
        tracker.run_if_needed { _ =>
          published.toOption.flatten match {
            case None            => true
            case Some(published) =>
              infos.zip(aliases).exists {
                // the results leave out students without an alias
                case (Some(info), Some(a)) =>
                  published.get(a) match {
                    case None    => true
                    case Some(r) =>
                      r.prepare_info.sha != info.sha ||
                      !ids.subsetOf(r.outcomes.keySet) ||
                      !r.outcomes.keySet.subsetOf(shown) ||
                      r.outcomes.view
                        .filterKeys(ids.contains)
                        .values
                        .exists { o =>
                          o.outcomes.size < n &&
                          (keep_going || o.outcomes
                            .forall(_._1 == OutcomeStatus.pass))
                        }
                  }
                case _ => false
              }
          }
        }
      }
  }

  // What a project's report pages show (as r2's HtmlGen.page): its published
  // results, from the mirror, and its settings
  lazy val report_inputs: Target[ReportInputs] =
    target(
      (
        latest = published_results(Phase.Latest),
        one = published_results(Phase.One),
        two = published_results(Phase.Two),
        state = state,
        phase1_tests = phase1_tests,
        phase1_weight = phase1_weight,
        phase2_tests = phase2_tests,
        phase2_weight = phase2_weight,
        weights = test_weights,
        bad_tests = bad_tests,
        test_cutoff = test_cutoff,
        code_cutoff = code_cutoff,
        enrollment = Course(c, env).enrollment,
        aliases = this.aliases,
        course_staff = Course(c, env).staff,
        project_staff = staff,
        projects = Course(c, env).active_projects(repos.CoursesConfig.any)
      )
    ) { (_, deps) =>
      ReportInputs(
        title = s"${c}_$p",
        project_name = p.value,
        test_cutoff = deps.test_cutoff,
        code_cutoff = deps.code_cutoff,
        enrollments = deps.enrollment.size,
        results = Seq(
          Phase.Latest -> deps.latest,
          Phase.One -> deps.one,
          Phase.Two -> deps.two
        ).map((phase, r) => (phase, r, deps.state.publishes(phase))),
        phase1_tests = deps.phase1_tests,
        phase1_weight = deps.phase1_weight,
        phase2_tests = deps.phase2_tests,
        phase2_weight = deps.phase2_weight,
        weights = deps.weights,
        bad_tests = deps.bad_tests,
        staff_aliases = HtmlGen.staff_aliases(
          deps.aliases,
          deps.course_staff ++ deps.project_staff
        ),
        tabs = HtmlGen.tabs(c.value, p.value, deps.projects.map(_.value))
      )
    }

  lazy val scores: (Phase, Int) => Target[SortedMap[CSID, SortedSet[TestId]]] =
    fun { (phase: Phase, n: Int) =>
      complex_target[SortedMap[CSID, SortedSet[TestId]]] { tracker =>
        val csids = tracker.track(students_with_submission).toSeq
        val passed =
          tracker.track_all(
            csids.map(csid => Submission(c, p, csid, env).scores(phase, n))
          )
        tracker.run_if_needed(_ => csids.zip(passed).to(SortedMap))
      }
    }

  // ---- helpers -------------------------------------------------------

  // a name's base and extension, as os.Path's baseName and ext
  private def split(name: String): (String, String) =
    name.lastIndexOf('.') match {
      case -1 => (name, "")
      case i  => (name.take(i), name.drop(i + 1))
    }

  private def named(ids: SortedSet[TestId], names: SortedSet[String]) =
    ids.filter(t =>
      names.contains(t.external_name) || names.contains(t.internal_name)
    )

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
}

object Project {
  // a file on master, if the repo and the file are there
  private[grader] def text_on(
      ctx: Context[?],
      m: Option[Commit],
      file: String
  ): Option[String] =
    m.flatMap(c => ctx.read_text(ctx.commit(c).tree, os.RelPath(file)))

  private[grader] def readable[A: upickle.default.Reader](
      json: Option[String],
      name: String
  ): Boolean =
    json.exists(j =>
      Try(NoNulls.check(upickle.default.read[A](j), name)).isSuccess
    )
}
