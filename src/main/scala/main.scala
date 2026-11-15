import ag.common.{RunLock, given_ReadWriter_SortedMap}
import ag.grader.{CSID, Course, Phase, HtmlGen, Project, Submission}
import ag.grader.repos.CoursesConfig
import mainargs.{
  Flag,
  ParserForClass,
  ParserForMethods,
  TokensReader,
  arg,
  main
}

import scala.collection.SortedMap
import scala.util.matching.Regex
import scala.collection.SortedSet

given TokensReader.Simple[os.Path] with {
  override def shortName: String = "path"

  /** */
  override def read(strings: Seq[String]): Either[String, os.Path] = if (
    strings.isEmpty
  ) {
    Right(os.pwd / "workspace")
  } else {
    val one = strings.mkString("/")
    val p = if (one.startsWith("/")) {
      os.Path(one)
    } else {
      os.pwd / os.RelPath(one)
    }
    Right(p)
  }
}

given TokensReader.Simple[Regex] with {
  override def shortName: String = "regex"

  override def alwaysRepeatable: Boolean = true

  /** */
  override def read(strings: Seq[String]): Either[String, Regex] = if (
    strings.isEmpty
  ) {
    Right(""".*""".r)
  } else {
    Right(Regex(strings.mkString("|")))
  }
}

given TokensReader.Simple[Phase] with {
  override def shortName: String = "1|2|latest"
  override def read(strings: Seq[String]): Either[String, Phase] =
    strings.headOption match {
      case Some("1")      => Right(Phase.One)
      case Some("2")      => Right(Phase.Two)
      case Some("latest") => Right(Phase.Latest)
      case s              =>
        Left(s"Invalid phase '${s.getOrElse("")}'; phases are 1, 2 and latest")
    }
}

enum AliasSortMode:
  case CSID
  case Alias

given TokensReader.Simple[AliasSortMode] with {
  override def shortName: String = "mode"
  override def allowEmpty: Boolean = false
  override def alwaysRepeatable: Boolean = false

  /** */
  override def read(strings: Seq[String]): Either[String, AliasSortMode] =
    strings.head match
      case "alias" => Right(AliasSortMode.Alias)
      case "csid"  => Right(AliasSortMode.CSID)
      case s       =>
        Left(s"Invalid sort mode '$s'; possible modes are 'alias' or 'csid'")
}

//@main
case class CommonArgs(
    @arg(name = "courses", short = 'c', doc = "courses to consider (regex)")
    courses_ : Regex = """.*""".r,
    @arg(short = 'p', doc = "projects to consider (regex)")
    projects_ : Regex = """.*""".r,
    @arg(short = 's', doc = "students to consider (regex)")
    students_ : Regex = """.*""".r,
    @arg(short = 't', doc = "tests to consider (regex)")
    tests_ : Regex = """.*""".r,
    @arg(short = 'v', doc = "verbose output")
    verbose: Flag = Flag(false),
    @arg(doc =
      "r3: when done, link every target used under <workspace>/r3/browse/<its path>/@, to look at its value and data"
    )
    browse: Flag = Flag(false),
    @arg(doc =
      "rebuild what this command asks for, even if its saved results are current (their inputs are reused)"
    )
    force: Flag = Flag(false),
    workspace: os.Path = os.pwd / "workspace"
)

object CommonArgs {
  given ParserForClass[CommonArgs] = ParserForClass[CommonArgs]
}

object Main {
  // The keys in the dropbox (see ag.grader.Dropbox), for each active course
  // the arguments pick
  @main
  def dropbox(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      for {
        c <- s
          .track(CoursesConfig(env).active_courses(commonArgs.courses_))
          .toSeq
      } {
        println(s"\n------ $c -------")
        println(
          upickle.default.write(s.track(env.dropbox.keys(c)), indent = 2)
        )
      }
    }

  // The tests (picked by -t) no picked enrolled student is happy with, in
  // their published results (see Submission.published_outcomes):
  // course_project:test
  @main
  def bad_tests(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val r = (for {
        c <- CoursesConfig(env)
          .active_courses(commonArgs.courses_)
          .tally(s)
          .toSeq
        p <- Course(c, env).active_projects(commonArgs.projects_).tally(s).toSeq
        csid <- Course(c, env).enrolled(commonArgs.students_).tally(s).toSeq
        // counted on its own, to tell no results from results without tests
        _ <- Submission(c, p, csid, env).published_results.tally(s).toSeq
        (test, outcome) <- Submission(c, p, csid, env)
          .published_outcomes(commonArgs.tests_)
          .tally(s)
          .toSeq
      } yield ((c, p, test), !outcome.is_happy)).groupMapReduce(_._1)(_._2)(
        _ && _
      )

      val bad = r.toSeq.sortBy(_._1).filter(_._2).map(_._1)
      for ((c, p, t) <- bad)
        println(s"${c}_$p:${t.external_name}")
      s.tally.report("bad_tests", bad.size, commonArgs.verbose.value)
    }

  // Records the dropbox's keys in gitolite-admin in the outbox, for `sync`
  // to push (see repos.KeysRepo): every active course's, since they share
  // the repo; -c picks the courses whose key counts are shown.
  @main
  def publish_keys(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      if (env.dropbox.root.isEmpty)
        println("no dropbox_path in config.json: keys left alone")
      else {
        for {
          c <- s
            .track(CoursesConfig(env).active_courses(commonArgs.courses_))
            .toSeq
        } {
          println(s"\n------ $c -------")
          println(s.track(env.dropbox.keys(c)).size)
        }
        val ob = outbox_of(commonArgs.workspace)
        val p = s.track(ag.grader.repos.KeysRepo(env).latest)
        val todo = ob.record(p)
        println(
          s"gitolite-admin: ${if (todo) "to push" else "up to date"}; " +
            s"${p.value.notices.size} to tell; ${ob.pending.size} pending in all"
        )
      }
    }

  // Records each picked active course's enrollment, from the dropbox, in
  // the outbox, for `sync` to push (see repos.EnrollmentRepo): a course
  // the dropbox has no directory for is left alone.
  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      if (env.dropbox.root.isEmpty)
        println("no dropbox_path in config.json: enrollments left alone")
      else {
        val ob = outbox_of(commonArgs.workspace)
        for {
          c <- s
            .track(CoursesConfig(env).active_courses(commonArgs.courses_))
            .toSeq
        } {
          println(s"\n------ $c -------")
          s.track(env.dropbox.course_keys(c)) match {
            case None => println("no directory in the dropbox: left alone")
            case Some(keys) =>
              val todo = ob.record(
                s.track(ag.grader.repos.EnrollmentRepo(c, env).latest)
              )
              println(
                s"${keys.size} students; ${
                    if (todo) "to push" else "up to date"
                  }"
              )
          }
        }
        println(s"${ob.pending.size} pending in all")
      }
    }

  // How many students each picked active course has enrolled, as its
  // enrollment repo on the local mirror says
  @main
  def enrollment(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      for {
        c <- s
          .track(CoursesConfig(env).active_courses(commonArgs.courses_))
          .toSeq
      } {
        println(s"\n------ $c -------")
        println(s.track(Course(c, env).enrollment).size)
      }
    }

  // Records each picked active course's grades repo in the outbox, for
  // `sync` to push (see repos.GradesRepo): made with a README if it isn't
  // there, then left to the staff.
  @main
  def create_grades_repos(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val ob = outbox_of(commonArgs.workspace)
      val cs = CoursesConfig(env).active_courses(commonArgs.courses_).tally(s)
      for (c <- cs.toSeq) {
        val repo = ag.grader.repos.GradesRepo(c, env)
        val todo = ob.record(s.track(repo.latest))
        println(
          s"${repo.repo_name}: ${if (todo) "to push" else "up to date"}"
        )
      }
      println(s"${ob.pending.size} pending in all")
      s.tally.report("create_grades_repos", cs.size, commonArgs.verbose.value)
    }

  // the picked active courses, one per line
  @main
  def courses(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val cs = CoursesConfig(env).active_courses(commonArgs.courses_).tally(s)
      cs.foreach(println)
      s.tally.report("courses", cs.size, commonArgs.verbose.value)
    }

  // every repo's master on the local mirror, as of its last sync: "repo
  // sha", as the server's `history`
  @main
  def history(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      for (
        (repo, id) <- s.track(
          ag.rules.Source(mirror_root(commonArgs.workspace)).masters
        )
      )
        println(s"$repo $id")
    }

  // the picked active projects of the picked active courses, one per line:
  // course_project
  @main
  def projects(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val ps = for {
        c <- CoursesConfig(env)
          .active_courses(commonArgs.courses_)
          .tally(s)
          .toSeq
        p <- Course(c, env).active_projects(commonArgs.projects_).tally(s).toSeq
      } yield ag.grader.repos.ProjectRepo.name_of(c, p)
      ps.foreach(println)
      s.tally.report("projects", ps.size, commonArgs.verbose.value)
    }

  // Each picked active course's enrolled students (picked by -s), each with
  // the picked active projects they have a submission in
  @main
  def students(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      var shown = 0
      for (
        c <- CoursesConfig(env)
          .active_courses(commonArgs.courses_)
          .tally(s)
          .toSeq
      ) {
        val ps =
          s.track(Course(c, env).active_projects(commonArgs.projects_)).toSeq
        val submitted = s.track_all(
          ps.map(p => Project(c, p, env).students_with_submission)
        )
        val csids = Course(c, env).enrolled(commonArgs.students_).tally(s).toSeq
        println(s"\n$c (${csids.size.toString} students):")
        val width = csids.map(_.value.length).maxOption.getOrElse(0)
        for (csid <- csids) {
          val in = for {
            (p, with_submission) <- ps.zip(submitted)
            if with_submission.contains(csid)
          } yield p.value
          println(
            String.format(s"  %-${width}s  %s", csid.value, in.mkString(" "))
          )
        }
        shown += csids.size
      }
      s.tally.report("students", shown, commonArgs.verbose.value)
    }

  // Each picked enrolled student's alias in each picked active project, as
  // `grade` would publish it (marked if it isn't published yet; ? if they
  // have none: no submission yet). -s picks students by CSID; -t picks them
  // by their own test, named by alias outside and CSID inside (as run's -t),
  // so a student with no alias has no test for it to pick.
  @main
  def aliases(
      commonArgs: CommonArgs,
      @arg(
        name = "sort",
        doc = "How to sort the aliases; 'alias' or 'csid'.  Defaults to 'csid'."
      )
      sortMode: AliasSortMode = AliasSortMode.CSID
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val tests_re = commonArgs.tests_
      val f = s.tally
      var shown = 0
      for {
        c <- CoursesConfig(env)
          .active_courses(commonArgs.courses_)
          .tally(s)
          .toSeq
        p <- Course(c, env).active_projects(commonArgs.projects_).tally(s).toSeq
      } {
        val aliases = Project(c, p, env).aliases.tally(s)
        val published = s.track(Project(c, p, env).published_aliases)
        val csids = Course(c, env)
          .enrolled(commonArgs.students_)
          .tally(s)
          .toSeq
          .filter(csid =>
            f.keep(
              "matching -t",
              tests_re.regex == ".*" || aliases
                .get(csid)
                .exists(a =>
                  tests_re.matches(a.toString) || tests_re.matches(csid.value)
                )
            )
          )
        val sorted = sortMode match {
          case AliasSortMode.CSID  => csids
          case AliasSortMode.Alias =>
            csids.sortBy(aliases.get(_).fold(Int.MaxValue)(_.value))
        }
        val base_name = s"${c}_$p"
        val width =
          sorted
            .map(_.value.length)
            .maxOption
            .getOrElse(0) + base_name.length + 1
        println(s"\n$base_name:")
        for (csid <- sorted) {
          val name =
            String.format(s"%-${width}s", s"${base_name}_${csid.value}")
          val alias = aliases.get(csid) match {
            case None                                       => "?"
            case Some(a) if published.get(csid).contains(a) => a.toString
            case Some(a) => s"${a.toString} (not published yet)"
          }
          println(s"$name  $alias")
        }
        shown += sorted.size
      }
      f.report("aliases", shown, commonArgs.verbose.value)
    }

  // What `grade` would run for each picked active project in a phase, as
  // things stand: the test set and keep-going the state and phase give (and
  // the set shown, if it's another), and the tests themselves, those -t
  // picks (by external or internal name, as run's -t).
  @main
  def phase_tests(
      commonArgs: CommonArgs,
      @arg(doc = "1, 2 or latest (default: the latest results)")
      phase: Phase = Phase.Latest
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val tests_re = commonArgs.tests_
      val f = s.tally
      var found = 0
      for {
        c <- CoursesConfig(env)
          .active_courses(commonArgs.courses_)
          .tally(s)
          .toSeq
        p <- Course(c, env).active_projects(commonArgs.projects_).tally(s).toSeq
      } {
        val state = s.track(Project(c, p, env).state)
        val tests = state.tests(phase)
        val shown = state.shows(phase)
        val ids = Project(c, p, env)
          .test_ids_for(tests)
          .tally(s)
          .toSeq
          .filter(id =>
            f.keep(
              "matching -t",
              tests_re.matches(id.external_name) ||
                tests_re.matches(id.internal_name)
            )
          )
        println(
          s"\n${c}_$p (${HtmlGen.phase_label(phase)}): ${state.description}; " +
            (if (state.publishes(phase)) "published" else "not published yet") +
            s"; ${tests: String} tests (${ids.size})" +
            (if (shown != tests) s", shown: ${shown: String}" else "") +
            s", keep-going ${if (state.keeps_going(phase)) "on" else "off"}"
        )
        for (id <- ids) println(s"  ${id.external_name}")
        found += ids.size
      }
      f.report("phase_tests", found, commonArgs.verbose.value)
    }

  @main
  def publish_work_repos(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val ob = outbox_of(commonArgs.workspace)
      var found = 0
      for {
        c <- CoursesConfig(env)
          .active_courses(commonArgs.courses_)
          .tally(s)
          .toSeq
        p <- Course(c, env).active_projects(commonArgs.projects_).tally(s).toSeq
      } {
        val csids = Course(c, env).enrolled(commonArgs.students_).tally(s).toSeq
        val made = s.track_all(
          csids.map(csid => ag.grader.repos.WorkRepo(c, p, csid, env).latest)
        )
        val k = made.count(ob.record)
        println(s"${c}_$p: ${csids.size} students, $k to make")
        found += csids.size
      }
      println(s"${ob.pending.size} pending in all")
      s.tally.report("publish_work_repos", found, commonArgs.verbose.value)
    }

  // the engine's workspace, inside the run's: its state (r3.git,
  // checkouts, ...)
  private def r3_workspace(workspace: os.Path): os.Path = workspace / "r3"

  private def outbox_of(workspace: os.Path): ag.grader.Outbox =
    ag.grader.Outbox(
      workspace / "outbox",
      mirror_root(workspace),
      r3_workspace(workspace) / "r3.git"
    )

  // Works out, from the local mirror, what the repos the arguments pick
  // should hold (ag.grader.repos), and records what differs in the
  // outbox, for `sync` to push: the students' keys and each active course's
  // enrollment (from the dropbox), grades repo and site; each project's
  // override, aliases, submitted tests and tests repos; and each enrolled
  // student's work repo for each active project. The results repos are
  // `grade`'s. Run it again after a sync: what's read from the mirror (the
  // tests from the override repo, the students from the enrollment) takes
  // a round to get there.
  @main
  def outbox(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val course_re = commonArgs.courses_
      val project_re = commonArgs.projects_
      val student_re = commonArgs.students_
      val env = env_of(commonArgs.workspace)
      val picked = for {
        c <- s.track(CoursesConfig(env).course_names).toSeq
        if course_re.matches(c.value)
        p <- s.track(Course(c, env).project_names).toSeq
        if project_re.matches(p.value)
      } yield (c, p)
      val courses = s.track(CoursesConfig(env).active_courses(course_re)).toSeq
      // with no dropbox, the keys and enrollments are left alone: they're
      // written from it, and an empty one would unenroll everyone
      val has_dropbox = env.dropbox.root.isDefined
      if (!has_dropbox)
        println(
          "no dropbox_path in config.json: keys and enrollments left alone"
        )
      // each enrolled student's repo for each active project
      val work_repos = for {
        c <- courses
        p <- s.track(Course(c, env).active_projects(project_re)).toSeq
        csid <- s.track(Course(c, env).enrolled(student_re)).toSeq
      } yield ag.grader.repos.WorkRepo(c, p, csid, env).latest
      val wanted = s.track_all(
        (if (has_dropbox)
           ag.grader.repos.KeysRepo(env).latest +: courses.map(
             ag.grader.repos.EnrollmentRepo(_, env).latest
           )
         else Seq()) ++
          courses.flatMap(c =>
            Seq(
              ag.grader.repos.GradesRepo(c, env).latest,
              ag.grader.repos.SiteRepo(c, env).latest
            )
          ) ++
          picked.flatMap((c, p) => ag.grader.repos.Grade.project(c, p, env)) ++
          work_repos
      )
      val ob = outbox_of(commonArgs.workspace)
      val n = wanted.count(ob.record)
      println(
        s"${courses.size} courses, ${picked.size} projects, ${work_repos.size} work repos; " +
          s"${wanted.size} repos, $n to push; ${ob.pending.size} pending in all"
      )
    }

  // what the r3 targets work from: the workspace's local mirror and the
  // dropbox
  private def env_of(workspace: os.Path): ag.grader.repos.Env =
    ag.grader.repos.Env(
      ag.rules.Source(mirror_root(workspace)),
      ag.grader.Dropbox(ag.grader.Config.dropbox_root)
    )

  // Picks the active projects to run, from the local mirror (as
  // publish_results did: with no -p, one with work to do, picked by
  // priority, see Priority; named ones as far as their work goes; --tries:
  // all of them, to that many), and works out their results: every phase
  // they publish (its tests and keep-going from the project's state), and
  // each student's latest. The tries are a limit, not a promise: a test
  // runs up to that many times, and stops sooner at a failure unless it
  // keeps going. Publishes nothing. Says how it went; the projects, each
  // with its limit.
  //
  // With `narrow`, only the students -s picks, the tests -t picks, and the
  // phase `phase` (if given) are run, and only their outcomes worked out.
  private def run_projects(
      s: ag.rules.State,
      commonArgs: CommonArgs,
      tries: Option[Int],
      narrow: Boolean,
      phase: Option[Phase]
  ): Seq[(ag.grader.CourseName, ag.grader.ProjectName, Int)] = {
    val course_re = commonArgs.courses_
    val project_re = commonArgs.projects_
    val env = env_of(commonArgs.workspace)
    val candidates = for {
      c <- s.track(CoursesConfig(env).active_courses(course_re)).toSeq
      p <- s.track(Course(c, env).active_projects(project_re)).toSeq
    } yield (c, p)
    def name(c: ag.grader.CourseName, p: ag.grader.ProjectName) = s"${c}_$p"
    def state(c: ag.grader.CourseName, p: ag.grader.ProjectName) =
      s.track(Project(c, p, env).state)
    def phases(c: ag.grader.CourseName, p: ag.grader.ProjectName): Seq[Phase] =
      Seq(Phase.Latest, Phase.One, Phase.Two).filter(state(c, p).publishes)
    def work(
        c: ag.grader.CourseName,
        p: ag.grader.ProjectName
    ): Option[(ag.grader.Band, Int)] =
      ag.grader.Priority.band(
        state(c, p).is_live,
        s.track(Project(c, p, env).iterations),
        n =>
          phases(c, p).exists(ph => s.track(Project(c, p, env).has_work(ph, n)))
      )
    def report(
        c: ag.grader.CourseName,
        p: ag.grader.ProjectName,
        w: Option[(ag.grader.Band, Int)]
    ) =
      println(
        s"${name(c, p)}: ${state(c, p).description}; ${w.fold("up to date")(
            (band, n) => s"work to do (${band.description}, up to $n tries)"
          )}"
      )
    val named = commonArgs.projects_.regex != ".*"
    val projects: Seq[(ag.grader.CourseName, ag.grader.ProjectName, Int)] =
      tries match {
        case Some(n) => candidates.map((c, p) => (c, p, n))
        case None if named || candidates.size <= 1 =>
          for ((c, p) <- candidates) yield {
            val w = work(c, p)
            report(c, p, w)
            (c, p, w.fold(s.track(Project(c, p, env).iterations).max)(_._2))
          }
        case None =>
          val busy = for {
            (c, p) <- candidates
            w = work(c, p)
            _ = report(c, p, w)
            (band, n) <- w
          } yield ((c, p, n), p.value, band)
          val picked = ag.grader.Priority.pick(busy).toSeq
          if (picked.isEmpty) println("nothing to do")
          picked
      }
    for ((c, p, n) <- projects) {
      val run_phases = phase.fold(phases(c, p))(Seq(_))
      println(
        s"running ${name(c, p)}, up to $n tries: ${state(c, p).description}; " +
          run_phases
            .map(ph =>
              s"${HtmlGen.phase_label(ph)}: ${state(c, p).tests(ph): String}" +
                (if (state(c, p).keeps_going(ph)) ", keep-going" else "")
            )
            .mkString("; ")
      )
      val started = ag.grader.runs_started()
      if (narrow) {
        val f = s.tally
        val st = state(c, p)
        val outs = s.track_all(
          for {
            ph <- run_phases
            csid <- Project(c, p, env).students_with_submission.tally(s).toSeq
            if f.keep("matching -s", commonArgs.students_.matches(csid.value))
            // a commit to grade for the phase
            _ <- Submission(c, p, csid, env).prepare_info(ph).tally(s).toSeq
            id <- Project(c, p, env).test_ids_for(st.tests(ph)).tally(s).toSeq
            if f.keep(
              "matching -t",
              commonArgs.tests_.matches(id.external_name) ||
                commonArgs.tests_.matches(id.internal_name)
            )
          } yield Submission(c, p, csid, env)
            .run_one(st.keeps_going(ph), n)(ph, id)
        )
        val results = outs.map(_.value)
        println(
          s"${name(c, p)}: ${(ag.grader.runs_started() - started).toString} runs; " +
            s"${results.count(_.is_happy)} of ${results.size} test results passed"
        )
        f.report("run", results.size, commonArgs.verbose.value)
      } else {
        val students =
          s.track(Project(c, p, env).students_with_submission).toSeq
        val files = s.track(Project(c, p, env).results_files(n))
        val latest = s.track_all(
          students.map(csid =>
            Submission(c, p, csid, env).results(Phase.Latest, n)
          )
        )
        val outcomes = latest.flatten.flatMap(_.outcomes.values)
        println(
          s"${name(c, p)}: ${(ag.grader.runs_started() - started).toString} runs; " +
            s"${files.keys.mkString(", ")} for ${students.size} students; latest: " +
            s"${outcomes.count(_.is_happy)} of ${outcomes.size} test results passed"
        )
      }
    }
    projects
  }

  // Runs the picked projects' tests, here, up to their limit (see
  // run_projects): nothing is published (grade publishes it). -s, -t and
  // --phase run just those students, tests and phase.
  @main
  def run(
      commonArgs: CommonArgs,
      @arg(doc =
        "the most tries a test gets (default: as the project's iterations say)"
      )
      tries: Option[Int] = None,
      @arg(doc =
        "run just this phase: 1, 2 or latest (default: every phase the project publishes)"
      )
      phase: Option[Phase] = None
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val narrow = phase.isDefined ||
        commonArgs.students_.regex != ".*" || commonArgs.tests_.regex != ".*"
      val _ = run_projects(s, commonArgs, tries, narrow, phase)
    }

  // Runs the picked projects (as run does), then records their results, as
  // far as they got, in the outbox, for `sync` to push: what the project
  // publishes (override, aliases, submitted tests, tests), its results, and
  // each student's own results and prepared code (see repos.Grade.all).
  // The r3 form of publish_results. With -s or -t, only the students and
  // tests they pick are run (as run's), and what's recorded is everyone's
  // results as far as they've run (see repos.Grade.so_far), with only
  // the picked students' own repos.
  @main
  def grade(
      commonArgs: CommonArgs,
      @arg(doc =
        "the most tries a test gets (default: as the project's iterations say)"
      )
      tries: Option[Int] = None
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val picking =
        commonArgs.students_.regex != ".*" || commonArgs.tests_.regex != ".*"
      val projects = run_projects(s, commonArgs, tries, picking, None)
      val wanted = s.track_all(projects.flatMap { (c, p, n) =>
        val students =
          s.track(Project(c, p, env).students_with_submission).toSeq
        if (picking)
          ag.grader.repos.Grade.so_far(
            c,
            p,
            // as run_projects picked them (its report counts them)
            students.filter(csid => commonArgs.students_.matches(csid.value)),
            env
          )
        else ag.grader.repos.Grade.all(c, p, students, n, env)
      })
      val ob = outbox_of(commonArgs.workspace)
      val k = wanted.count(ob.record)
      println(
        s"${projects.size} projects, ${wanted.size} repos, $k to push; ${ob.pending.size} pending in all"
      )
    }

  // Each student's score for `phase`: the weights of the phase 2 tests they
  // passed at least once in `tries` tries, halved if late (as
  // r2's compute_scores, for one round), from the local mirror
  @main
  def scores(
      commonArgs: CommonArgs,
      @arg(short = 'n', doc = "how many tries")
      tries: Int = 1,
      @arg(doc = "1 or 2 (default: the latest)")
      phase: Phase = Phase.Latest
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val course_re = commonArgs.courses_
      val project_re = commonArgs.projects_
      val env = env_of(commonArgs.workspace)
      for {
        c <- s.track(CoursesConfig(env).active_courses(course_re)).toSeq
        p <- s.track(Course(c, env).active_projects(project_re)).toSeq
      } {
        val weights = s.track(Project(c, p, env).test_weights)
        val weight = (for {
          t <- s.track(Project(c, p, env).test_ids).toSeq
          w <- weights
          if Regex(w.pattern).matches(t.external_name)
        } yield (t, w.weight)).to(SortedMap)
        val total = weight.values.sum
        println(s"${c}_$p after 1 runs, total $total:")
        for (
          (csid, passed) <- s.track(Project(c, p, env).scores(phase, tries))
        ) {
          val raw = passed.toSeq.map(weight).sum
          val late = s.track(Submission(c, p, csid, env).is_late(phase))
          val score = if (late) raw.toDouble / 2.0 else raw.toDouble
          println(
            f"    ${csid.toString} ${score.toString} ${100.0 * (score / total.toDouble)}%.2f ${
                if (late) "late" else "ontime"
              }"
          )
        }
      }
    }

  // Pushes the outbox to the git server (see ag.grader.Outbox), then brings
  // the local mirror up to date. Needs config.json's can_push_repo.
  @main
  def sync(workspace: os.Path = os.pwd / "workspace"): Unit =
    if (!ag.grader.Config.pushes_allowed)
      println("config.json doesn't allow pushing (can_push_repo)")
    else {
      val ob = outbox_of(workspace)
      println(s"${ob.pending.size} repos to push")
      ob.sync(
        ag.grader.Config.gitolite_server,
        ag.grader.Config.mail_allowed,
        println
      )
      println(s"${ob.pending.size} still pending")
    }

  // Records each picked active course's site in the outbox, for `sync` to
  // push, and serves it on Cloudflare (see repos.SiteRepo and
  // SiteDeployment), if config.json's cloudflare is enabled: again only
  // when its pages change. The r3 form of push_to_cloudflare.
  @main
  def push_to_cloudflare(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val env = env_of(commonArgs.workspace)
      val host = ag.grader.Config.cloudflare.filter(_.enabled)
      if (host.isEmpty)
        println("cloudflare isn't enabled in config.json: sites not served")
      val ob = outbox_of(commonArgs.workspace)
      val cs = CoursesConfig(env).active_courses(commonArgs.courses_).tally(s)
      for (c <- cs.toSeq) {
        val todo =
          ob.record(s.track(ag.grader.repos.SiteRepo(c, env).latest))
        val at = host.map(h =>
          s.track(ag.grader.repos.SiteDeployment(c, h, env).deployed)
        )
        println(
          s"$c: site ${if (todo) "to push" else "up to date"}; " +
            at.fold("not served")(u => s"served at $u")
        )
      }
      println(s"${ob.pending.size} pending in all")
      s.tally.report("push_to_cloudflare", cs.size, commonArgs.verbose.value)
    }

  // The local mirror of the git server: kept up to date by `mirror`, read
  // by the status pages.
  private def mirror_root(workspace: os.Path): os.Path = workspace / "mirror"

  // Brings the local mirror of the git server up to date (see
  // ag.grader.LocalMirror): run it now and then, e.g. from a timer.
  @main
  def mirror(workspace: os.Path = os.pwd / "workspace"): Unit =
    ag.grader.LocalMirror.update(
      ag.grader.Config.gitolite_server,
      mirror_root(workspace),
      println
    )

  // The grading status, from the local mirror (see ag.grader.Status). Its
  // data are r3 targets (ag.grader.StatusData) on the mirror, worked out by
  // read-only States in the workspace's status/ directory, which these pages
  // lock for as long as the program runs: a workspace of their own, so they
  // can go on beside runs. They never update the mirror. None if another
  // program has them.
  private def status_pages(workspace: os.Path): Option[ag.grader.Status] = {
    val dir = workspace / "status"
    os.makeDir.all(dir)
    ag.common.RunLock.acquire(dir, s"status pages for $command_line") match {
      case Right(_) =>
        val states = ag.grader.ReadOnlyStates(dir, ttl_ms = 60 * 1000)
        Some(
          ag.grader.Status(
            ag.grader.StatusData(ag.rules.Source(mirror_root(workspace))),
            () => states()
          )
        )
      case Left(holder) =>
        println(s"no status pages: ${dir.toString} is in use by $holder")
        None
    }
  }

  // Serves the status pages (and nothing else) until stopped. It only reads
  // the local mirror (see `mirror`), in its own workspace, so it can go on
  // beside runs, which serve the same pages while they're up.
  @main
  def status(workspace: os.Path = os.pwd / "workspace"): Unit =
    ag.grader.Config.web match {
      case None =>
        println(
          "the status pages need config.json's \"web\" (see ag.web.WebConfig)"
        )
      case Some(web) =>
        // its own command output: a run may be using the workspace's
        ag.rules.use_std_out_err(workspace / "status" / "std_out_err")
        status_pages(workspace).foreach { pages =>
          pages.warm(println)
          if (!ag.web.WebServer.serve_pages(workspace, web, pages))
            println(
              s"if a run is serving port ${web.port}, its server has the status pages too, at /status/"
            )
        }
    }

  // Prints a hash of a password, for config.json's "web": {"password_hash"}
  @main
  def hash_password(): Unit = {
    def ask(prompt: String): Array[Char] = Option(System.console()) match {
      case Some(console) => console.readPassword(prompt).nn
      case None          =>
        print(prompt)
        scala.io.StdIn.readLine().toCharArray.nn
    }
    val password = ask("password: ")
    if (password.isEmpty) println("empty password")
    else if (!password.sameElements(ask("again: "))) println("they differ")
    else println(ag.web.Password.hash(password))
  }

  // the command line, for telling a second run who holds the lock
  private var command_line = ""

  // Every command works in its workspace holding the workspace's lock (two
  // runs sharing a workspace would corrupt each other's state), with the
  // command output in it, and the status pages served while it runs (if
  // config.json has "web"). `f` gets an r3 State in the workspace's r3/,
  // which --force makes rebuild what the command asks for.
  private def in_r3_workspace[A](
      args: CommonArgs
  )(f: ag.rules.State => A): A = {
    val workspace = args.workspace
    os.makeDir.all(workspace)
    RunLock(workspace, command_line) {
      ag.rules.use_std_out_err(workspace / "std_out_err")
      val server = for {
        web <- ag.grader.Config.web
        pages <- status_pages(workspace)
        server <- ag.web.WebServer.start(workspace, web, Some(pages))
      } yield server
      try
        ag.rules.State.run(
          r3_workspace(workspace),
          args.force.value,
          args.browse.value
        )(f)
      finally server.foreach(_.stop())
    }
  }

  def main(args: Array[String]): Unit = {
    command_line = s"run ${args.mkString(" ")}"
    val _ = ParserForMethods(this).runOrExit(args.toIndexedSeq)
  }
}
