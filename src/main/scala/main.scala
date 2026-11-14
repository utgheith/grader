import ag.common.{RunLock, given_ReadWriter_Regex, given_ReadWriter_SortedMap}
import ag.grader.{CSID, Course, Phase, HtmlGen, Project, TestId, TestSet}
import ag.r2.{Scope, State, Target}
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
    workspace: os.Path = os.pwd / "workspace",
    @arg(doc =
      "start paused: nothing runs until it's resumed from the run's page (needs config.json's \"web\")"
    )
    paused: Flag = Flag(false),
    @arg(
      name = "pause-at-end",
      doc =
        "before exiting, wait to be let go from the run's page, to look at how it ended (needs config.json's \"web\")"
    )
    pause_at_end: Flag = Flag(false)
) extends Scope(".") {

  lazy val courses: Target[Regex] = (this / courses_).target(EmptyTuple) {
    (_, _) =>
      courses_
  }

  lazy val projects: Target[Regex] = (this / projects_).target(EmptyTuple) {
    (_, _) =>
      projects_
  }

  lazy val students: Target[Regex] = (this / students_).target(EmptyTuple) {
    (_, _) =>
      students_
  }

  lazy val tests: Target[Regex] = (this / tests_).target(EmptyTuple) { (_, _) =>
    tests_
  }

  // TODO: warn when no courses matched
  lazy val selected_courses: Target[Seq[Course]] =
    target((active_courses = Course.active_courses, courses = courses)) {
      (_, deps) =>
        deps.active_courses
          .filter(course => deps.courses.matches(course.course_name))
          .sorted
    }

  // TODO: warn when no projects matched
  lazy val selected_projects: Target[Seq[Project]] =
    target(
      (
        active_projects = Project.active_projects,
        courses = courses,
        projects = projects
      )
    ) { (_, deps) =>
      for {
        p <- deps.active_projects.toSeq
        if deps.projects.matches(p.project_name)
        if deps.courses.matches(p.course.course_name)
      } yield p
    }
}

object CommonArgs {
  given ParserForClass[CommonArgs] = ParserForClass[CommonArgs]
}

object Main {
  // The keys in the dropbox (see ag.grader.Dropbox), for each active course
  // the arguments pick
  @main
  def dropbox(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val pubs = publishers_of(commonArgs.workspace)
      val defs = pubs.grading.runs.subs.defs
      for {
        c <- s.track(defs.active_courses(commonArgs.courses_)).toSeq
      } {
        println(s"\n------ $c -------")
        println(
          upickle.default.write(s.track(pubs.dropbox.keys(c)), indent = 2)
        )
      }
    }

  // The tests (picked by -t) no picked enrolled student is happy with, in
  // their published results (see Submissions.student_outcomes):
  // course_project:test
  @main
  def bad_tests(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val subs = publishers_of(commonArgs.workspace).grading.runs.subs
      val defs = subs.defs
      val r = (for {
        c <- defs.active_courses(commonArgs.courses_).tally(s).toSeq
        p <- defs.active_projects(c, commonArgs.projects_).tally(s).toSeq
        csid <- subs.enrolled(c, commonArgs.students_).tally(s).toSeq
        // counted on its own, to tell no results from results without tests
        _ <- subs.student_results(c, p, csid).tally(s).toSeq
        (test, outcome) <- subs
          .student_outcomes(c, p, csid, commonArgs.tests_)
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
  // to push (see Publishers.keys): every active course's, since they share
  // the repo; -c picks the courses whose key counts are shown.
  @main
  def publish_keys(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val pubs = publishers_of(commonArgs.workspace)
      val defs = pubs.grading.runs.subs.defs
      if (pubs.dropbox.root.isEmpty)
        println("no dropbox_path in config.json: keys left alone")
      else {
        for {
          c <- s.track(defs.active_courses(commonArgs.courses_)).toSeq
        } {
          println(s"\n------ $c -------")
          println(s.track(pubs.dropbox.keys(c)).size)
        }
        val ob = outbox_of(commonArgs.workspace)
        val p = s.track(pubs.keys)
        val todo = ob.record(p)
        println(
          s"gitolite-admin: ${if (todo) "to push" else "up to date"}; " +
            s"${p.value.notices.size} to tell; ${ob.pending.size} pending in all"
        )
      }
    }

  // Records each picked active course's enrollment, from the dropbox, in
  // the outbox, for `sync` to push (see Publishers.enrollment): a course
  // the dropbox has no directory for is left alone.
  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val pubs = publishers_of(commonArgs.workspace)
      val defs = pubs.grading.runs.subs.defs
      if (pubs.dropbox.root.isEmpty)
        println("no dropbox_path in config.json: enrollments left alone")
      else {
        val ob = outbox_of(commonArgs.workspace)
        for {
          c <- s.track(defs.active_courses(commonArgs.courses_)).toSeq
        } {
          println(s"\n------ $c -------")
          s.track(pubs.dropbox.course_keys(c)) match {
            case None => println("no directory in the dropbox: left alone")
            case Some(keys) =>
              val todo = ob.record(s.track(pubs.enrollment(c)))
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
      val subs = publishers_of(commonArgs.workspace).grading.runs.subs
      for {
        c <- s.track(subs.defs.active_courses(commonArgs.courses_)).toSeq
      } {
        println(s"\n------ $c -------")
        println(s.track(subs.enrollment(c)).size)
      }
    }

  // Records each picked active course's grades repo in the outbox, for
  // `sync` to push (see repos.GradesRepo): made with a README if it isn't
  // there, then left to the staff.
  @main
  def create_grades_repos(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val defs = publishers_of(commonArgs.workspace).grading.runs.subs.defs
      val env = ag.grader.repos.Env(defs.source)
      val ob = outbox_of(commonArgs.workspace)
      val cs = defs.active_courses(commonArgs.courses_).tally(s)
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
      val defs = publishers_of(commonArgs.workspace).grading.runs.subs.defs
      val cs = defs.active_courses(commonArgs.courses_).tally(s)
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
          ag.r3.Source(mirror_root(commonArgs.workspace)).masters
        )
      )
        println(s"$repo $id")
    }

  // the picked active projects of the picked active courses, one per line:
  // course_project
  @main
  def projects(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val defs = publishers_of(commonArgs.workspace).grading.runs.subs.defs
      val ps = for {
        c <- defs.active_courses(commonArgs.courses_).tally(s).toSeq
        p <- defs.active_projects(c, commonArgs.projects_).tally(s).toSeq
      } yield ag.grader.repos.ProjectRepo.name_of(c, p)
      ps.foreach(println)
      s.tally.report("projects", ps.size, commonArgs.verbose.value)
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
      val runs = publishers_of(commonArgs.workspace).grading.runs
      val subs = runs.subs
      val defs = subs.defs
      val tests_re = commonArgs.tests_
      val f = s.tally
      var shown = 0
      for {
        c <- defs.active_courses(commonArgs.courses_).tally(s).toSeq
        p <- defs.active_projects(c, commonArgs.projects_).tally(s).toSeq
      } {
        val aliases = runs.aliases(c, p).tally(s)
        val published = s.track(subs.aliases(c, p))
        val csids = subs
          .enrolled(c, commonArgs.students_)
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
      val grading = publishers_of(commonArgs.workspace).grading
      val defs = grading.runs.subs.defs
      val tests_re = commonArgs.tests_
      val f = s.tally
      var found = 0
      for {
        c <- defs.active_courses(commonArgs.courses_).tally(s).toSeq
        p <- defs.active_projects(c, commonArgs.projects_).tally(s).toSeq
      } {
        val state = s.track(grading.state(c, p))
        val tests = state.tests(phase)
        val shown = state.shows(phase)
        val ids = grading.runs
          .test_ids_for(c, p, tests)
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
      val pubs = publishers_of(commonArgs.workspace)
      val subs = pubs.grading.runs.subs
      val defs = subs.defs
      val ob = outbox_of(commonArgs.workspace)
      var found = 0
      for {
        c <- defs.active_courses(commonArgs.courses_).tally(s).toSeq
        p <- defs.active_projects(c, commonArgs.projects_).tally(s).toSeq
      } {
        val csids = subs.enrolled(c, commonArgs.students_).tally(s).toSeq
        val made = s.track_all(
          csids.map(csid =>
            ag.grader.repos.WorkRepo(c, p, csid, pubs.env).latest
          )
        )
        val k = made.count(ob.record)
        println(s"${c}_$p: ${csids.size} students, $k to make")
        found += csids.size
      }
      println(s"${ob.pending.size} pending in all")
      s.tally.report("publish_work_repos", found, commonArgs.verbose.value)
    }

  // r3's workspace for grading, inside the run's: r3 keeps its own state
  // (r3.git, checkouts, ...) beside ag.r2's targets/
  private def r3_workspace(workspace: os.Path): os.Path = workspace / "r3"

  // Checks the r3 port of the course and project definitions
  // (ag.grader.Definitions, on the local mirror) against ag.r2's (Course,
  // Project, on the git server): every course's and project's settings, and
  // each phase's deadline for every student with an extension. Prints what
  // differs; meant for a mirror that's up to date with the server.
  @main
  def compare_definitions(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      val r2 = scala.collection.mutable.SortedMap[String, Any]()
      val r3 = scala.collection.mutable.SortedMap[String, Any]()
      val nobody = ag.grader.CSID("nobody")

      val courses = root.track(Course.all)
      r2("courses") = courses.map(_.course_name).to(SortedSet)
      r2("active courses") =
        root.track(Course.active_courses).map(_.course_name).to(SortedSet)
      for (c <- courses) {
        val cn = c.course_name
        r2(s"$cn active") = root.track(c.active)
        r2(s"$cn staff") = root.track(c.staff)
        r2(s"$cn notifications") = root.track(c.notifications)
        val ps = root.track(c.projects)
        r2(s"$cn projects") = ps.keySet
        r2(s"$cn active projects") = root.track(c.active_projects).keySet
        for (p <- ps.values) {
          val k = s"$cn/${p.project_name}"
          r2(s"$k active") = root.track(p.active)
          r2(s"$k phase1_tests") = root.track(p.phase1_tests)
          r2(s"$k phase1_weight") = root.track(p.phase1_weight)
          r2(s"$k phase2_tests") = root.track(p.phase2_tests)
          r2(s"$k phase2_weight") = root.track(p.phase2_weight)
          r2(s"$k test_weights") = root.track(p.test_weights)
          r2(s"$k bad_tests") = root.track(p.bad_tests)
          r2(s"$k iterations") = root.track(p.iterations)
          r2(s"$k test_cutoff") = root.track(p.test_cutoff)
          r2(s"$k code_cutoff") = root.track(p.code_cutoff)
          r2(s"$k extensions") = root.track(p.extensions)
          r2(s"$k test_extensions") = root.track(p.test_extensions)
          r2(s"$k docker_file") = root.track(p.docker_file)
          r2(s"$k staff") = root.track(p.staff)
          val e = root.track(p.extensions)
          for {
            csid <- (e.phase1.keySet ++ e.phase2.keySet + nobody).toSeq
            phase <- Phase.values
          } r2(s"$k deadline ${csid.value} ${phase.toString}") =
            root.track(p.deadline(csid, phase))
        }
      }

      val d = ag.grader.Definitions(ag.r3.Source(mirror_root(root.workspace)))
      val s = ag.r3.State(r3_workspace(root.workspace))
      r3("courses") = s.track(d.course_names)
      r3("active courses") =
        s.track(d.active_courses(ag.grader.Definitions.any))
      for (cn <- s.track(d.course_names)) {
        r3(s"$cn active") = s.track(d.course_active(cn))
        r3(s"$cn staff") = s.track(d.course_staff(cn))
        r3(s"$cn notifications") = s.track(d.notifications(cn))
        r3(s"$cn projects") = s.track(d.project_names(cn))
        r3(s"$cn active projects") =
          s.track(d.active_projects(cn, ag.grader.Definitions.any))
        for (pn <- s.track(d.project_names(cn))) {
          val k = s"$cn/$pn"
          r3(s"$k active") = s.track(d.project_active(cn, pn))
          r3(s"$k phase1_tests") = s.track(d.phase1_tests(cn, pn))
          r3(s"$k phase1_weight") = s.track(d.phase1_weight(cn, pn))
          r3(s"$k phase2_tests") = s.track(d.phase2_tests(cn, pn))
          r3(s"$k phase2_weight") = s.track(d.phase2_weight(cn, pn))
          r3(s"$k test_weights") = s.track(d.test_weights(cn, pn))
          r3(s"$k bad_tests") = s.track(d.bad_tests(cn, pn))
          r3(s"$k iterations") = s.track(d.iterations(cn, pn))
          r3(s"$k test_cutoff") = s.track(d.test_cutoff(cn, pn))
          r3(s"$k code_cutoff") = s.track(d.code_cutoff(cn, pn))
          r3(s"$k extensions") = s.track(d.extensions(cn, pn))
          r3(s"$k test_extensions") = s.track(d.test_extensions(cn, pn))
          r3(s"$k docker_file") = s.track(d.docker_file(cn, pn))
          r3(s"$k staff") = s.track(d.project_staff(cn, pn))
          val e = s.track(d.extensions(cn, pn))
          for {
            csid <- (e.phase1.keySet ++ e.phase2.keySet + nobody).toSeq
            phase <- Phase.values
          } r3(s"$k deadline ${csid.value} ${phase.toString}") =
            s.track(d.deadline(cn, pn, csid, phase))
        }
      }

      val keys = r2.keySet ++ r3.keySet
      val differ = keys.filter(k => r2.get(k) != r3.get(k))
      for (k <- differ)
        println(s"$k\n  r2: ${r2.get(k).toString}\n  r3: ${r3.get(k).toString}")
      println(
        s"${keys.size - differ.size} of ${keys.size} the same, ${differ.size} differ"
      )
    }

  // A directory's files, for comparing data: path -> its link's target or
  // its contents' hash (directories aren't listed: git trees can't keep
  // empty ones)
  private def files_of(dir: os.Path): SortedMap[String, String] =
    if (!os.isDir(dir)) SortedMap()
    else
      os.walk(dir, followLinks = false)
        .filter(p => !os.isDir(p, followLinks = false))
        .map { p =>
          val what =
            if (os.isLink(p)) s"link ${os.readLink(p).toString}"
            else {
              // streamed: a file can be bigger than memory
              val md = java.security.MessageDigest.getInstance("SHA-256").nn
              scala.util.Using.resource(os.read.inputStream(p)) { in =>
                val buf = new Array[Byte](1 << 20)
                var n = in.read(buf)
                while (n >= 0) { md.update(buf, 0, n); n = in.read(buf) }
              }
              md.digest().nn.map(b => f"$b%02x").mkString
            }
          (p.relativeTo(dir).toString, what)
        }
        .to(SortedMap)

  // Checks the r3 port of the submissions (ag.grader.Submissions, on the
  // local mirror) against ag.r2's (Course, Project, on the git server), for
  // the projects (active or not) and students the arguments pick: each
  // submission, its graded commit and what's prepared from it per phase,
  // its own test, and what's read back from the aliases
  // and results repos. ag.r2 goes first (it may push, e.g. the override
  // repo), then the mirror is brought up to date, then r3. Prints what
  // differs.
  @main
  def compare_submissions(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      val r2 = scala.collection.mutable.SortedMap[String, Any]()
      val r3 = scala.collection.mutable.SortedMap[String, Any]()
      val course_re = root.track(commonArgs.courses)
      val project_re = root.track(commonArgs.projects)
      val student_re = root.track(commonArgs.students)
      val picked = for {
        c <- root.track(Course.all)
        if course_re.matches(c.course_name)
        p <- root.track(c.projects).values.toSeq
        if project_re.matches(p.project_name)
      } yield p
      println(s"${picked.size} projects")
      // the commits prepared, per project and student: the same on both sides
      val shas = scala.collection.mutable
        .Map[String, scala.collection.immutable.SortedSet[String]]()
      def key(p: Project, csid: ag.grader.CSID) =
        s"${p.course.course_name}/${p.project_name}/${csid.value}"

      for (p <- picked) {
        val k = s"${p.course.course_name}/${p.project_name}"
        val csids =
          root
            .track(p.course.enrolled_csids)
            .filter(s => student_re.matches(s.value))
        r2(s"$k enrolled") = csids
        r2(s"$k with submission") =
          root.track(p.students_with_submission).filter(csids.contains)
        r2(s"$k aliases") = root.track(p.get_aliases)
        // prepare reads it, and it's pushed if it isn't there: before the
        // mirror is brought up to date, so r3 sees the same
        val _ = root.track(p.publish_override_repo)
        val tests = root.track(p.student_tests_by_csid)
        for (csid <- csids) {
          val ks = key(p, csid)
          r2(s"$ks student test") = tests.get(csid).map(_.value)
          r2(s"$ks student test files") =
            tests.get(csid).map(t => files_of(t.get_data_path(root)))
          r2(s"$ks results") = root.track(p.get_student_results(csid))
          for (phase <- Phase.values) {
            r2(s"$ks ${phase.toString} graded") =
              root.track(p.graded_commit(csid, phase))
            val info = root.track(p.prepare_info(csid, phase))
            r2(s"$ks ${phase.toString} info") = info
            r2(s"$ks ${phase.toString} late") =
              root.track(p.is_late(csid, phase))
            info.foreach(i =>
              shas(ks) = shas.getOrElse(
                ks,
                scala.collection.immutable.SortedSet[String]()
              ) + i.sha
            )
          }
        }
      }

      println("bringing the mirror up to date")
      ag.grader.LocalMirror.update(
        ag.grader.Config.gitolite_server,
        mirror_root(root.workspace),
        _ => ()
      )

      val defs =
        ag.grader.Definitions(ag.r3.Source(mirror_root(root.workspace)))
      val subs = ag.grader.Submissions(defs)
      val s = ag.r3.State(r3_workspace(root.workspace))
      for (p <- picked) {
        val (c, pn) = (
          ag.grader.CourseName(p.course.course_name),
          ag.grader.ProjectName(p.project_name)
        )
        val k = s"$c/$pn"
        val csids = s
          .track(subs.enrolled_csids(c))
          .filter(x => student_re.matches(x.value))
        r3(s"$k enrolled") = csids
        r3(s"$k with submission") =
          s.track(subs.students_with_submission(c, pn)).filter(csids.contains)
        r3(s"$k aliases") = s.track(subs.aliases(c, pn))
        val tests = s.track(subs.student_tests_by_csid(c, pn))
        for (csid <- csids) {
          val ks = key(p, csid)
          r3(s"$ks student test") = tests.get(csid).map(_.value)
          r3(s"$ks student test files") =
            tests.get(csid).map(t => files_of(s.checkout(t.data)))
          r3(s"$ks results") = s.track(subs.student_results(c, pn, csid))
          for (phase <- Phase.values) {
            r3(s"$ks ${phase.toString} graded") =
              s.track(subs.graded_commit(c, pn, csid, phase))
            val info = s.track(subs.prepare_info(c, pn, csid, phase))
            r3(s"$ks ${phase.toString} info") = info
            r3(s"$ks ${phase.toString} late") =
              s.track(subs.is_late(c, pn, csid, phase))
            info.foreach(i =>
              shas(ks) = shas.getOrElse(
                ks,
                scala.collection.immutable.SortedSet[String]()
              ) + i.sha
            )
          }
        }
      }

      // what's prepared from each commit graded, on either side
      println(s"comparing ${shas.values.map(_.size).sum} prepared commits")
      for {
        p <- picked
        csid <- root.track(p.course.enrolled_csids).toSeq
        ks = key(p, csid)
        sha <- shas
          .getOrElse(ks, scala.collection.immutable.SortedSet[String]())
          .toSeq
      } {
        r2(s"$ks prepared $sha") =
          files_of(root.track(p.prepare(csid, sha)).get_data_path(root))
        r3(s"$ks prepared $sha") = files_of(
          s.checkout(
            s.track(
              subs.prepare(
                ag.grader.CourseName(p.course.course_name),
                ag.grader.ProjectName(p.project_name),
                csid,
                sha
              )
            ).data
          )
        )
      }

      val keys = r2.keySet ++ r3.keySet
      val differ = keys.filter(k => r2.get(k) != r3.get(k))
      for (k <- differ)
        println(
          s"$k\n  r2: ${r2.get(k).toString.take(2000)}\n  r3: ${r3.get(k).toString.take(2000)}"
        )
      println(
        s"${keys.size - differ.size} of ${keys.size} the same, ${differ.size} differ"
      )
    }

  // Checks the r3 port of the tests (ag.grader.TestRuns) against ag.r2's
  // (Project), for the projects (active or not) the arguments pick: the
  // aliases, the test sets, each test's files and the docker image. r2's
  // side publishes the aliases and tests repos, as its runs do, before the
  // mirror is brought up to date and r3's side reads it. With --run, it also
  // runs the tests -t picks on the latest commits of the students -s picks,
  // once on each side, and compares the outcomes and the files each run
  // left (not their contents: times, say, differ from run to run).
  @main
  def compare_tests(
      commonArgs: CommonArgs,
      @arg(doc = "also run the tests, once on each side")
      run: Flag = Flag(false)
  ): Unit =
    in_workspace(commonArgs) { root =>
      val r2 = scala.collection.mutable.SortedMap[String, Any]()
      val r3 = scala.collection.mutable.SortedMap[String, Any]()
      val course_re = root.track(commonArgs.courses)
      val project_re = root.track(commonArgs.projects)
      val student_re = root.track(commonArgs.students)
      val test_re = root.track(commonArgs.tests)
      val picked = for {
        c <- root.track(Course.all)
        if course_re.matches(c.course_name)
        p <- root.track(c.projects).values.toSeq
        if project_re.matches(p.project_name)
      } yield p
      println(s"${picked.size} projects")
      val sets: Seq[ag.grader.TestSet] =
        Seq[ag.grader.TestSet]("all", "good", "chosen", "phase1", "phase2")
      def picks(id: ag.grader.TestId): Boolean =
        test_re.matches(id.external_name) || test_re.matches(id.internal_name)
      def outcome(o: ag.grader.Outcome, files: Iterable[String]) =
        (o.outcomes.map(_._1), files.toSeq.sorted)

      for (p <- picked) {
        val k = s"${p.course.course_name}/${p.project_name}"
        r2(s"$k aliases") = root.track(p.anti_aliases).map(_.swap)
        for (set <- sets)
          r2(s"$k ${set: String}") = root.track(p.test_ids_for(set))
        for (id <- root.track(p.test_ids))
          r2(s"$k test ${id.toString}") =
            files_of(root.track(p.test_info(id)).get_data_path(root))
        r2(s"$k image") = root.track(p.docker_image)
      }
      if (run.value) {
        val runs = for {
          p <- picked
          csid <- root.track(p.students_with_submission).toSeq
          if student_re.matches(csid.value)
          info <- root.track(p.prepare_info(csid, Phase.Latest)).toSeq
          id <- root.track(p.test_ids).toSeq
          if picks(id)
        } yield (p, csid, id, p.run_one(false, 1)(csid, info.sha, id))
        println(s"running ${runs.size} tests with r2")
        for (((p, csid, id, _), o) <- runs.zip(root.track_all(runs.map(_._4))))
          r2(
            s"${p.course.course_name}/${p.project_name}/${csid.value} run ${id.toString}"
          ) = outcome(o.value, os.list(o.get_data_path(root)).map(_.last))
      }

      println("bringing the mirror up to date")
      ag.grader.LocalMirror.update(
        ag.grader.Config.gitolite_server,
        mirror_root(root.workspace),
        _ => ()
      )

      val defs =
        ag.grader.Definitions(ag.r3.Source(mirror_root(root.workspace)))
      val subs = ag.grader.Submissions(defs)
      val t = ag.grader.TestRuns(subs)
      val s = ag.r3.State(r3_workspace(root.workspace))
      for (p <- picked) {
        val (c, pn) = (
          ag.grader.CourseName(p.course.course_name),
          ag.grader.ProjectName(p.project_name)
        )
        val k = s"$c/$pn"
        r3(s"$k aliases") = s.track(t.aliases(c, pn))
        for (set <- sets)
          r3(s"$k ${set: String}") = s.track(t.test_ids_for(c, pn, set))
        for (id <- s.track(t.test_ids(c, pn)))
          r3(s"$k test ${id.toString}") =
            files_of(s.checkout(s.track(t.test_info(c, pn, id)).data))
        r3(s"$k image") = s.track(t.docker_image(c, pn))
      }
      if (run.value) {
        val runs = for {
          p <- picked
          (c, pn) = (
            ag.grader.CourseName(p.course.course_name),
            ag.grader.ProjectName(p.project_name)
          )
          csid <- s.track(subs.students_with_submission(c, pn)).toSeq
          if student_re.matches(csid.value)
          _ <- s.track(subs.prepare_info(c, pn, csid, Phase.Latest)).toSeq
          id <- s.track(t.test_ids(c, pn)).toSeq
          if picks(id)
        } yield (
          p,
          csid,
          id,
          t.run_one(false, 1)(c, pn, csid, Phase.Latest, id)
        )
        println(s"running ${runs.size} tests with r3")
        for (((p, csid, id, _), o) <- runs.zip(s.track_all(runs.map(_._4))))
          r3(
            s"${p.course.course_name}/${p.project_name}/${csid.value} run ${id.toString}"
          ) = outcome(o.value, s.list(o.data).getOrElse(Seq()))
      }

      val keys = r2.keySet ++ r3.keySet
      val differ = keys.filter(k => r2.get(k) != r3.get(k))
      for (k <- differ)
        println(
          s"$k\n  r2: ${r2.get(k).toString.take(2000)}\n  r3: ${r3.get(k).toString.take(2000)}"
        )
      println(
        s"${keys.size - differ.size} of ${keys.size} the same, ${differ.size} differ"
      )
    }

  private def outbox_of(workspace: os.Path): ag.grader.Outbox =
    ag.grader.Outbox(
      workspace / "outbox",
      mirror_root(workspace),
      r3_workspace(workspace) / "r3.git"
    )

  // Works out, from the local mirror, what the repos the arguments pick
  // should hold (ag.grader.Publishers), and records what differs in the
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
      val pubs = publishers_of(commonArgs.workspace)
      val subs = pubs.grading.runs.subs
      val defs = subs.defs
      val picked = for {
        c <- s.track(defs.course_names).toSeq
        if course_re.matches(c.value)
        p <- s.track(defs.project_names(c)).toSeq
        if project_re.matches(p.value)
      } yield (c, p)
      val courses = s.track(defs.active_courses(course_re)).toSeq
      // with no dropbox, the keys and enrollments are left alone: they're
      // written from it, and an empty one would unenroll everyone
      val has_dropbox = pubs.dropbox.root.isDefined
      if (!has_dropbox)
        println(
          "no dropbox_path in config.json: keys and enrollments left alone"
        )
      // each enrolled student's repo for each active project
      val work_repos = for {
        c <- courses
        p <- s.track(defs.active_projects(c, project_re)).toSeq
        csid <- s.track(subs.enrolled(c, student_re)).toSeq
      } yield pubs.work_repo(c, p, csid)
      val wanted = s.track_all(
        (if (has_dropbox) pubs.keys +: courses.map(pubs.enrollment)
         else Seq()) ++
          courses.flatMap(c => Seq(pubs.grades(c), pubs.site(c))) ++
          picked.flatMap((c, p) => pubs.all(c, p)) ++
          work_repos
      )
      val ob = outbox_of(commonArgs.workspace)
      val n = wanted.count(ob.record)
      println(
        s"${courses.size} courses, ${picked.size} projects, ${work_repos.size} work repos; " +
          s"${wanted.size} repos, $n to push; ${ob.pending.size} pending in all"
      )
    }

  // the r3 layers, on the workspace's local mirror
  private def publishers_of(workspace: os.Path): ag.grader.Publishers =
    ag.grader.Publishers(
      ag.grader.Grading(
        ag.grader.TestRuns(
          ag.grader.Submissions(
            ag.grader.Definitions(ag.r3.Source(mirror_root(workspace)))
          )
        )
      ),
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
      s: ag.r3.State,
      commonArgs: CommonArgs,
      tries: Option[Int],
      narrow: Boolean,
      phase: Option[Phase]
  ): Seq[(ag.grader.CourseName, ag.grader.ProjectName, Int)] = {
    val course_re = commonArgs.courses_
    val project_re = commonArgs.projects_
    val grading = publishers_of(commonArgs.workspace).grading
    val subs = grading.runs.subs
    val defs = subs.defs
    val candidates = for {
      c <- s.track(defs.active_courses(course_re)).toSeq
      p <- s.track(defs.active_projects(c, project_re)).toSeq
    } yield (c, p)
    def name(c: ag.grader.CourseName, p: ag.grader.ProjectName) = s"${c}_$p"
    def state(c: ag.grader.CourseName, p: ag.grader.ProjectName) =
      s.track(grading.state(c, p))
    def phases(c: ag.grader.CourseName, p: ag.grader.ProjectName): Seq[Phase] =
      Seq(Phase.Latest, Phase.One, Phase.Two).filter(state(c, p).publishes)
    def work(
        c: ag.grader.CourseName,
        p: ag.grader.ProjectName
    ): Option[(ag.grader.Band, Int)] =
      ag.grader.Priority.band(
        state(c, p).is_live,
        s.track(defs.iterations(c, p)),
        n => phases(c, p).exists(ph => s.track(grading.has_work(c, p, ph, n)))
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
            (c, p, w.fold(s.track(defs.iterations(c, p)).max)(_._2))
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
        val runs = grading.runs
        val outs = s.track_all(for {
          ph <- run_phases
          csid <- subs.students_with_submission(c, p).tally(s).toSeq
          if f.keep("matching -s", commonArgs.students_.matches(csid.value))
          // a commit to grade for the phase
          _ <- subs.prepare_info(c, p, csid, ph).tally(s).toSeq
          id <- runs.test_ids_for(c, p, st.tests(ph)).tally(s).toSeq
          if f.keep(
            "matching -t",
            commonArgs.tests_.matches(id.external_name) ||
              commonArgs.tests_.matches(id.internal_name)
          )
        } yield runs.run_one(st.keeps_going(ph), n)(c, p, csid, ph, id))
        val results = outs.map(_.value)
        println(
          s"${name(c, p)}: ${(ag.grader.runs_started() - started).toString} runs; " +
            s"${results.count(_.is_happy)} of ${results.size} test results passed"
        )
        f.report("run", results.size, commonArgs.verbose.value)
      } else {
        val students = s.track(subs.students_with_submission(c, p)).toSeq
        val files = s.track(grading.results_files(c, p, n))
        val latest = s.track_all(
          students.map(grading.student_results(c, p, _, Phase.Latest, n))
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
  // each student's own results and prepared code (see Publishers.graded).
  // The r3 form of publish_results.
  @main
  def grade(
      commonArgs: CommonArgs,
      @arg(doc =
        "the most tries a test gets (default: as the project's iterations say)"
      )
      tries: Option[Int] = None
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val pubs = publishers_of(commonArgs.workspace)
      val subs = pubs.grading.runs.subs
      if (commonArgs.students_.regex != ".*" || commonArgs.tests_.regex != ".*")
        println("grade publishes whole projects: -s and -t are ignored")
      val projects = run_projects(s, commonArgs, tries, false, None)
      val wanted = s.track_all(projects.flatMap { (c, p, n) =>
        pubs.graded(c, p, s.track(subs.students_with_submission(c, p)), n)
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
      val grading = publishers_of(commonArgs.workspace).grading
      val runs = grading.runs
      val defs = runs.subs.defs
      for {
        c <- s.track(defs.active_courses(course_re)).toSeq
        p <- s.track(defs.active_projects(c, project_re)).toSeq
      } {
        val weights = s.track(defs.test_weights(c, p))
        val weight = (for {
          t <- s.track(runs.test_ids(c, p)).toSeq
          w <- weights
          if Regex(w.pattern).matches(t.external_name)
        } yield (t, w.weight)).to(SortedMap)
        val total = weight.values.sum
        println(s"${c}_$p after 1 runs, total $total:")
        for ((csid, passed) <- s.track(grading.scores(c, p, phase, tries))) {
          val raw = passed.toSeq.map(weight).sum
          val late = s.track(runs.subs.is_late(c, p, csid, phase))
          val score = if (late) raw.toDouble / 2.0 else raw.toDouble
          println(
            f"    ${csid.toString} ${score.toString} ${100.0 * (score / total.toDouble)}%0.2f ${
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
      val pubs = publishers_of(commonArgs.workspace)
      val defs = pubs.grading.runs.subs.defs
      val host = ag.grader.Config.cloudflare.filter(_.enabled)
      if (host.isEmpty)
        println("cloudflare isn't enabled in config.json: sites not served")
      val ob = outbox_of(commonArgs.workspace)
      val cs = defs.active_courses(commonArgs.courses_).tally(s)
      for (c <- cs.toSeq) {
        val todo =
          ob.record(s.track(ag.grader.repos.SiteRepo(c, pubs.env).latest))
        val at = host.map(h =>
          s.track(ag.grader.repos.SiteDeployment(c, h, pubs.env).deployed)
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
            ag.grader.StatusData(ag.r3.Source(mirror_root(workspace))),
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

  // Every command works in its workspace holding the workspace's lock: two
  // runs sharing a workspace would corrupt each other's state.
  private def in_workspace[A](args: CommonArgs)(f: State => A): A = {
    val workspace = args.workspace
    os.makeDir.all(workspace)
    RunLock(workspace, command_line) {
      ag.rules.use_std_out_err(workspace / "std_out_err")
      State.run(workspace, args.force.value) { state =>
        val web = ag.grader.Config.web
        ag.r2.LiveRun.serving(
          state,
          web,
          paused = args.paused.value,
          pause_at_end = args.pause_at_end.value,
          pages = web.flatMap(_ => status_pages(workspace))
        )(f(state))
      }
    }
  }

  // in_workspace for the r3 commands, with no ag.r2.State: the workspace's
  // lock, the command output in it, and the status pages served while it
  // runs (if config.json has "web"; there's no live view of an r3 run, so
  // --paused and --pause-at-end don't apply). `f` gets an r3 State in the
  // workspace's r3/, which --force makes rebuild what the command asks for.
  private def in_r3_workspace[A](args: CommonArgs)(f: ag.r3.State => A): A = {
    val workspace = args.workspace
    if (args.paused.value || args.pause_at_end.value)
      println("--paused and --pause-at-end are for r2 runs: ignored")
    os.makeDir.all(workspace)
    RunLock(workspace, command_line) {
      ag.rules.use_std_out_err(workspace / "std_out_err")
      val server = for {
        web <- ag.grader.Config.web
        pages <- status_pages(workspace)
        server <- ag.web.WebServer.start(workspace, web, Some(pages), None)
      } yield server
      try
        ag.r3.State.run(
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
