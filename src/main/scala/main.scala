import ag.common.{
  RunLock,
  given_ReadWriter_Regex,
  given_ReadWriter_SortedMap,
  human,
  timed
}
import ag.grader.{
  Band,
  Priority,
  ProjectState,
  CSID,
  Course,
  Phase,
  Gitolite,
  HtmlGen,
  Project,
  TestId,
  TestSet
}
import ag.r2.{Many, Scope, State, Target, run_if_needed}
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
import java.time.ZoneId
import java.time.format.DateTimeFormatter

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
  override def shortName: String = "1|2"
  override def read(strings: Seq[String]): Either[String, Phase] =
    strings.headOption match {
      case Some("1") => Right(Phase.One)
      case Some("2") => Right(Phase.Two)
      case s => Left(s"Invalid phase '${s.getOrElse("")}'; phases are 1 and 2")
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

  lazy val submissions: Target[Seq[(Project, CSID)]] = complex_target {
    tracker =>
      val ps: Seq[Project] = tracker.track(selected_projects)
      val csids_seq: Seq[SortedSet[CSID]] =
        tracker.track_all(ps.map(_.students_with_submission))
      val s = tracker.track(students)

      run_if_needed(tracker) { _ =>
        for {
          (p, csids) <- ps.zip(csids_seq)
          csid <- csids
          if s.matches(csid.value)
        } yield (p, csid)

      }

  }

  lazy val test_ids: Target[Seq[(Project, TestId)]] = complex_target {
    tracker =>
      val ps: Seq[Project] = tracker.track(selected_projects)

      val per_project_test_ids: Seq[SortedSet[TestId]] =
        tracker.track_all(ps.map(_.test_ids))

      val ts = tracker.track(tests)

      run_if_needed(tracker) { _ =>
        // run less often, we can afford to work harder here
        for {
          (p, test_ids) <- ps.zip(per_project_test_ids)
          test_id <- test_ids
          if ts.matches(test_id.external_name) || ts.matches(
            test_id.internal_name
          )
        } yield (p, test_id)
      }
  }

  lazy val runs: Target[Seq[(Project, CSID, TestId)]] = complex_target {
    tracker =>
      val ps: Seq[Project] = tracker.track(selected_projects)

      val per_project = tracker.track_all(
        (
          students = Many(ps.map(_.students_with_submission)),
          test_ids = Many(ps.map(_.test_ids))
        )
      )
      val per_project_ids: Seq[(SortedSet[CSID], SortedSet[TestId])] =
        per_project.students.zip(per_project.test_ids)

      val the_students = tracker.track(students)
      val the_tests = tracker.track(tests)

      run_if_needed(tracker) { _ =>
        for {
          (p, (csids, test_ids)) <- ps.zip(per_project_ids)
          the_test_ids = test_ids.filter(id =>
            the_tests.matches(id.external_name) || the_tests.matches(
              id.internal_name
            )
          )
          if the_test_ids.nonEmpty
          csid <- csids.toSeq
          if the_students.matches(csid.value)
          test_id <- the_test_ids
        } yield (p, csid, test_id)
      }
  }
}

object CommonArgs {
  given ParserForClass[CommonArgs] = ParserForClass[CommonArgs]
}

object Main {
  @main
  def dropbox(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(s"\n------ ${c.course_name} -------")
        println(upickle.default.write(root.track(c.dropbox), indent = 2))
      }
    }

  @main
  def bad_tests(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      val r = (for {
        p <- root.track(commonArgs.selected_projects)
        s <- root.track(p.course.enrollment).keySet.toSeq
        if root.track(commonArgs.students).matches(s.value)
        r <- root.track(p.get_student_results(s)).toSeq
        (test, outcome) <- r.outcomes.toSeq
      } yield (p, s, test, outcome)).groupMapReduce { case (p, _, test, _) =>
        (p, test)
      } { case (_, _, _, outcome) =>
        !outcome.is_happy
      }(_ && _)

      r.filter { case (_, v) => v }.keys.toList.sorted.foreach { case (p, t) =>
        println(s"${p.course.course_name}_${p.project_name}:${t.external_name}")
      }
    }

  @main
  def publish_keys(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(s"\n------ ${c.course_name} -------")
        println(
          upickle.default
            .write(root.track(c.publish_keys).value.size, indent = 2)
        )
      }
    }

  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(s"\n------ ${c.course_name} -------")
        println(root.track(c.publish_enrollment).value.size)
        // println(upickle.default.write(c.publish_enrollment.value, indent=2))
      }
    }

  @main
  def enrollment(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(s"\n------ ${c.course_name} -------")
        println(root.track(c.enrollment).size)
        // println(upickle.default.write(c.publish_enrollment.value, indent=2))
      }
    }

  @main
  def create_grades_repos(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(s"Creating ${c.course_name}__grades")
        val _ = root.track(c.create_grades_repo)
      }
    }

  @main
  def courses(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      println(root.track(commonArgs.selected_courses))
    }

  @main
  def history(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      root.track(Gitolite.history).foreach(println)
    }

  @main
  def info(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      root.track(Gitolite.info).foreach(println)
    }

  @main
  def projects(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      pprint.pprintln(root.track(commonArgs.selected_projects))
    }

  @main
  def overrides(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (p <- root.track(commonArgs.selected_projects)) {
        println(root.track(p.publish_override_repo))
      }
    }

  @main
  def work_repos(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        for {
          (pn, p) <- root.track(c.active_projects)
          if root.track(commonArgs.projects).matches(pn)
        } {
          for {
            (csid, _) <- root.track(c.enrollment)
            if root.track(commonArgs.students).matches(csid.value)
          } {
            println(root.track(p.work_repo(csid)))
          }
        }
      }
    }

  @main
  def aliases(
      commonArgs: CommonArgs,
      @arg(
        name = "sort",
        doc = "How to sort the aliases; 'alias' or 'csid'.  Defaults to 'csid'."
      )
      sortMode: AliasSortMode = AliasSortMode.CSID
  ): Unit = in_workspace(commonArgs) { root =>
    val sort = sortMode match
      case AliasSortMode.CSID  => false
      case AliasSortMode.Alias => true

    for (c <- root.track(commonArgs.selected_courses)) {
      val enrollment = root.track(c.enrollment)

      for {
        (pn, p) <- root.track(c.active_projects)
        if root.track(commonArgs.projects).matches(pn)
      } {
        val aliases = root.track(p.get_aliases)
        val csids = enrollment.keys
          .filter((id: CSID) =>
            root.track(commonArgs.students).matches(id.value)
          )
          .toIndexedSeq
        val sorted =
          if (sort) csids.sortBy(aliases.get(_).map(_.value)) else csids

        val base_name = s"${c.course_name}_$pn"
        val longest_csid = sorted.maxByOption(_.value.length)
        val max_width =
          longest_csid.map(_.value.length).getOrElse(0) + base_name.length + 1

        println(s"\n$base_name:")

        for (csid <- sorted) {
          val target_name = s"${base_name}_${csid.toString}"
          val padded_name = String.format(s"%-${max_width}s", target_name)
          println(
            s"$padded_name  ${aliases.get(csid).map(_.toString).getOrElse("?")}"
          )
        }
      }
    }
  }

  // What publish_results would run for each project in a phase, as things
  // stand: the test set and keep-going the state and phase give, and the
  // tests themselves.
  @main
  def phase_tests(
      commonArgs: CommonArgs,
      @arg(doc = "1 or 2 (default: the latest results)")
      phase: Phase = Phase.Latest
  ): Unit = in_workspace(commonArgs) { root =>
    for (p <- root.track(commonArgs.selected_projects)) {
      val state = state_of(p, root)
      val tests = state.tests(phase)
      val ids = root.track(p.test_ids_for(tests))
      println(
        s"\n${p.course.course_name}_${p.project_name} (${HtmlGen.phase_label(phase)}): " +
          s"${state.description}; " +
          (if (state.publishes(phase)) "published" else "not published yet") +
          s"; ${tests: String} tests (${ids.size}), keep-going ${
              if (state.keeps_going(phase)) "on" else "off"
            }"
      )
      for (id <- ids) println(s"  ${id.external_name}")
    }
  }

  @main
  def chosen_tests(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs) { root =>
    for (p <- root.track(commonArgs.selected_projects)) {
      println(s"\n${p.course.course_name}_${p.project_name}:")
      for (test_id <- root.track(p.phase2_test_ids)) {
        println(test_id.external_name)
      }
    }
  }

  @main
  def publish_tests(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs) { root =>
    for (p <- root.track(commonArgs.selected_projects)) {
      println(s"-- publishing tests for ${p.project_name}")
      pprint.pprintln(root.track(p.publish_tests))
    }
  }

  @main
  def prepare(
      commonArgs: CommonArgs,
      @arg(doc =
        "grade phase 1 or 2: the commit its commit_id_<n> file names, else the last before its deadline (default: the latest)"
      )
      phase: Phase = Phase.Latest,
      @arg(
        short = 'd',
        doc =
          "Directory to copy prepared submissions into. Defaults to 'prepared'"
      )
      destination: os.Path = os.pwd / "prepared"
  ): Unit = in_workspace(commonArgs) { root =>
    val base = destination

    for (c <- root.track(commonArgs.selected_courses)) {

      val enrollment = root.track(c.enrollment)

      for {
        (pn, p) <- root.track(c.active_projects)
        if root.track(commonArgs.projects).matches(pn)
      } {

        val aliases = root.track(p.get_aliases)
        val tests = root.track(p.publish_tests)
        val test_extensions = root.track(p.test_extensions)

        for {
          (csid, _) <- enrollment
          if root.track(commonArgs.students).matches(csid.value)
        } {
          val info = root.track(p.prepare_info(csid, phase))
          val target_name = s"${c.course_name}_${pn}_${csid.toString}"
          val target_path = base / target_name

          info match {
            case Some(data) =>
              println(
                s"${aliases.get(csid).map(_.toString).getOrElse("?")} $target_name ${data.commit_time
                    .withZoneSameInstant(ZoneId.systemDefault)
                    .toString} ${data.sha} ${data.push_time
                    .map(ins => ins.atZone(ZoneId.systemDefault()))
                    .toString}"
              )
              os.copy.over(
                from =
                  root.track(p.prepare(csid, data.sha)).get_data_path(root),
                to = target_path,
                followLinks = false,
                replaceExisting = true,
                createFolders = true
              )

              for {
                (_, test_info) <- tests.value
              } {
                p.copy_test(
                  test_info.id,
                  tests.get_data_path(root),
                  target_path,
                  test_extensions
                )
              }
            case None =>
          }
        }
      }
    }

    println(s"copied to ${base.toString}")
  }

  @main
  def late_commits(
      commonArgs: CommonArgs,
      @arg(
        short = 'd',
        doc = "Show repository names, commit hashes, and commit messages"
      )
      details: Flag,
      @arg(
        doc = "Sort all commits by date rather than grouping commits by repo"
      )
      sort: Flag
  ): Unit = in_workspace(commonArgs) { root =>
    val datetime_format = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val show_details = details.value

    for (c <- root.track(commonArgs.selected_courses)) {
      val enrollment = root.track(c.enrollment)

      for {
        (pn, p) <- root.track(c.active_projects)
        if root.track(commonArgs.projects).matches(pn)
      } {
        val project_deadline =
          root.track(p.code_cutoff).format(datetime_format)

        val late_repos = enrollment
          .map((csid, _) => csid)
          .filter(csid => root.track(commonArgs.students).matches(csid.value))
          .map(csid => (csid, root.track(p.late_commits(csid)).value))
          .filter((_, late_commits) => late_commits.nonEmpty)
          .toSeq

        println(
          s"\nFound ${late_repos.length} repositories with late commits for ${c.course_name}_$pn"
        )
        println(s"Using deadline: $project_deadline\n")

        val print_commit = (commit: ag.grader.LateCommit) => {
          val time = commit.time
            .withZoneSameInstant(ZoneId.systemDefault)
            .format(datetime_format)

          val days = commit.delay.toDays
          val hours = commit.delay.toHours % 24
          val minutes = commit.delay.toMinutes % 60
          val seconds = commit.delay.getSeconds % 60
          val duration = days match
            case 0 => f"$hours%02d:$minutes%02d:$seconds%02d"
            case 1 => f"$days%d day, $hours%02d:$minutes%02d:$seconds%02d"
            case _ => f"$days%d days, $hours%02d:$minutes%02d:$seconds%02d"

          val msg =
            if commit.message.length > 64 then commit.message.take(61) ++ "..."
            else commit.message

          val commit_hash = commit.hash.take(8)
          val line =
            if show_details then s"| $time ($duration late); $commit_hash $msg"
            else s"| $time ($duration late)"

          println(line)
        }

        if (sort.value) {
          val commits = late_repos
            .flatMap((_, late_commits) => late_commits)
            .sortBy(_.time)

          for (commit <- commits) {
            print_commit(commit)
          }
        } else {
          val repos =
            if show_details then late_repos
            else scala.util.Random.shuffle(late_repos)

          for (((csid, late_commits), i) <- repos.zipWithIndex) {
            val target_name =
              if show_details then s"${c.course_name}_${pn}_${csid.toString}"
              else s"Submission $i"

            println(target_name)
            for (commit <- late_commits) {
              print_commit(commit)
            }
          }
        }
      }
    }
  }

  @main
  def publish_work_repos(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(c.course_name)
        for {
          (pn, p) <- root.track(c.active_projects)
          if root.track(commonArgs.projects).matches(pn)
        } {
          println(s"    $pn")
          var count = 0
          for {
            (csid, _) <- root.track(c.enrollment)
            if root.track(commonArgs.students).matches(csid.value)
          } {
            val _ = root.track(p.publish_work_repo(csid))
            count = count + 1
          }
          println(s"            $count")
        }
      }
    }

  @main
  def submissions(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      pprint.pprintln(root.track(commonArgs.submissions))
      // for ((p, csid) <- commonArgs.submissions.guilty) {
      //  println(s"$p $csid")
      // }
    }

  @main
  def test_ids(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for ((p, test_id) <- root.track(commonArgs.test_ids)) {
        println(s"${p.toString} ${test_id.toString}")
      }

    }

  @main
  def latest(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs) { root =>
    root.track(Gitolite.latest).foreach(println)

  }

  // Call "f" the number of times implied by "range"
  // Stop early if total time exceeds "minutes"
  // Returns: (last index value, last returned value from "f")
  private def loop[Acc](range: Range, minutes: Int, init: Acc)(
      f: (Int, Acc) => Acc
  ): (Int, Acc) = {
    val limit = System.currentTimeMillis() + minutes * 60 * 1000

    val r = range.inclusive

    var c: Int = r.start
    var acc: Acc = init

    while (c <= r.end && System.currentTimeMillis() < limit) {
      val (t, v) = timed {
        f(c, acc)
      }
      acc = v

      println(
        s"---------------------------> finished iteration #$c in ${human(t)}, free memory ${Runtime.getRuntime
            .freeMemory()}/${Runtime.getRuntime.maxMemory()}"
      )
      c = c + r.step
    }

    c = c - r.step
    (c, acc)
  }

  @main
  def run(
      commonArgs: CommonArgs,
      @arg(doc =
        "grade phase 1 or 2: the commit its commit_id_<n> file names, else the last before its deadline (default: the latest)"
      )
      phase: Phase = Phase.Latest,
      @arg(
        name = "result-file",
        doc = "The json file to write the results summary out to"
      )
      result_file: Option[String]
  ): Unit = in_workspace(commonArgs) { root =>
    val runs = root.track(commonArgs.runs)
    // the phase's tests (as publish_results runs them), narrowed by -t
    val outs = root.track_all {
      for {
        (p, csid, test_id) <- runs
        if root
          .track(p.test_ids_for(state_of(p, root).tests(phase)))
          .contains(test_id)
        info <- root.track(p.prepare_info(csid, phase)).toSeq
      } yield p
        .run_one(
          state_of(p, root).keeps_going(phase),
          root.track(p.iterations).max
        )(
          csid,
          info.sha,
          test_id
        )
    }

    result_file.foreach(file_name => {
      val results =
        outs
          .groupBy(_.value.csid)
          .map((csid, signed_paths) => {
            val outcomes = signed_paths.map(_.value)
            val test_results = outcomes
              .groupBy(_.test_id)
              .map((test_id, runs) => {
                val passed = runs.count(_.is_happy)
                val total = runs.size
                (test_id.external_name, ujson.Arr(passed, total))
              })
              .to(Map)
            val commit_id = outcomes.head.commit_id.getOrElse("")
            (
              csid.toString(),
              Map(
                "commit_id" -> ujson.Str(commit_id),
                "results" -> ujson.Obj.from(test_results)
              )
            )
          })
          .to(Map)

      val path =
        if (file_name.charAt(0) == '/') os.Path(file_name)
        else os.pwd / os.RelPath(file_name)
      os.write(
        path,
        upickle.default.write(results, indent = 2)
      )
    })

  }

  // A project's state (from its deadlines and the clock), announced once.
  private def state_of(p: Project, root: State): ProjectState =
    root.track(p.state)

  @main
  def publish_results(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs) { root =>
    val candidates = root.track(commonArgs.selected_projects)
    def name(p: Project) = s"${p.course.course_name}_${p.project_name}"
    // the phases a project publishes, each with its test set and keep-going
    def phases(p: Project): Seq[(Phase, TestSet, Boolean)] = for {
      phase <- Seq(Phase.Latest, Phase.One, Phase.Two)
      state = state_of(p, root)
      if state.publishes(phase)
    } yield (phase, state.tests(phase), state.keeps_going(phase))
    // the band of `p`'s work and the tries it runs to, if it has work to do
    // (see Priority); the tries come from the project's iterations
    def work(p: Project): Option[(Band, Int)] =
      Priority.band(
        state_of(p, root).is_live,
        root.track(p.iterations),
        n => phases(p).exists((phase, _, _) => root.track(p.has_work(phase, n)))
      )
    def report(p: Project, w: Option[(Band, Int)]): Unit =
      println(
        s"${name(p)}: ${state_of(p, root).description}; ${w.fold("up to date")(
            (band, n) => s"work to do (${band.description}, to $n tries)"
          )}"
      )
    // with no -p, grade one project with work to do, picked by priority (see
    // Priority); named projects are graded as far as their work goes
    val named = commonArgs.projects_.regex != ".*"
    val projects: Seq[(Project, Int)] =
      if (named || candidates.size <= 1)
        for (p <- candidates.toSeq) yield {
          val w = work(p)
          report(p, w)
          (p, w.fold(root.track(p.iterations).max)(_._2))
        }
      else {
        val busy = for {
          p <- candidates.toSeq
          w = work(p)
          _ = report(p, w)
          (band, n) <- w
        } yield ((p, n), p.project_name, band)
        val picked = Priority.pick(busy).toSeq
        if (picked.isEmpty) println("nothing to do")
        picked.foreach((p, n) => println(s"picked ${name(p)}, to $n tries"))
        picked
      }
    for ((p, n) <- projects) {
      // the latest results, and each phase's once its deadline has passed;
      // the state and the phase say which tests are run and shown, and
      // whether to keep going
      phases(p).foreach { (phase, tests, kg) =>
        val shown = state_of(p, root).shows(phase)
        println(
          s"${name(p)} (${HtmlGen.phase_label(phase)}): ${state_of(p, root).description}; tests: ${tests: String}" +
            (if (shown != tests) s" (shown: ${shown: String})" else "") +
            s", keep-going ${if (kg) "on" else "off"}"
        )
        val _ = root.track(p.publish_results(kg, tests, shown)(phase, n))
      }
    }
  }

  @main
  def student_tests(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs) { root =>
    for (p <- root.track(commonArgs.selected_projects)) {
      val _ =
        val out = root.track(p.student_tests_by_csid)
        pprint.pprintln(out)
      // println(upickle.default.write(results, indent=2))
    }
  }

  @main
  def get_results(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(c.course_name)
        for {
          (pn, p) <- root.track(c.active_projects)
          if root.track(commonArgs.projects).matches(pn)
        } {
          println(s"${c.course_name}:$pn")
          for {
            (csid, _) <- root.track(c.enrollment)
            if root.track(commonArgs.students).matches(csid.value)
          } {
            val res = root.track(p.get_student_results(csid))
            res.foreach { res =>
              val count = res.outcomes.size
              val pass =
                res.outcomes.values.count(_.is_happy)
              println(
                s"${c.course_name}:$pn:${csid.toString}:${res.alias.map(_.toString).getOrElse("")}:$pass/$count"
              )
            }
          }
        }
      }
    }

  @main
  def notify_results(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        for {
          (pn, p) <- root.track(c.active_projects)
          if root.track(commonArgs.projects).matches(pn)
        } {
          for {
            (csid, _) <- root.track(c.enrollment)
            if root.track(commonArgs.students).matches(csid.value)
          } {
            // say(s"---> ${c.course_name}:$pn:$csid")
            root.track(p.notify_student_results(csid))
          }
        }
      }
    }

  @main
  def gen_html(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        for {
          (pn, p) <- root.track(c.active_projects)
          if root.track(commonArgs.projects).matches(pn)
        } {

          root.track(HtmlGen(p).gen_html)

        }
      }
    }

  @main
  def publish_site(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        println(
          s"${c.site_repo_name}: ${root.track(c.publish_site).value.mkString(" ")}"
        )
      }
    }

  @main
  // With --force, also rebuilds the site repo (an input of the deploy), so
  // a change to how the site is put together reaches Cloudflare.
  def push_to_cloudflare(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      for (c <- root.track(commonArgs.selected_courses)) {
        if (commonArgs.force.value) root.force(c.publish_site)
        val url = root.track(c.push_to_cloudflare).value
        println(s"${c.course_name}: ${url.getOrElse("not deployed")}")
      }
    }

  @main
  def compute_scores(
      commonArgs: CommonArgs,
      @arg(short = 'n', doc = "how many scoring rounds")
      count: Int = 1,
      @arg(doc =
        "grade phase 1 or 2: the commit its commit_id_<n> file names, else the last before its deadline (default: the latest)"
      )
      phase: Phase = Phase.Latest,
      minutes: Int = 1
  ): Unit = in_workspace(commonArgs) { root =>
    for (p <- root.track(commonArgs.selected_projects)) {
      val chosen = root.track(p.test_ids)
      val weights = root.track(p.test_weights)
      val test_weights = (for {
        c <- chosen
        w <- weights
        p = Regex(w.pattern)
        if p.matches(c.external_name)
      } yield (c, w.weight)).to(SortedMap)

      val max_weight = test_weights.values.sum

      val (n, outs) = loop(1 to count, minutes, Seq()) { (c, acc) =>
        val scores = (for {
          (csid, passing) <- root
            .track(
              p
                .compute_scores(phase, c)
            )
            .toSeq
          score = passing.toSeq.map(t => test_weights(t)).sum
        } yield (csid, score)).to(SortedMap)
        acc :+ scores
      }
      val total = n * max_weight
      println(
        s"${p.course.course_name}_${p.project_name} after $n runs, total $total:"
      )
      pprint.pprintln(chosen)
      pprint.pprintln(test_weights)
      pprint.pprintln(weights)
      val totals = (for {
        out <- outs
        (csid, score) <- out.toSeq
      } yield (csid, score)).groupMapReduce(_._1)(_._2)(_ + _).to(SortedMap)
      for {
        (csid, raw_score) <- totals
      } {
        val is_late =
          root.track(p.is_late(csid, phase))
        val score =
          if (is_late) raw_score.toDouble / 2.0 else raw_score.toDouble
        println(
          f"    ${csid.toString} ${score.toString} ${100.0 * (score / total.toDouble)}%0.2f ${(if (
                                                                                                 is_late
                                                                                               ) "late"
                                                                                               else "ontime").toString}"
        )
      }
    }
  }

  class Export(commonArgs: CommonArgs) {
    val root = State(commonArgs.workspace, commonArgs.force.value)
    def courses: Array[Course] =
      root.track(commonArgs.selected_courses).toArray
    def projects: Array[Project] =
      root.track(commonArgs.selected_projects).toArray
  }

  /*
  @main
  def python(
      @arg(positional = true, doc = "script file name")
      script: String,
      commonArgs: CommonArgs
  ): Unit = {
    import org.graalvm.polyglot.Context
    val code = os.read(os.pwd / script)
    val ctx = Context
      .newBuilder("python")
      .option("python.PythonPath", ".")
      .allowAllAccess(true)
      .build()
    try {
      ctx.getBindings("python").putMember("from_scala", Export(commonArgs))
      val _ = ctx.eval("python", code)
    } finally {
      ctx.close()
    }
  }
   */

  @main
  def play(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs) { root =>
      pprint.pprintln(root.track(Gitolite.mirror("courses_config")))
    }

  @main
  def pl(workspace: os.Path = os.pwd / "workspace", prefix: os.Path): Unit =
    in_workspace(workspace) { root =>
      pprint.pprintln(root.track(PlTools(prefix).fstar))
    }

  // Prints a hash of a password, for config.json's "web": {"password_hash"}
  @main
  def hash_password(): Unit = {
    def ask(prompt: String): Array[Char] = Option(System.console()) match {
      case Some(console) => console.readPassword(prompt).nn
      case None =>
        print(prompt)
        scala.io.StdIn.readLine().toCharArray.nn
    }
    val password = ask("password: ")
    if (password.isEmpty) println("empty password")
    else if (!password.sameElements(ask("again: "))) println("they differ")
    else println(ag.r2.Password.hash(password))
  }

  // the command line, for telling a second run who holds the lock
  private var command_line = ""

  // Every command works in its workspace holding the workspace's lock: two
  // runs sharing a workspace would corrupt each other's state.
  private def in_workspace[A](
      workspace: os.Path,
      force: Boolean = false,
      paused: Boolean = false,
      pause_at_end: Boolean = false
  )(
      f: State => A
  ): A = {
    os.makeDir.all(workspace)
    RunLock(workspace, command_line) {
      ag.rules.use_std_out_err(workspace / "std_out_err")
      State.run(workspace, force) { state =>
        ag.r2.WebServer.serving(
          state,
          ag.grader.Config.web,
          paused = paused,
          pause_at_end = pause_at_end
        )(f(state))
      }
    }
  }

  private def in_workspace[A](args: CommonArgs)(f: State => A): A =
    in_workspace(
      args.workspace,
      force = args.force.value,
      paused = args.paused.value,
      pause_at_end = args.pause_at_end.value
    )(f)

  def main(args: Array[String]): Unit = {
    command_line = s"run ${args.mkString(" ")}"
    val _ = ParserForMethods(this).runOrExit(args.toIndexedSeq)
  }
}
