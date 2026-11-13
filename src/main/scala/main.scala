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
      commonArgs: CommonArgs,
      @arg(doc =
        "grade the picked projects to this many tries (default: as their iterations and work say)"
      )
      tries: Option[Int] = None
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
      if (tries.isDefined) candidates.toSeq.map(p => (p, tries.get))
      else if (named || candidates.size <= 1)
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
      r3("active courses") = s.track(d.active_courses)
      for (cn <- s.track(d.course_names)) {
        r3(s"$cn active") = s.track(d.course_active(cn))
        r3(s"$cn staff") = s.track(d.course_staff(cn))
        r3(s"$cn notifications") = s.track(d.notifications(cn))
        r3(s"$cn projects") = s.track(d.project_names(cn))
        r3(s"$cn active projects") = s.track(d.active_projects(cn))
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
  // its late commits and own test, and what's read back from the aliases
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
          r2(s"$ks late commits") = root.track(p.late_commits(csid)).value
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
        val (c, pn) = (p.course.course_name, p.project_name)
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
          r3(s"$ks late commits") = s.track(subs.late_commits(c, pn, csid))
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
              subs.prepare(p.course.course_name, p.project_name, csid, sha)
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
        val (c, pn) = (p.course.course_name, p.project_name)
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
          (c, pn) = (p.course.course_name, p.project_name)
          csid <- s.track(subs.students_with_submission(c, pn)).toSeq
          if student_re.matches(csid.value)
          info <- s.track(subs.prepare_info(c, pn, csid, Phase.Latest)).toSeq
          id <- s.track(t.test_ids(c, pn)).toSeq
          if picks(id)
        } yield (p, csid, id, t.run_one(false, 1)(c, pn, csid, info.sha, id))
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
    in_workspace(commonArgs) { root =>
      val course_re = root.track(commonArgs.courses)
      val project_re = root.track(commonArgs.projects)
      val student_re = root.track(commonArgs.students)
      val pubs = publishers_of(root.workspace)
      val subs = pubs.grading.runs.subs
      val defs = subs.defs
      val s = ag.r3.State(r3_workspace(root.workspace))
      val picked = for {
        c <- s.track(defs.course_names).toSeq
        if course_re.matches(c)
        p <- s.track(defs.project_names(c)).toSeq
        if project_re.matches(p)
      } yield (c, p)
      val courses = s.track(defs.active_courses).toSeq.filter(course_re.matches)
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
        p <- s.track(defs.active_projects(c)).toSeq
        if project_re.matches(p)
        csid <- s.track(subs.enrolled_csids(c)).toSeq
        if student_re.matches(csid.value)
      } yield pubs.work_repo(c, p, csid)
      val wanted = s.track_all(
        (if (has_dropbox) pubs.keys +: courses.map(pubs.enrollment)
         else Seq()) ++
          courses.flatMap(c => Seq(pubs.grades(c), pubs.site(c))) ++
          picked.flatMap((c, p) => pubs.all(c, p)) ++
          work_repos
      )
      val ob = outbox_of(root.workspace)
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

  // Grades the active projects the arguments pick, from the local mirror,
  // and records their results repos (the project's and each student's) in
  // the outbox, for `sync` to push: the r3 form of publish_results. The
  // phases, tests and keep-going come from each project's state. With no
  // -p, one project with work to do, picked by priority (see Priority);
  // named projects are graded as far as their work goes. --tries grades the
  // picked projects to that many tries instead.
  @main
  def grade(
      commonArgs: CommonArgs,
      @arg(doc = "how many tries (default: as the project's iterations say)")
      tries: Option[Int] = None
  ): Unit =
    in_workspace(commonArgs) { root =>
      val course_re = root.track(commonArgs.courses)
      val project_re = root.track(commonArgs.projects)
      val pubs = publishers_of(root.workspace)
      val grading = pubs.grading
      val subs = grading.runs.subs
      val defs = subs.defs
      val s = ag.r3.State(r3_workspace(root.workspace))
      val candidates = for {
        c <- s.track(defs.active_courses).toSeq
        if course_re.matches(c)
        p <- s.track(defs.active_projects(c)).toSeq
        if project_re.matches(p)
      } yield (c, p)
      def name(c: String, p: String) = s"${c}_$p"
      def state(c: String, p: String) = s.track(grading.state(c, p))
      def phases(c: String, p: String): Seq[Phase] =
        Seq(Phase.Latest, Phase.One, Phase.Two).filter(state(c, p).publishes)
      def work(c: String, p: String): Option[(ag.grader.Band, Int)] =
        ag.grader.Priority.band(
          state(c, p).is_live,
          s.track(defs.iterations(c, p)),
          n => phases(c, p).exists(ph => s.track(grading.has_work(c, p, ph, n)))
        )
      def report(c: String, p: String, w: Option[(ag.grader.Band, Int)]) =
        println(
          s"${name(c, p)}: ${state(c, p).description}; ${w.fold("up to date")(
              (band, n) => s"work to do (${band.description}, to $n tries)"
            )}"
        )
      val named = commonArgs.projects_.regex != ".*"
      val projects: Seq[(String, String, Int)] = tries match {
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
          } yield ((c, p, n), p, band)
          val picked = ag.grader.Priority.pick(busy).toSeq
          if (picked.isEmpty) println("nothing to do")
          picked
      }
      for ((c, p, n) <- projects)
        println(
          s"grading ${name(c, p)} to $n tries: ${state(c, p).description}; " +
            phases(c, p)
              .map(ph =>
                s"${HtmlGen.phase_label(ph)}: ${state(c, p).tests(ph): String}" +
                  (if (state(c, p).keeps_going(ph)) ", keep-going" else "")
              )
              .mkString("; ")
        )
      val wanted = s.track_all(projects.flatMap { (c, p, n) =>
        pubs.graded(c, p, s.track(subs.students_with_submission(c, p)), n)
      })
      val ob = outbox_of(root.workspace)
      val k = wanted.count(ob.record)
      println(
        s"${projects.size} projects, ${wanted.size} repos, $k to push; ${ob.pending.size} pending in all"
      )
    }

  // Each student's score for `phase`: the weights of the phase 2 tests they
  // passed at least once in `tries` tries, halved if late (as
  // compute_scores, for one round), from the local mirror
  @main
  def scores(
      commonArgs: CommonArgs,
      @arg(short = 'n', doc = "how many tries")
      tries: Int = 1,
      @arg(doc = "1 or 2 (default: the latest)")
      phase: Phase = Phase.Latest
  ): Unit =
    in_workspace(commonArgs) { root =>
      val course_re = root.track(commonArgs.courses)
      val project_re = root.track(commonArgs.projects)
      val grading = publishers_of(root.workspace).grading
      val runs = grading.runs
      val defs = runs.subs.defs
      val s = ag.r3.State(r3_workspace(root.workspace))
      for {
        c <- s.track(defs.active_courses).toSeq
        if course_re.matches(c)
        p <- s.track(defs.active_projects(c)).toSeq
        if project_re.matches(p)
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
      ob.sync(ag.grader.Config.gitolite_server, println)
      println(s"${ob.pending.size} still pending")
    }

  // Deploys each active course's site repo, as the local mirror has it, to
  // its Cloudflare Pages project (see CloudflareConfig), if it changed since
  // it was last deployed from this workspace (workspace/cloudflare/<course>
  // says what was): the r3 form of push_to_cloudflare. Needs config.json's
  // cloudflare, enabled.
  @main
  def deploy(commonArgs: CommonArgs): Unit =
    ag.grader.Config.cloudflare.filter(_.enabled) match {
      case None     => println("cloudflare isn't enabled in config.json")
      case Some(cf) =>
        in_workspace(commonArgs) { root =>
          val course_re = root.track(commonArgs.courses)
          val defs = publishers_of(root.workspace).grading.runs.subs.defs
          val s = ag.r3.State(r3_workspace(root.workspace))
          val dir = root.workspace / "cloudflare"
          for {
            c <- s.track(defs.active_courses).toSeq
            if course_re.matches(c)
          } {
            val project = cf.project_of(c)
            s.track(defs.source.master(s"${c}__site")) match {
              case None         => println(s"$c: no site yet")
              case Some(commit) =>
                val tree = s.commit(commit).tree
                // what's deployed: where, and which tree
                val what = s"$project ${cf.domain.getOrElse("")} ${tree.id}"
                val stamp = dir / c
                if (scala.util.Try(os.read(stamp)).toOption.contains(what))
                  println(s"$c: up to date ($project)")
                else {
                  // the repo's files, less its dot files; wrangler runs in
                  // the site's parent, where its .wrangler cache goes
                  val work = dir / s".tmp-$c"
                  os.remove.all(work)
                  val site = work / "site"
                  os.makeDir.all(site)
                  s.copy_to(tree, site)
                  for (f <- os.list(site) if f.last.startsWith("."))
                    os.remove.all(f)
                  println(s"$c: deploying to $project")
                  val url = cf.publish(site, c)
                  os.write.over(stamp, what, createFolders = true)
                  os.remove.all(work)
                  println(s"$c: deployed, at $url")
                }
            }
          }
        }
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
          "the status pages need config.json's \"web\" (see ag.r2.WebConfig)"
        )
      case Some(web) =>
        // its own command output: a run may be using the workspace's
        ag.rules.use_std_out_err(workspace / "status" / "std_out_err")
        status_pages(workspace).foreach { pages =>
          pages.warm(println)
          if (!ag.r2.WebServer.serve_pages(workspace, web, pages))
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
        val web = ag.grader.Config.web
        ag.r2.WebServer.serving(
          state,
          web,
          paused = paused,
          pause_at_end = pause_at_end,
          pages = web.flatMap(_ => status_pages(workspace))
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
