import ag.common.{
  RunLock,
  given_ReadWriter_Regex,
  given_ReadWriter_SortedMap,
  human,
  timed
}
import ag.grader.{
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
import ag.r2.{Scope, State, Target, run_if_needed}
import ag.task.Task
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
    @arg(short = 'n', doc = "how many iterations?")
    count: Int = 1,
    @arg(short = 'v', doc = "verbose output")
    verbose: Flag = Flag(false),
    @arg(doc =
      "rebuild what this command asks for, even if its saved results are current (their inputs are reused)"
    )
    force: Flag = Flag(false),
    workspace: os.Path = os.pwd / "workspace"
) extends Scope(".") {

  lazy val courses: Target[Regex] = (this / courses_).target() {
    courses_
  }

  lazy val projects: Target[Regex] = (this / projects_).target() {
    projects_
  }

  lazy val students: Target[Regex] = (this / students_).target() {
    students_
  }

  lazy val tests: Target[Regex] = (this / tests_).target() {
    tests_
  }

  // TODO: warn when no courses matched
  lazy val selected_courses: Target[Seq[Course]] =
    target(Course.active_courses, courses) { (active_courses, courses) =>
      active_courses
        .filter(course => courses.matches(course.course_name))
        .sorted
    }

  // TODO: warn when no projects matched
  lazy val selected_projects: Target[Seq[Project]] =
    target(Project.active_projects, courses, projects) {
      (active_projects, courses, projects) =>
        for {
          p <- active_projects.toSeq
          if projects.matches(p.project_name)
          if courses.matches(p.course.course_name)
        } yield p
    }

  lazy val submissions: Target[Seq[(Project, CSID)]] = complex_target {
    val ps: Seq[Project] = selected_projects.guilty
    val csids_seq: Seq[SortedSet[CSID]] = {
      for { p <- ps } yield p.students_with_submission.guilty
    }
    val s = students.guilty

    run_if_needed {
      for {
        (p, csids) <- ps.zip(csids_seq)
        csid <- csids
        if s.matches(csid.value)
      } yield (p, csid)

    }

  }

  lazy val test_ids: Target[Seq[(Project, TestId)]] = complex_target {
    val ps: Seq[Project] = selected_projects.guilty

    val per_project_test_ids: Seq[SortedSet[TestId]] = for {
      p <- ps
    } yield p.test_ids.guilty

    val ts = tests.guilty

    run_if_needed {

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
    val ps: Seq[Project] = selected_projects.guilty

    val per_project_ids: Seq[(SortedSet[CSID], SortedSet[TestId])] =
      for {
        p: Project <- ps
      } yield (p.students_with_submission.guilty, p.test_ids.guilty)

    val the_students = students.guilty
    val the_tests = tests.guilty

    run_if_needed {
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
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        println(s"\n------ ${c.course_name} -------")
        println(upickle.default.write(c.dropbox.track.block, indent = 2))
      }
    }

  @main
  def bad_tests(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      val r = (for {
        p <- commonArgs.selected_projects.track.block
        s <- p.course.enrollment.track.block.keySet.toSeq
        if commonArgs.students.guilty.matches(s.value)
        r <- p.get_student_results(s).track.block.toSeq
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
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        println(s"\n------ ${c.course_name} -------")
        println(
          upickle.default
            .write(c.publish_keys.track.block.value.size, indent = 2)
        )
      }
    }

  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        println(s"\n------ ${c.course_name} -------")
        println(c.publish_enrollment.track.block.value.size)
        // println(upickle.default.write(c.publish_enrollment.value, indent=2))
      }
    }

  @main
  def enrollment(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        println(s"\n------ ${c.course_name} -------")
        println(c.enrollment.track.block.size)
        // println(upickle.default.write(c.publish_enrollment.value, indent=2))
      }
    }

  @main
  def create_grades_repos(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        println(s"Creating ${c.course_name}__grades")
        val _ = c.create_grades_repo.track.block
      }
    }

  @main
  def courses(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      println(commonArgs.selected_courses.track.block)
    }

  @main
  def history(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      Gitolite.history.track.block.foreach(println)
    }

  @main
  def info(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      Gitolite.info.track.block.foreach(println)
    }

  @main
  def projects(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      pprint.pprintln(commonArgs.selected_projects.track.block)
    }

  @main
  def overrides(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (p <- commonArgs.selected_projects.track.block) {
        println(p.publish_override_repo.track.block)
      }
    }

  @main
  def work_repos(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        for {
          (pn, p) <- c.active_projects.track.block
          if commonArgs.projects.track.block.matches(pn)
        } {
          for {
            (csid, _) <- c.enrollment.guilty
            if commonArgs.students.track.block.matches(csid.value)
          } {
            println(p.work_repo(csid).guilty)
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
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    val sort = sortMode match
      case AliasSortMode.CSID  => false
      case AliasSortMode.Alias => true

    for (c <- commonArgs.selected_courses.guilty) {
      val enrollment = c.enrollment.guilty

      for {
        (pn, p) <- c.active_projects.guilty
        if commonArgs.projects.guilty.matches(pn)
      } {
        val aliases = p.get_aliases.guilty
        val csids = enrollment.keys
          .filter((id: CSID) => commonArgs.students.guilty.matches(id.value))
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
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    for (p <- commonArgs.selected_projects.guilty) {
      val state = state_of(p)
      val tests = state.tests(phase)
      val ids = p.test_ids_for(tests).guilty
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
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    for (p <- commonArgs.selected_projects.guilty) {
      println(s"\n${p.course.course_name}_${p.project_name}:")
      for (test_id <- p.phase2_test_ids.guilty) {
        println(test_id.external_name)
      }
    }
  }

  @main
  def publish_tests(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    for (p <- commonArgs.selected_projects.guilty) {
      println(s"-- publishing tests for ${p.project_name}")
      pprint.pprintln(p.publish_tests.guilty)
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
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    val base = destination

    for (c <- commonArgs.selected_courses.track.block) {

      val enrollment = c.enrollment.track.block

      for {
        (pn, p) <- c.active_projects.track.block
        if commonArgs.projects.track.block.matches(pn)
      } {

        val aliases = p.get_aliases.guilty
        val tests = p.publish_tests.guilty
        val test_extensions = p.test_extensions.guilty

        for {
          (csid, _) <- enrollment
          if commonArgs.students.track.block.matches(csid.value)
        } {
          val info = p.prepare_info(csid, phase).guilty
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
                from = p.prepare(csid, data.sha).guilty.get_data_path,
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
                  tests.get_data_path,
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
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    val datetime_format = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val show_details = details.value

    for (c <- commonArgs.selected_courses.track.block) {
      val enrollment = c.enrollment.track.block

      for {
        (pn, p) <- c.active_projects.track.block
        if commonArgs.projects.track.block.matches(pn)
      } {
        val project_deadline = p.code_cutoff.track.block.format(datetime_format)

        val late_repos = enrollment
          .map((csid, _) => csid)
          .filter(csid => commonArgs.students.track.block.matches(csid.value))
          .map(csid => (csid, p.late_commits(csid).track.block.value))
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
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        println(c.course_name)
        for {
          (pn, p) <- c.active_projects.track.block
          if commonArgs.projects.track.block.matches(pn)
        } {
          println(s"    $pn")
          var count = 0
          for {
            (csid, _) <- c.enrollment.guilty
            if commonArgs.students.track.block.matches(csid.value)
          } {
            val _ = p.publish_work_repo(csid).guilty
            count = count + 1
          }
          println(s"            $count")
        }
      }
    }

  @main
  def submissions(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      pprint.pprintln(commonArgs.submissions.guilty)
      // for ((p, csid) <- commonArgs.submissions.guilty) {
      //  println(s"$p $csid")
      // }
    }

  @main
  def test_ids(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for ((p, test_id) <- commonArgs.test_ids.track.block) {
        println(s"${p.toString} ${test_id.toString}")
      }

    }

  @main
  def latest(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    Gitolite.latest.track.block.foreach(println)

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
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    val runs = commonArgs.runs.track.block
    // the phase's tests (as publish_results runs them), narrowed by -t
    val outs = Task.sequence {
      for {
        (p, csid, test_id) <- runs
        if p.test_ids_for(state_of(p).tests(phase)).guilty.contains(test_id)
        info <- p.prepare_info(csid, phase).guilty.toSeq
      } yield p
        .run_one(state_of(p).keeps_going(phase), commonArgs.count)(
          csid,
          info.sha,
          test_id
        )
        .track
    }.block

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
  private def state_of(p: Project)(using State): ProjectState = p.state.guilty

  @main
  def publish_results(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    val candidates = commonArgs.selected_projects.guilty
    def name(p: Project) = s"${p.course.course_name}_${p.project_name}"
    // the phases a project publishes, each with its test set and keep-going
    def phases(p: Project): Seq[(Phase, TestSet, Boolean)] = for {
      phase <- Seq(Phase.Latest, Phase.One, Phase.Two)
      state = state_of(p)
      if state.publishes(phase)
    } yield (phase, state.tests(phase), state.keeps_going(phase))
    // with no -p, grade one project with work to do, picked by priority (see
    // Priority)
    val named = commonArgs.projects_.regex != ".*"
    val projects =
      if (named || candidates.size <= 1) candidates
      else {
        val busy = candidates.filter { p =>
          val work = phases(p).exists((phase, _, _) =>
            p.has_work(phase, commonArgs.count).guilty
          )
          println(
            s"${name(p)}: ${state_of(p).description}; ${
                if (work) "work to do"
                else "up to date"
              }"
          )
          work
        }
        val picked = Priority
          .pick(busy.map(p => (p, p.project_name, state_of(p).is_live)))
          .toSeq
        if (picked.isEmpty) println("nothing to do")
        picked.foreach(p => println(s"picked ${name(p)}"))
        picked
      }
    for (p <- projects) {
      // the latest results, and each phase's once its deadline has passed;
      // the state and the phase say which tests, and whether to keep going
      phases(p).foreach { (phase, tests, kg) =>
        println(
          s"${name(p)} (${HtmlGen.phase_label(phase)}): ${state_of(p).description}; tests: ${tests: String}, keep-going ${
              if (kg) "on" else "off"
            }"
        )
        val _ = p.publish_results(kg, tests)(phase, commonArgs.count).guilty
      }
    }
  }

  @main
  def student_tests(
      commonArgs: CommonArgs
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    for (p <- commonArgs.selected_projects.guilty) {
      val _ =
        val out = p.student_tests_by_csid.guilty
        pprint.pprintln(out)
      // println(upickle.default.write(results, indent=2))
    }
  }

  @main
  def get_results(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        println(c.course_name)
        for {
          (pn, p) <- c.active_projects.guilty
          if commonArgs.projects.guilty.matches(pn)
        } {
          println(s"${c.course_name}:$pn")
          for {
            (csid, _) <- c.enrollment.guilty
            if commonArgs.students.guilty.matches(csid.value)
          } {
            val res = p.get_student_results(csid).guilty
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
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.track.block) {
        for {
          (pn, p) <- c.active_projects.guilty
          if commonArgs.projects.guilty.matches(pn)
        } {
          for {
            (csid, _) <- c.enrollment.guilty
            if commonArgs.students.guilty.matches(csid.value)
          } {
            // say(s"---> ${c.course_name}:$pn:$csid")
            p.notify_student_results(csid).guilty
          }
        }
      }
    }

  @main
  def gen_html(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.guilty) {
        for {
          (pn, p) <- c.active_projects.guilty
          if commonArgs.projects.guilty.matches(pn)
        } {

          HtmlGen(p).gen_html.guilty

        }
      }
    }

  @main
  def publish_site(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.guilty) {
        println(
          s"${c.site_repo_name}: ${c.publish_site.guilty.value.mkString(" ")}"
        )
      }
    }

  @main
  // With --force, also rebuilds the site repo (an input of the deploy), so
  // a change to how the site is put together reaches Cloudflare.
  def push_to_cloudflare(commonArgs: CommonArgs): Unit =
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      for (c <- commonArgs.selected_courses.guilty) {
        if (commonArgs.force.value) summon[State].force(c.publish_site)
        val url = c.push_to_cloudflare.guilty.value
        println(s"${c.course_name}: ${url.getOrElse("not deployed")}")
      }
    }

  @main
  def compute_scores(
      commonArgs: CommonArgs,
      @arg(doc =
        "grade phase 1 or 2: the commit its commit_id_<n> file names, else the last before its deadline (default: the latest)"
      )
      phase: Phase = Phase.Latest,
      minutes: Int = 1
  ): Unit = in_workspace(commonArgs.workspace, commonArgs.force.value) {
    for (p <- commonArgs.selected_projects.guilty) {
      val chosen = p.test_ids.guilty
      val weights = p.test_weights.guilty
      val test_weights = (for {
        c <- chosen
        w <- weights
        p = Regex(w.pattern)
        if p.matches(c.external_name)
      } yield (c, w.weight)).to(SortedMap)

      val max_weight = test_weights.values.sum

      val (n, outs) = loop(1 to commonArgs.count, minutes, Seq()) { (c, acc) =>
        val scores = (for {
          (csid, passing) <- p
            .compute_scores(phase, c)
            .guilty
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
          p.is_late(csid, phase).guilty
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
    given State = State(commonArgs.workspace, commonArgs.force.value)
    def courses: Array[Course] = commonArgs.selected_courses.guilty.toArray
    def projects: Array[Project] = commonArgs.selected_projects.guilty.toArray
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
    in_workspace(commonArgs.workspace, commonArgs.force.value) {
      pprint.pprintln(Gitolite.mirror("courses_config").guilty)
    }

  @main
  def pl(workspace: os.Path = os.pwd / "workspace", prefix: os.Path): Unit =
    in_workspace(workspace) {
      pprint.pprintln(PlTools(prefix).fstar.guilty)
    }

  // the command line, for telling a second run who holds the lock
  private var command_line = ""

  // Every command works in its workspace holding the workspace's lock: two
  // runs sharing a workspace would corrupt each other's state.
  private def in_workspace[A](workspace: os.Path, force: Boolean = false)(
      f: State ?=> A
  ): A = {
    os.makeDir.all(workspace)
    RunLock(workspace, command_line) {
      ag.rules.use_std_out_err(workspace / "std_out_err")
      State.run(workspace, force)(f)
    }
  }

  def main(args: Array[String]): Unit = {
    command_line = s"run ${args.mkString(" ")}"
    val _ = ParserForMethods(this).runOrExit(args.toIndexedSeq)
  }
}
