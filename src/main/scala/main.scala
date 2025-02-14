import ag.common.given_ReadWriter_SortedMap
import ag.git.git
import ag.grader.{
  CSID,
  Course,
  CutoffTime,
  Gitolite,
  HtmlGen,
  OutcomeStatus,
  Project,
  TestId
}
import ag.rules.{Maker, NopStateMonitor, RuleBase, State, human, say, timed}
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
import scala.annotation.tailrec
import ag.rules.SignedPath
import ag.grader.Outcome

class MyMonitor(commonArgs: CommonArgs) extends NopStateMonitor {

  override def onStart(s: State, rule: RuleBase): Unit = {
    // println(s"********** [${Thread.currentThread().nn.getName}] ${rule.path}")
  }
  override def onCompute(s: State, rule: RuleBase, in: Any): Unit = {
    if (commonArgs.verbose.value) {
      say(rule.path)
    }
  }
}

given TokensReader.Simple[os.Path] with {
  override def shortName: String = "path"

  /** read
    */
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

  /** read
    */
  override def read(strings: Seq[String]): Either[String, Regex] = if (
    strings.isEmpty
  ) {
    Right(""".*""".r)
  } else {
    Right(Regex(strings.mkString("|")))
  }
}

given TokensReader.Simple[CutoffTime] with {
  override def shortName: String = "datetime"
  override def allowEmpty: Boolean = true
  override def alwaysRepeatable: Boolean = false

  /** read
    */
  override def read(strings: Seq[String]): Either[String, CutoffTime] =
    Right(CutoffTime.fromString(strings.headOption))
}

enum AliasSortMode:
  case CSID
  case Alias

given TokensReader.Simple[AliasSortMode] with {
  override def shortName: String = "mode"
  override def allowEmpty: Boolean = false
  override def alwaysRepeatable: Boolean = false

  /** read
    */
  override def read(strings: Seq[String]): Either[String, AliasSortMode] =
    strings.head match
      case "alias" => Right(AliasSortMode.Alias)
      case "csid"  => Right(AliasSortMode.CSID)
      case s =>
        Left(s"Invalid sort mode '$s'; possible modes are 'alias' or 'csid'")
}

//@main
case class CommonArgs(
    @arg(short = 'c', doc = "courses to consider (regex)")
    courses: Regex = """.*""".r,
    @arg(short = 'p', doc = "projects to consider (regex)")
    projects: Regex = """.*""".r,
    @arg(short = 's', doc = "students to consider (regex)")
    students: Regex = """.*""".r,
    @arg(short = 't', doc = "tests to consider (regex)")
    tests: Regex = """.*""".r,
    @arg(short = 'n', doc = "how many iterations?")
    count: Int = 1,
    @arg(short = 'o', doc = "restrict to chosen tests")
    only_chosen: Flag = Flag(false),
    @arg(short = 'v', doc = "verbose output")
    verbose: Flag = Flag(false),
    workspace: os.Path = os.pwd / "workspace"
) {

  // TODO: warn when no courses matched
  lazy val selected_courses: Maker[Seq[Course]] = for {
    courses <- Course.active_courses
    selected_courses = for {
      course <- courses
      if this.courses.matches(course.course_name)
    } yield course
  } yield selected_courses

  // TODO: warn when no projects matched
  lazy val selected_projects: Maker[Seq[Project]] = for {
    // <- Maker
    selected_courses: Seq[Course] <- this.selected_courses
    active_projects: Seq[Seq[Project]] <- Maker.sequence(for {
      course <- selected_courses
    } yield course.active_projects.map(ps => ps.values.toSeq))
    selected_projects: Seq[Project] = for {
      sp <- active_projects
      p <- sp
      if this.projects.matches(p.project_name)
    } yield p
  } yield selected_projects

  lazy val submissions: Maker[Seq[(Project, CSID)]] = for {
    projects: Seq[Project] <- selected_projects
    per_project_csids <- Maker.sequence(for {
      project: Project <- projects
    } yield project.students_with_submission)
  } yield for {
    (p, csids) <- projects.zip(per_project_csids)
    csid <- csids.toSeq
    if students.matches(csid.value)
  } yield (p, csid)

  lazy val test_ids: Maker[Seq[(Project, TestId)]] = for {
    projects: Seq[Project] <- selected_projects
    per_project_test_ids: Seq[SortedSet[TestId]] <- Maker.sequence(for {
      p <- projects
    } yield if (only_chosen.value) p.chosen_test_ids else p.test_ids)
  } yield for {
    (p, test_ids) <- projects.zip(per_project_test_ids)
    test_id <- test_ids
    if tests.matches(test_id.external_name) || tests.matches(
      test_id.internal_name
    )
  } yield (p, test_id)

  lazy val runs: Maker[Seq[(Project, CSID, TestId)]] = for {
    projects: Seq[Project] <- selected_projects
    per_project_csids <- Maker.sequence(for {
      project: Project <- projects
    } yield project.students_with_submission)
    per_project_test_ids: Seq[SortedSet[TestId]] <- Maker.sequence(for {
      p <- projects
    } yield if (only_chosen.value) p.chosen_test_ids else p.test_ids)
  } yield for {
    ((p, csids), test_ids) <- projects
      .zip(per_project_csids)
      .zip(per_project_test_ids)
    the_test_ids = test_ids.filter(id =>
      tests.matches(id.external_name) || tests.matches(id.internal_name)
    )
    if the_test_ids.nonEmpty
    csid <- csids.toSeq
    if students.matches(csid.value)
    test_id <- the_test_ids
  } yield (p, csid, test_id)
}

object CommonArgs {
  given ParserForClass[CommonArgs] = ParserForClass[CommonArgs]
}

object Main {
  @main
  def dropbox(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(upickle.default.write(c.dropbox.value, indent = 2))
    }
  }

  @main
  def bad_tests(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    val r = (for {
      p <- commonArgs.selected_projects.value
      s <- p.course.enrollment.value.keySet.toSeq
      if commonArgs.students.matches(s.value)
      r <- p.get_student_results(s).value.toSeq
      (test, outcome) <- r.outcomes.toSeq
    } yield (p, s, test, outcome)).groupMapReduce {
      case (p, s, test, outcome) => (p, test)
    } { case (p, s, test, outcome) =>
      !outcome.outcome.contains(OutcomeStatus.pass)
    }(_ && _)

    r.filter { case (_, v) => v }.keys.toList.sorted.foreach { case (p, t) =>
      println(s"${p.course.course_name}_${p.project_name}:${t.external_name}")
    }
  }

  @main
  def publish_keys(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(upickle.default.write(c.publish_keys.value.data.size, indent = 2))
    }
  }

  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(c.publish_enrollment.value.data.size)
      // println(upickle.default.write(c.publish_enrollment.value, indent=2))
    }
  }

  @main
  def enrollment(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(c.enrollment.value.size)
      // println(upickle.default.write(c.publish_enrollment.value, indent=2))
    }
  }

  @main
  def create_grades_repos(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"Creating ${c.course_name}__grades")
      val _ = c.create_grades_repo.value
    }
  }

  @main
  def courses(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    println(commonArgs.selected_courses.value)
  }

  @main
  def history(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    Gitolite.history.value.foreach(println)
  }

  @main
  def info(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    Gitolite.info.value.foreach(println)
  }

  @main
  def projects(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    println(commonArgs.selected_projects.value)
  }

  @main
  def overrides(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (p <- commonArgs.selected_projects.value) {
      println(p.publish_override_repo.value)
    }
  }

  @main
  def work_repos(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {
        for {
          (csid, _) <- c.enrollment.value
          if commonArgs.students.matches(csid.value)
        } {
          println(p.work_repo(csid).value)
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
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    val sort = sortMode match
      case AliasSortMode.CSID  => false
      case AliasSortMode.Alias => true

    for (c <- commonArgs.selected_courses.value) {
      val enrollment = c.enrollment.value

      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {
        val aliases = p.get_aliases.value
        val csids = enrollment.keys
          .filter((id: CSID) => commonArgs.students.matches(id.value))
          .toIndexedSeq
        val sorted =
          if (sort) csids.sortBy(aliases.get(_).map(_.value)) else csids

        val base_name = s"${c.course_name}_$pn"
        val longest_csid = sorted.maxBy(_.value.length)
        val max_width = longest_csid.value.length + base_name.length + 1

        println(s"\n$base_name:")

        for (csid <- sorted) {
          val target_name = s"${base_name}_$csid"
          val padded_name = String.format(s"%-${max_width}s", target_name)
          println(s"$padded_name  ${aliases.getOrElse(csid, "?")}")
        }
      }
    }
  }

  @main
  def prepare(
      commonArgs: CommonArgs,
      @arg(
        name = "code-cutoff",
        doc =
          "The cutoff for the code; either an ISO-8601 datetime, 'default', or 'none'.  Defaults to 'none'."
      )
      cutoff: CutoffTime
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    val base = os.pwd / "prepared"

    for (c <- commonArgs.selected_courses.value) {

      val enrollment = c.enrollment.value

      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {

        val aliases = p.get_aliases.value
        val tests = p.publish_tests.value
        val test_extensions = p.test_extensions.value

        for {
          (csid, _) <- enrollment
          if commonArgs.students.matches(csid.value)
        } {
          val prep = p.prepare(csid, cutoff).value
          val target_name = s"${c.course_name}_${pn}_$csid"
          val target_path = base / target_name

          prep.data match {
            case Some(data) =>
              println(
                s"${aliases.getOrElse(csid, "?")} $target_name ${data.commit_time
                    .withZoneSameInstant(ZoneId.systemDefault)} ${data.sha} ${data.push_time
                    .map(ins => ins.atZone(ZoneId.systemDefault()))}"
              )
              os.copy.over(
                from = prep.path,
                to = target_path,
                followLinks = false,
                replaceExisting = true,
                createFolders = true
              )

              for {
                (_, test_info) <- tests.data
              } {
                p.copy_test(test_info, tests.path, target_path, test_extensions)
              }
            case None =>
          }
        }
      }
    }

    println(s"copied to $base")
  }

  @main
  def late_commits(
      commonArgs: CommonArgs,
      @arg(
        name = "code-cutoff",
        doc =
          "The cutoff for the code; either an ISO-8601 datetime, 'default', or 'none'.  Defaults to 'none'."
      )
      cutoff: CutoffTime,
      @arg(
        short = 'd',
        doc = "Show repository names, commit hashes, and commit messages"
      )
      details: Flag,
      @arg(
        doc = "Sort all commits by date rather than grouping commits by repo"
      )
      sort: Flag
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    val datetime_format = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val show_details = details.value

    for (c <- commonArgs.selected_courses.value) {
      val enrollment = c.enrollment.value

      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {
        val project_deadline = p.code_cutoff.value.format(datetime_format)

        val late_repos = enrollment
          .map((csid, _) => csid)
          .filter(csid => commonArgs.students.matches(csid.value))
          .map(csid => (csid, p.late_commits(csid, cutoff).value.data))
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
              if show_details then s"${c.course_name}_${pn}_$csid"
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
  def publish_work_repos(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(c.course_name)
      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {
        println(s"    $pn")
        var count = 0
        for {
          (csid, _) <- c.enrollment.value
          if commonArgs.students.matches(csid.value)
        } {
          val _ = p.publish_work_repo(csid).value
          count = count + 1
        }
        println(s"            $count")
      }
    }
  }

  @main
  def submissions(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for ((p, csid) <- commonArgs.submissions.value) {
      println(s"$p $csid")
    }
  }

  @main
  def test_ids(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for ((p, test_id) <- commonArgs.test_ids.value) {
      println(s"$p $test_id")
    }

  }

  @main
  def latest(
      commonArgs: CommonArgs
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    Gitolite.latest.value.foreach(println)

  }

  private def show_failures(out: Seq[SignedPath[Outcome]]): Unit = {
    out
      .map(_.data)
      .filterNot(_.outcome.contains(OutcomeStatus.pass))
      .groupBy(_.csid)
      .to(SortedMap)
      .foreach { (csid, s) =>
        println(csid)
        s.groupBy(_.project.course.course_name).to(SortedMap).foreach {
          (c, s) =>
            println(s"  $c")
            s.groupBy(_.project.project_name).to(SortedMap).foreach { (p, s) =>
              println(s"    ${c}_$p")
              s.sortBy(_.test_id.external_name).foreach { o =>
                println(
                  s"        ${o.test_id.external_name} ${o.outcome} ${o.tries} ${o.time}"
                )
              }
            }
        }
      }
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

  // Run up to "commonArgs.count" iterations.
  // Stop early if total time exceeds "minutes"
  // Stop running a specific submission/test combination once it fails
  // Returns: number of iterations
  private def do_run(
      commonArgs: CommonArgs,
      cutoff: CutoffTime,
      minutes: Int,
      loud: Boolean
  )(using
      State
  ): Int = {
    loop(1 to commonArgs.count, minutes, 1) { (c, _) =>
      val outcomes = for {
        runs <- commonArgs.runs
        out <- Maker.sequence {
          for ((p, csid, test_id) <- runs)
            yield p.run(csid, cutoff, test_id, c)
        }
      } yield out
      val out = outcomes.value
      if (loud) show_failures(out)
      c
    }._1
  }

  @main
  def run(
      commonArgs: CommonArgs,
      @arg(
        name = "code-cutoff",
        doc =
          "The cutoff for the code; either an ISO-8601 datetime, 'default', or 'none'.  Defaults to 'none'."
      )
      cutoff: CutoffTime,
      @arg(doc = "maximum number of minutes per iteration")
      minutes: Int = 10
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    val _ = do_run(commonArgs, cutoff, minutes, false)
  }

  @main
  def run_all(
      commonArgs: CommonArgs,
      @arg(
        name = "code-cutoff",
        doc =
          "The cutoff for the code; either an ISO-8601 datetime, 'default', or 'none'.  Defaults to 'none'."
      )
      cutoff: CutoffTime,
      @arg(
        name = "result-file",
        doc = "The json file to write the results summary out to"
      )
      result_file: Option[String]
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    val limit = System.currentTimeMillis() + 12 * 60 * 60 * 1000 // 12 hours

    @tailrec
    def loop(c: Int, in: Seq[SignedPath[Outcome]]): Seq[SignedPath[Outcome]] =
      if (c <= commonArgs.count && System.currentTimeMillis() < limit) {

        val outcomes = for {
          runs <- commonArgs.runs
          out <- Maker.sequence {
            for ((p, csid, test_id) <- runs)
              yield p.run_one(csid, cutoff, test_id, c)
          }
        } yield out

        val out = outcomes.value

        show_failures(out)

        println(
          s"---------------------------> finished iteration #$c/${commonArgs.count}"
        )
        println(s"max memory ${Runtime.getRuntime.maxMemory()}")
        println(s"free memory ${Runtime.getRuntime.freeMemory()}")

        loop(c + 1, in ++ out)

      } else {
        in
      }

    val outs = loop(1, Seq())

    result_file.foreach(file_name => {
      val results =
        outs
          .groupBy(_.data.csid)
          .map((csid, signed_paths) => {
            val outcomes = signed_paths.map(_.data)
            val test_results = outcomes
              .groupBy(_.test_id)
              .map((test_id, runs) => {
                val passed = runs.count(_.outcome.contains(OutcomeStatus.pass))
                val total = runs.size
                (test_id.external_name, ujson.Arr(passed, total))
              })
              .toMap()
            val commit_id = outcomes.head.commit_id.getOrElse("")
            (
              csid.toString(),
              Map(
                "commit_id" -> ujson.Str(commit_id),
                "results" -> ujson.Obj.from(test_results)
              )
            )
          })
          .toMap()

      val path =
        if (file_name.charAt(0) == '/') os.Path(file_name)
        else os.pwd / os.RelPath(file_name)
      os.write(
        path,
        upickle.default.write(results, indent = 2)
      )
    })

  }

  @main
  def publish_results(
      commonArgs: CommonArgs,
      cutoff: CutoffTime,
      minutes: Int = 10
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    val c = do_run(commonArgs, cutoff, minutes, false)

    for (p <- commonArgs.selected_projects.value) {
      val _ = p.publish_results(c).value
      // println(upickle.default.write(results, indent=2))
    }
  }

  @main
  def get_results(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(c.course_name)
      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {
        println(s"${c.course_name}:$pn")
        for {
          (csid, _) <- c.enrollment.value
          if commonArgs.students.matches(csid.value)
        } {
          val res = p.get_student_results(csid).value
          res.foreach { res =>
            val count = res.outcomes.size
            val pass =
              res.outcomes.values.count(_.outcome.contains(OutcomeStatus.pass))
            println(
              s"${c.course_name}:$pn:$csid:${res.alias.getOrElse("")}:$pass/$count"
            )
          }
        }
      }
    }
  }

  @main
  def notify_results(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {
        for {
          (csid, _) <- c.enrollment.value
          if commonArgs.students.matches(csid.value)
        } {
          // say(s"---> ${c.course_name}:$pn:$csid")
          p.notify_student_results(csid).value
        }
      }
    }
  }

  @main
  def gen_html(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {

        HtmlGen(p).gen_html.value

      }
    }
  }

  @main
  def compute_scores(
      commonArgs: CommonArgs,
      cutoff_time: CutoffTime,
      minutes: Int = 1
  ): Unit = {
    val m = MyMonitor(commonArgs)
    given State = State.of(commonArgs.workspace, m)

    for (p <- commonArgs.selected_projects.value) {
      val chosen = p.chosen_test_ids.value
      val weights = p.weights.value
      val test_weights = (for {
        c <- chosen
        external_name = c.external_name
        w <- weights
        p = Regex(w.pattern)
        if p.matches(c.external_name)
      } yield (c, w.weight)).to(SortedMap)

      val max_weight = test_weights.values.sum

      val (n, outs) = loop(1 to commonArgs.count, minutes, Seq()) { (c, acc) =>
        val scores = (for {
          (csid, passing) <- p.compute_scores(cutoff_time, c).value.toSeq
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
        val is_late = p.is_late(csid, cutoff_time).value
        val score =
          if (is_late) raw_score.toDouble / 2.0 else raw_score.toDouble
        println(
          f"    $csid $score ${100.0 * (score / total.toDouble)}%.02f ${
              if (is_late) "late" else "ontime"
            }"
        )
      }
    }
  }

  @main
  def play(commonArgs: CommonArgs): Unit = {
    pprint.pprintln(git().notes().call())
  }

  def main(args: Array[String]): Unit = {
    val _ = ParserForMethods(this).runOrExit(args.toIndexedSeq)
  }
}
