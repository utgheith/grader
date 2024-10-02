import ag.grader.{
  Course,
  CSID,
  CutoffTime,
  HtmlGen,
  OutcomeStatus,
  Project,
  TestId
}
import ag.rules.{
  Maker,
  NopStateMonitor,
  RuleBase,
  State,
  given_ReadWriter_SortedMap,
  say
}
import mainargs.{ParserForClass, ParserForMethods, TokensReader, arg, main}

import scala.collection.SortedMap
import scala.util.matching.Regex
import scala.collection.SortedSet

import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import ag.rules.SignedPath
import ag.grader.Outcome

class MyMonitor extends NopStateMonitor {

  override def onStart(s: State, rule: RuleBase): Unit = {
    // println(s"********** [${Thread.currentThread().nn.getName}] ${rule.path}")
  }
  override def onCompute(s: State, rule: RuleBase, in: Any): Unit = {
    say(rule.path)
  }
}

given TokensReader.Simple[os.Path] with {
  override def shortName: String = "path"

  override def read(strs: Seq[String]): Either[String, os.Path] = if (
    strs.isEmpty
  ) {
    Right(os.pwd / "workspace")
  } else {
    val one = strs.mkString("/")
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

  override def read(strs: Seq[String]): Either[String, Regex] = if (
    strs.isEmpty
  ) {
    Right(""".*""".r)
  } else {
    Right(Regex(strs.mkString("|")))
  }
}

given TokensReader.Simple[CutoffTime] with {
  override def shortName: String = "datetime"
  override def allowEmpty: Boolean = true
  override def alwaysRepeatable: Boolean = false

  override def read(strs: Seq[String]): Either[String, CutoffTime] =
    Right(CutoffTime.fromString(strs.headOption))
}

enum AliasSortMode:
  case CSID
  case Alias

given TokensReader.Simple[AliasSortMode] with {
  override def shortName: String = "mode"
  override def allowEmpty: Boolean = false
  override def alwaysRepeatable: Boolean = false

  override def read(strs: Seq[String]): Either[String, AliasSortMode] =
    strs.head match
      case "alias" => Right(AliasSortMode.Alias)
      case "csid"  => Right(AliasSortMode.CSID)
      case s =>
        Left(s"Invalid sort mode '${s}'; possible modes are 'alias' or 'csid'")
}

//@main
case class CommonArgs(
    @arg(short = 'c')
    courses: Regex = """.*""".r,
    @arg(short = 'p')
    projects: Regex = """.*""".r,
    @arg(short = 's')
    students: Regex = """.*""".r,
    @arg(short = 't')
    tests: Regex = """.*""".r,
    @arg(short = 'n')
    count: Int = 1,
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
    } yield p.test_ids)
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
    } yield p.test_ids)
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
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(upickle.default.write(c.dropbox.value, indent = 2))
    }
  }

  @main
  def publish_keys(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(upickle.default.write(c.publish_keys.value.data.size, indent = 2))
    }
  }

  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(c.publish_enrollment.value.data.size)
      // println(upickle.default.write(c.publish_enrollment.value, indent=2))
    }
  }

  @main
  def enrollment(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(c.enrollment.value.size)
      // println(upickle.default.write(c.publish_enrollment.value, indent=2))
    }
  }

  @main
  def courses(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    println(commonArgs.selected_courses.value)
  }

  @main
  def projects(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    println(commonArgs.selected_projects.value)
  }

  @main
  def overrides(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (p <- commonArgs.selected_projects.value) {
      println(p.publish_override_repo.value)
    }
  }

  @main
  def work_repos(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
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
    val m = MyMonitor()
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
        val csids = enrollment
          .map(_._1)
          .filter((id: CSID) => commonArgs.students.matches(id.value))
          .toIndexedSeq
        val sorted =
          if (sort) csids.sortBy(aliases.get(_).map(_.value)) else csids

        val base_name = s"${c.course_name}_${pn}"
        val longest_csid = sorted.maxBy(_.value.length)
        val max_width = longest_csid.value.length + base_name.length + 1

        println(s"\n${base_name}:")

        for (csid <- sorted) {
          val target_name = s"${base_name}_${csid}"
          val padded_name = String.format(s"%-${max_width}s", target_name)
          println(s"${padded_name}  ${aliases.getOrElse(csid, "?")}")
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
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)

    val base = os.pwd / "prepared"

    for (c <- commonArgs.selected_courses.value) {

      val enrollment = c.enrollment.value

      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {

        val aliases = p.get_aliases.value

        for {
          (csid, _) <- enrollment
          if commonArgs.students.matches(csid.value)
        } {
          val prep = p.prepare(csid, cutoff).value
          val target_name = s"${c.course_name}_${pn}_${csid}"
          val target_path = base / target_name

          prep.data match {
            case Some(data) =>
              println(
                s"${aliases.getOrElse(csid, "?")} $target_name ${data.commit_time
                    .withZoneSameInstant(ZoneId.systemDefault)} ${data.sha}"
              )
              os.copy.over(
                from = prep.path,
                to = target_path,
                followLinks = false,
                replaceExisting = true,
                createFolders = true
              )
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
      cutoff: CutoffTime
  ): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)

    val datetime_format = DateTimeFormatter.ISO_LOCAL_DATE_TIME

    for (c <- commonArgs.selected_courses.value) {
      val enrollment = c.enrollment.value

      for {
        (pn, p) <- c.active_projects.value
        if commonArgs.projects.matches(pn)
      } {
        val project_deadline = p.code_cutoff.value.format(datetime_format)

        val late_repos = enrollment
          .map((csid, _) => csid)
          .filter((csid) => commonArgs.students.matches(csid.value))
          .map((csid) => (csid, p.late_commits(csid, cutoff).value.data))
          .filter((_, late_commits) => !late_commits.isEmpty)
          .toSeq

        println(
          s"\nFound ${late_repos.length} repositories with late commits for ${c.course_name}_${pn}"
        )
        println(s"Using deadline: ${project_deadline}\n")

        for ((csid, late_commits) <- late_repos) {
          val target_name = s"${c.course_name}_${pn}_${csid}"
          println(target_name)
          for (commit <- late_commits) {
            val time_str = commit.time
              .withZoneSameInstant(ZoneId.systemDefault)
              .format(datetime_format)

            val days = commit.delay.toDays()
            val hours = commit.delay.toHours() % 24
            val minutes = commit.delay.toMinutes() % 60
            val seconds = commit.delay.getSeconds() % 60
            val duration_str = days match
              case 0 => f"$hours%02d:$minutes%02d:$seconds%02d"
              case 1 => f"$days%d day, $hours%02d:$minutes%02d:$seconds%02d"
              case _ => f"$days%d days, $hours%02d:$minutes%02d:$seconds%02d"

            val message = commit.message.length > 64 match
              case true  => commit.message.take(61) ++ "..."
              case false => commit.message

            println(
              s"| ${time_str} (${duration_str} late); ${commit.hash.take(8)} ${message}"
            )
          }
        }
      }
    }
  }

  @main
  def publish_work_repos(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
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
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for ((p, csid) <- commonArgs.submissions.value) {
      println(s"$p $csid")
    }
  }

  @main
  def test_ids(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for ((p, test_id) <- commonArgs.test_ids.value) {
      println(s"$p $test_id")
    }

  }

  @main
  def run(
      commonArgs: CommonArgs,
      @arg(
        name = "code-cutoff",
        doc =
          "The cutoff for the code; either an ISO-8601 datetime, 'default', or 'none'.  Defaults to 'none'."
      )
      cutoff: CutoffTime
  ): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)

    val limit = System.currentTimeMillis() + 12 * 60 * 60 * 1000 // 12 hours

    @tailrec
    def loop(c: Int, in: Seq[SignedPath[Outcome]]): Seq[SignedPath[Outcome]] =
      if (c <= commonArgs.count && System.currentTimeMillis() < limit) {

        val outcomes = for {
          runs <- commonArgs.runs
          out <- Maker.sequence {
            for ((p, csid, test_id) <- runs)
              yield p.run(csid, cutoff, test_id, c)
          }
        } yield out

        val out = outcomes.value

        println(
          s"---------------------------> finished iteration #$c/${commonArgs.count}"
        )
        loop(c + 1, out)

      } else {
        in
      }

    val outs = loop(1, Seq())
    for {
      outcome <- outs.map(_.data)
      if !outcome.outcome.contains(OutcomeStatus.Pass)
    } {
      println(
        s"${outcome.test_id.external_name} ${outcome.outcome} ${outcome.tries}"
      )
    }

  }

  @main
  def publish_results(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)

    for (p <- commonArgs.selected_projects.value) {
      val _ = p.publish_results(commonArgs.count).value
      // println(upickle.default.write(results, indent=2))
    }
  }

  @main
  def get_results(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
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
            val pass = res.outcomes.values
              .filter(_.outcome == Some(OutcomeStatus.Pass))
              .size
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
    val m = MyMonitor()
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
    val m = MyMonitor()
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

  def main(args: Array[String]): Unit = {
    ParserForMethods(this).runOrExit(args.toIndexedSeq)
  }
}
