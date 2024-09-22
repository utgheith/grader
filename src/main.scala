import ag.grader.{Course, CSID, HtmlGen, Project, TestId}
import ag.rules.{Maker, NopStateMonitor, RuleBase, State, given_ReadWriter_SortedMap, say}
import mainargs.{ParserForClass, ParserForMethods, TokensReader, arg, main}

import scala.collection.SortedMap
import scala.util.matching.Regex
import scala.collection.SortedSet

import java.time.ZoneId
import scala.annotation.tailrec

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

  override def read(strs: Seq[String]): Either[String, os.Path] = if (strs.isEmpty) {
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
  
  override def read(strs: Seq[String]): Either[String, Regex] = if (strs.isEmpty) {
    Right(""".*""".r)
  } else {
    Right(Regex(strs.mkString("|")))
  }
}

//@main
case class CommonArgs(
    @arg(short='c')
    courses: Regex = """.*""".r,
    @arg(short='p')
    projects: Regex = """.*""".r,
    @arg(short='s')
    students: Regex = """.*""".r,
    @arg(short='t')
    tests: Regex = """.*""".r,
    @arg(short='n')
    count: Int = 1,
    workspace: os.Path = os.pwd / "workspace") {

  lazy val selected_courses: Maker[Seq[Course]] = for {
    courses <- Course.active_courses
    selected_courses = for {
      course <- courses
      if this.courses.matches(course.course_name)
    } yield course
  } yield selected_courses

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
    if tests.matches(test_id.external_name) || tests.matches(test_id.internal_name)
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
    ((p, csids), test_ids) <- projects.zip(per_project_csids).zip(per_project_test_ids)
    the_test_ids = test_ids.filter(id => tests.matches(id.external_name) || tests.matches(id.internal_name))
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
      println(upickle.default.write(c.dropbox.value, indent=2))
    }
  }

  @main
  def publish_keys(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(upickle.default.write(c.publish_keys.value.data.size, indent=2))
    }
  }

  @main
  def publish_enrollment(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(c.publish_enrollment.value.data.size)
      //println(upickle.default.write(c.publish_enrollment.value, indent=2))
    }
  }

  @main
  def enrollment(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)
    for (c <- commonArgs.selected_courses.value) {
      println(s"\n------ ${c.course_name} -------")
      println(c.enrollment.value.size)
      //println(upickle.default.write(c.publish_enrollment.value, indent=2))
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
  def prepare(commonArgs: CommonArgs): Unit = {
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
          val prep = p.prepare(csid).value
          val target_name = s"${c.course_name}_${pn}_${csid}"
          val target_path = base / target_name

          prep.data match {
            case Some(data) =>
              println(s"${aliases.getOrElse(csid, "?")} $target_name ${data.commit_time.withZoneSameInstant(ZoneId.systemDefault)} ${data.sha}")
              os.copy.over(
                from=prep.path,
                to=target_path,
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
    for ((p,csid) <- commonArgs.submissions.value) {
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
  def run(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)

    val limit = System.currentTimeMillis() + 5 * 60 * 1000

    @tailrec
    def loop(c: Int): Unit = if (c <= commonArgs.count && System.currentTimeMillis() < limit) {

      val outcomes = for {
        runs <- commonArgs.runs
        out <- Maker.sequence {
          for (
            (p, csid, test_id) <- runs
          ) yield p.run(csid, test_id, c)
        }
      } yield out

      val _ = outcomes.value

      println(s"---------------------------> finished iteration #$c/${commonArgs.count}")
      loop(c+1)

    }

    loop(1)

  }

  @main
  def publish_results(commonArgs: CommonArgs): Unit = {
    val m = MyMonitor()
    given State = State.of(commonArgs.workspace, m)

    for (p <- commonArgs.selected_projects.value) {
      val _ = p.publish_results(commonArgs.count).value
      //println(upickle.default.write(results, indent=2))
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
            val pass = res.outcomes.values.filter(_.outcome.contains("pass")).size
            println(s"${c.course_name}:$pn:$csid:${res.alias.getOrElse("")}:$pass/$count")
            
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
          //say(s"---> ${c.course_name}:$pn:$csid")
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



