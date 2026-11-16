package ag.grader

import ag.web.{WebPage, WebPages}
import ag.rules.{State, Target, Tree}

import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.collection.SortedMap
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

// The grading status, from what the grader has committed (results, aliases,
// submissions), never by running anything: courses, then a course's projects
// and each milestone's progress, then a milestone's students, a student's tests, a
// test's runs, and a run. Served at /status/ (see WebServer). Its data are
// `data`'s targets, looked up in `state()`: a read-only State, refreshed now
// and then (see ReadOnlyStates).
final class Status(data: StatusData, state: () => State) extends WebPages {
  import StatusData.{CommitRow, TestCommit}

  val prefix = "status"

  ///////////// what's committed /////////////

  private def look[A](t: Target[A]): A = state().track(t)

  // all of them at once, each on its own thread
  private def look_all[A](ts: Seq[Target[A]]): Seq[A] = state().track_all(ts)

  private def courses: SortedMap[CourseName, RawCourse] = look(data.courses)

  private def project_repo(c: CourseName, p: ProjectName) =
    data.project_repo(c, p)

  // a milestone's published results, by alias, and when they were committed
  private def results(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone
  ): Either[String, Option[
    (SortedMap[Alias, RedactedStudentResults], Option[String])
  ]] = {
    val r = look(data.milestone_results(c, p, milestone))
    r.error.toLeft(r.results.map(_ -> r.committed))
  }

  private def aliases(c: CourseName, p: ProjectName): SortedMap[CSID, Alias] =
    look(
      data.aliases(c, p)
    )

  // a student's own results (the current milestone's), and the files beside them: its
  // tests' output
  private def own_results(
      c: CourseName,
      p: ProjectName,
      csid: CSID
  ): Option[(StudentResults, Tree)] =
    look(data.own_results(c, p, csid))

  private def test_commit(
      c: CourseName,
      p: ProjectName,
      csid: CSID,
      milestone: Milestone
  ): Option[TestCommit] =
    look(data.test_commit(c, p, csid, milestone))

  ///////////// working it out /////////////

  private def project_state(rp: RawProject): ProjectState =
    ProjectState.at(LocalDateTime.now().nn, rp.milestones)

  // runs a test still has to go, as run_one decides: up to the most tries
  // (iterations.max), unless it failed and the milestone doesn't keep going
  private def remaining(o: OutcomeApi, keep_going: Boolean, max: Int): Int =
    if (
      o.outcomes.size < max && (keep_going || o.outcomes.forall(
        _._1 == OutcomeStatus.pass
      ))
    )
      max - o.outcomes.size
    else 0

  private case class Progress(
      students: Int,
      tests: Int,
      runs: Int,
      to_go: Int,
      passed: Int,
      // student-test pairs: all of their runs passed, and how many there are
      happy: Int,
      pairs: Int
  ) {
    def rate: String = percent(passed, runs)
  }

  private def progress(
      outcomes: Iterable[Iterable[OutcomeApi]],
      keep_going: Boolean,
      max: Int
  ): Progress = {
    val all = outcomes.flatten
    Progress(
      students = outcomes.size,
      tests = outcomes.map(_.size).maxOption.getOrElse(0),
      runs = all.map(_.outcomes.size).sum,
      to_go = all.map(remaining(_, keep_going, max)).sum,
      passed = all.map(_.outcomes.count(_._1 == OutcomeStatus.pass)).sum,
      happy = all.count(_.is_happy),
      pairs = all.size
    )
  }

  ///////////// pages /////////////

  def page(path: String): Option[WebPage] = {
    val parts = path.split('/').toSeq.filter(_.nonEmpty).map(decode)
    val html =
      try {
        parts match {
          case Seq()                   => Some(courses_page())
          case Seq(c)                  => course_page(CourseName(c))
          case Seq(c, "by-student")    => course_students_page(CourseName(c))
          case Seq(c, "by-student", s) =>
            course_student_page(CourseName(c), CSID(s))
          case Seq(c, p) => project_page(CourseName(c), ProjectName(p))
          case Seq(c, p, "by-student") =>
            project_students_page(CourseName(c), ProjectName(p))
          case Seq(c, p, "by-test") =>
            project_tests_page(CourseName(c), ProjectName(p))
          case Seq(c, p, milestone) =>
            milestone_of(c, p, milestone).flatMap(
              milestone_page(CourseName(c), ProjectName(p), _)
            )
          case Seq(c, p, milestone, "by-test") =>
            milestone_of(c, p, milestone).flatMap(
              tests_page(CourseName(c), ProjectName(p), _)
            )
          case Seq(c, p, milestone, "by-test", t) =>
            milestone_of(c, p, milestone).flatMap(
              one_test_page(CourseName(c), ProjectName(p), _, t)
            )
          case Seq(c, p, milestone, s) =>
            milestone_of(c, p, milestone).flatMap(
              student_page(CourseName(c), ProjectName(p), _, CSID(s))
            )
          case Seq(c, p, milestone, s, t) =>
            milestone_of(c, p, milestone).flatMap(
              test_page(CourseName(c), ProjectName(p), _, CSID(s), t)
            )
          case Seq(c, p, milestone, s, t, n) =>
            milestone_of(c, p, milestone).flatMap(milestone =>
              n.toIntOption.flatMap(
                run_page(
                  CourseName(c),
                  ProjectName(p),
                  milestone,
                  CSID(s),
                  t,
                  _
                )
              )
            )
          case _ => None
        }
      } catch {
        case NonFatal(e) =>
          Some(
            shell(
              "Status",
              Seq(),
              s"""<p class="bad">This page couldn't be made: ${h(
                  e.toString
                )}</p>"""
            )
          )
      }
    html.map(WebPage("text/html", _))
  }

  // one of the project's milestones, by its slug
  private def milestone_of(c: String, p: String, s: String): Option[Milestone] =
    project_of(CourseName(c), ProjectName(p)).flatMap(
      _.all_milestones.find(_.slug == s)
    )
  private def slug(milestone: Milestone): String = milestone.slug
  private def milestone_name(milestone: Milestone): String = milestone.slug

  private def courses_page(): String = {
    val cs = courses
    val rows = cs.toSeq.map { (name, rc) =>
      val active = rc.projects.count(_._2.active)
      s"""<tr><td><a href="${enc(name.value)}/">${h(name.value)}</a></td>
         |<td><a href="${enc(name.value)}/">by project</a> · <a href="${enc(
          name.value
        )}/by-student/">by student</a></td>
         |<td>${
          if (rc.active) "active" else "<span class=muted>inactive</span>"
        }</td>
         |<td class=num>${rc.projects.size}</td><td class=num>$active</td></tr>""".stripMargin
    }
    shell(
      "Courses",
      Seq(),
      if (cs.isEmpty)
        "<p class=muted>No courses: courses_config is missing or empty.</p>"
      else
        s"""<table class=sortable><thead><tr><th>course</th><th>drill down</th><th>state</th><th class=num>projects</th><th class=num>active</th></tr></thead>
           |<tbody>${rows.mkString}</tbody></table>""".stripMargin
    )
  }

  // the views of one thing, as tabs; `on` is the one shown
  private def tabs(on: String, vs: (String, String)*): String =
    vs.map((name, link) =>
      if (name == on) s"<span class=on>${h(name)}</span>"
      else s"""<a href="$link">${h(name)}</a>"""
    ).mkString("<nav class=views>", "", "</nav>")

  // looks at all of `c`'s projects' results (and aliases) at once
  private def look_course(c: CourseName, rc: RawCourse): Unit = {
    val ps = rc.projects.keys.toSeq.map(ProjectName(_))
    val _ = Try(look_all(for {
      p <- ps
      rp <- rc.projects.get(p.value).toSeq
      milestone <- rp.all_milestones
    } yield data.milestone_results(c, p, milestone)))
    val _ = Try(look_all(ps.map(data.aliases(c, _))))
  }

  private def project_line(rp: RawProject): String = {
    val st = project_state(rp)
    val due = rp.milestones.map(m => s"${h(m.name)} due ${h(fmt(m.deadline))}")
    s"""<p class=muted>${h(st.description)} · ${due.mkString(
        " · "
      )} · ${rp.iterations.min}–${rp.iterations.max} runs per test</p>"""
  }

  // a project's milestones and their progress; links are relative to `base`,
  // the project's page
  private def milestones_table(
      c: CourseName,
      p: ProjectName,
      rp: RawProject,
      base: String
  ): String = {
    val st = project_state(rp)
    val rows = rp.all_milestones.map { milestone =>
      val link = s"$base${slug(milestone)}/"
      results(c, p, milestone) match {
        case Left(why) =>
          s"""<tr><td><a href="$link">${milestone_name(
              milestone
            )}</a></td><td colspan=8 class=bad>${h(why)}</td></tr>"""
        case Right(None) =>
          val why =
            if (!st.publishes(milestone))
              s"not yet: published after ${h(fmt(rp.deadline(milestone)))}"
            else "nothing published"
          s"""<tr><td><a href="$link">${milestone_name(
              milestone
            )}</a></td><td colspan=8 class=muted>$why</td></tr>"""
        case Right(Some((rs, when))) =>
          val pr = progress(
            rs.values.map(_.outcomes.values),
            st.keeps_going(milestone),
            rp.iterations.max
          )
          s"""<tr><td><a href="$link">${milestone_name(
              milestone
            )}</a> <a class=muted href="${link}by-test/">by test</a></td>
             |<td class=num>${pr.students}</td><td class=num>${pr.tests}</td>
             |<td class=num>${pr.runs}</td><td class=num>${pr.to_go}</td>
             |<td>${bar(pr.runs, pr.runs + pr.to_go)}</td>
             |<td class=num>${pr.rate}</td>
             |<td class=num>${percent(pr.happy, pr.pairs)}</td>
             |<td class=muted>${when
              .map(w => h(fmt_iso(w)))
              .getOrElse("")}</td></tr>""".stripMargin
      }
    }
    s"""<table><thead><tr><th>milestone</th><th class=num>students</th><th class=num>tests</th><th class=num>runs done</th><th class=num>to go</th><th>progress</th><th class=num>runs passed</th><th class=num>tests passing</th><th>committed</th></tr></thead>
       |<tbody>${rows.mkString}</tbody></table>""".stripMargin
  }

  private def course_views(on: String, up: String): String =
    tabs(on, "by project" -> up, "by student" -> s"${up}by-student/")

  private def course_page(c: CourseName): Option[String] =
    courses.get(c).map { rc =>
      look_course(c, rc)
      val blocks = projects_of(rc).sortBy(!_._2.active).map { (p, rp) =>
        s"""<section class=project><h2><a href="${enc(p.value)}/">${h(
            p.value
          )}</a> ${
            if (rp.active) "" else "<span class=muted>(inactive)</span>"
          }</h2>
           |${project_line(rp)}
           |${milestones_table(
            c,
            p,
            rp,
            s"${enc(p.value)}/"
          )}</section>""".stripMargin
      }
      shell(
        c.value,
        Seq("Courses" -> "../"),
        course_views("by project", "./") + (if (blocks.isEmpty)
                                              "<p class=muted>No projects.</p>"
                                            else blocks.mkString)
      )
    }

  // a student's results in a milestone, if any: their alias's
  private def results_of(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone,
      csid: CSID
  ): Option[RedactedStudentResults] =
    for {
      alias <- aliases(c, p).get(csid)
      (rs, _) <- results(c, p, milestone).toOption.flatten
      r <- rs.get(alias)
    } yield r

  // a student's standing in a milestone, in one cell: tests passing all runs,
  // and the share of runs passed; links to their page for it
  private def standing(
      c: CourseName,
      p: ProjectName,
      rp: RawProject,
      milestone: Milestone,
      csid: CSID,
      link: String
  ): String =
    results_of(c, p, milestone, csid) match {
      case None    => "<td class=muted></td>"
      case Some(r) =>
        val pr = progress(
          Seq(r.outcomes.values),
          project_state(rp).keeps_going(milestone),
          rp.iterations.max
        )
        val late = Option.when(r.prepare_info.days_late > 0)(
          f""" <span class="tag warn">${r.prepare_info.days_late}%.1fd late</span>"""
        )
        s"""<td class=num><a href="$link">${pr.happy}/${pr.pairs}</a> <span class=muted>${pr.rate}</span>${late
            .getOrElse("")}${r.prepare_info.gap
            .map(g => " " + gap_tag(g))
            .getOrElse("")}</td>"""
    }

  // the course's students: everyone with an alias in one of its projects
  private def course_students_page(c: CourseName): Option[String] =
    courses.get(c).map { rc =>
      look_course(c, rc)
      val ps = projects_of(rc).sortBy((p, rp) => (!rp.active, p))
      val csids = ps.flatMap((p, _) => aliases(c, p).keys).distinct.sorted
      val rows = csids.map { csid =>
        val cells = ps.map((p, rp) =>
          standing(
            c,
            p,
            rp,
            project_state(rp).current,
            csid,
            s"../${enc(p.value)}/${slug(project_state(rp).current)}/${enc(csid.value)}/"
          )
        )
        s"""<tr><td><a href="${enc(csid.value)}/">${h(
            csid.value
          )}</a></td>${cells.mkString}</tr>"""
      }
      shell(
        s"$c / students",
        Seq("Courses" -> "../../", c.value -> "../"),
        s"""${course_views("by student", "../")}
           |<p>${csids.size} students · each project's current results: tests passing every run, and the share of runs passed</p>
           |<table class=sortable><thead><tr><th>student</th>${ps
            .map((p, _) => s"<th class=num>${h(p.value)}</th>")
            .mkString}</tr></thead>
           |<tbody>${rows.mkString}</tbody></table>""".stripMargin
      )
    }

  // one student, across the course's projects and their milestones
  private def course_student_page(c: CourseName, csid: CSID): Option[String] =
    courses.get(c).map { rc =>
      look_course(c, rc)
      val blocks =
        projects_of(rc).sortBy((p, rp) => (!rp.active, p)).map { (p, rp) =>
          val alias = aliases(c, p).get(csid)
          val rows = rp.all_milestones.map { milestone =>
            val link =
              s"../../${enc(p.value)}/${slug(milestone)}/${enc(csid.value)}/"
            results_of(c, p, milestone, csid) match {
              case None =>
                s"""<tr><td><a href="$link">${milestone_name(
                    milestone
                  )}</a></td><td colspan=9 class=muted>not in the published results</td></tr>"""
              case Some(r) =>
                s"""<tr><td><a href="$link">${milestone_name(
                    milestone
                  )}</a></td>${student_cells(
                    r,
                    project_state(rp).keeps_going(milestone),
                    rp.iterations.max
                  )}</tr>"""
            }
          }
          s"""<section class=project><h2><a href="../../${enc(p.value)}/">${h(
              p.value
            )}</a> <span class=muted>${alias
              .map(a => s"alias ${h(a.toString)}")
              .getOrElse("no alias")}</span></h2>
           |${project_line(rp)}
           |<table><thead><tr><th>milestone</th>$student_heads</tr></thead><tbody>${rows.mkString}</tbody></table></section>""".stripMargin
        }
      shell(
        s"$c / ${csid.value}",
        Seq("Courses" -> "../../../", c.value -> "../../", "students" -> "../"),
        blocks.mkString
      )
    }

  // a student's results in a milestone, as cells after their name and alias
  private def student_cells(
      r: RedactedStudentResults,
      keep_going: Boolean,
      max: Int
  ): String = {
    val pr = progress(Seq(r.outcomes.values), keep_going, max)
    val pi = r.prepare_info
    s"""<td class=mono>${h(pi.sha.take(10))}</td>
       |<td>${h(fmt_z(pi.commit_time))}</td>
       |<td class=num>${
        if (pi.days_late > 0) f"${pi.days_late}%.2f" else ""
      }</td>
       |<td>${flags(pi)}</td>
       |<td class=num>${pr.happy}/${pr.pairs}</td>
       |<td class=num>${pr.runs}</td><td class=num>${pr.to_go}</td>
       |<td>${bar(pr.runs, pr.runs + pr.to_go)}</td>
       |<td class=num>${pr.rate}</td>""".stripMargin
  }

  private val student_heads =
    """<th>commit</th><th>committed</th><th class=num>days late</th><th>notes</th><th class=num>tests passing</th><th class=num>runs</th><th class=num>to go</th><th>progress</th><th class=num>runs passed</th>"""

  private def project_views(on: String, up: String): String =
    tabs(
      on,
      "milestones" -> up,
      "by student" -> s"${up}by-student/",
      "by test" -> s"${up}by-test/"
    )

  private def project_page(c: CourseName, p: ProjectName): Option[String] =
    project_of(c, p).map { rp =>
      val _ =
        Try(look_all(rp.all_milestones.map(data.milestone_results(c, p, _))))
      shell(
        s"$c / $p",
        Seq("Courses" -> "../../", c.value -> "../"),
        s"""${project_views("milestones", "./")}
           |${project_line(rp)}
           |${milestones_table(c, p, rp, "")}""".stripMargin
      )
    }

  // a project's students, with their standing in each milestone
  private def project_students_page(
      c: CourseName,
      p: ProjectName
  ): Option[String] =
    project_of(c, p).map { rp =>
      val _ =
        Try(look_all(rp.all_milestones.map(data.milestone_results(c, p, _))))
      val as = aliases(c, p)
      val rows = as.toSeq.map { (csid, alias) =>
        val cells = rp.all_milestones.map(milestone =>
          standing(
            c,
            p,
            rp,
            milestone,
            csid,
            s"../${slug(milestone)}/${enc(csid.value)}/"
          )
        )
        s"""<tr><td><a href="../../by-student/${enc(csid.value)}/">${h(
            csid.value
          )}</a></td><td class=num>${h(
            alias.toString
          )}</td>${cells.mkString}</tr>"""
      }
      shell(
        s"$c / $p / students",
        Seq("Courses" -> "../../../", c.value -> "../../", p.value -> "../"),
        s"""${project_views("by student", "../")}
           |<p>${as.size} students · in each milestone: tests passing every run, and the share of runs passed</p>
           |<table class=sortable><thead><tr><th>student</th><th class=num>alias</th>${rp.all_milestones
            .map(milestone =>
              s"<th class=num>${milestone_name(milestone)}</th>"
            )
            .mkString}</tr></thead>
           |<tbody>${rows.mkString}</tbody></table>""".stripMargin
      )
    }

  // a project's tests, with how each milestone's students do on them
  private def project_tests_page(
      c: CourseName,
      p: ProjectName
  ): Option[String] =
    project_of(c, p).map { rp =>
      val _ =
        Try(look_all(rp.all_milestones.map(data.milestone_results(c, p, _))))
      val per_milestone = rp.all_milestones.map { milestone =>
        milestone -> results(c, p, milestone).toOption.flatten
          .map(_._1)
          .map(rs =>
            test_views(c, p, rp, milestone, rs).map(t => t.name -> t).toMap
          )
      }
      val names = (look(data.published_tests(c, p)) ++ per_milestone.flatMap(
        _._2.toSeq.flatMap(_.keys)
      )).distinct.sorted
      val first = per_milestone
        .flatMap(_._2)
        .flatMap(_.values)
        .groupBy(_.name)
        .view
        .mapValues(_.head)
        .toMap
      val rows = names.map { name =>
        val t = first.get(name)
        val author = t.flatMap(_.author) match {
          case Some(a) =>
            s"""<a href="../../by-student/${enc(a.value)}/">${h(a.value)}</a>"""
          case None => if (t.isEmpty) "" else "<span class=muted>course</span>"
        }
        val tags = t.toSeq
          .flatMap(t =>
            Seq(
              Option.when(t.milestones.nonEmpty)(
                t.milestones
                  .map(m => s"<span class=tag>${h(m)}</span>")
                  .mkString
              ),
              Option.when(t.bad)(
                """<span class="tag warn" title="in bad_tests: ignored">bad</span>"""
              )
            ).flatten
          )
          .mkString(" ")
        val cells = per_milestone.map { (milestone, views) =>
          views.flatMap(_.get(name)).filter(_.ran > 0) match {
            case None    => "<td class=muted></td>"
            case Some(v) =>
              s"""<td class=num><a href="../${slug(milestone)}/by-test/${enc(
                  name
                )}/">${v.passing}/${v.ran}</a> <span class=muted>${percent(
                  v.runs_passed,
                  v.runs
                )}</span>${
                  if (v.flaky > 0) """ <span class="tag warn">flaky</span>"""
                  else ""
                }</td>"""
          }
        }
        s"""<tr><td>${h(
            name
          )}</td><td>$author</td><td>$tags</td>${cells.mkString}</tr>"""
      }
      shell(
        s"$c / $p / tests",
        Seq("Courses" -> "../../../", c.value -> "../../", p.value -> "../"),
        s"""${project_views("by test", "../")}
           |<p>${names.size} tests · in each milestone: students passing every run, of those it ran on, and the share of runs passed</p>
           |<table class=sortable><thead><tr><th>test</th><th>author</th><th>chosen</th>${rp.all_milestones
            .map(milestone =>
              s"<th class=num>${milestone_name(milestone)}</th>"
            )
            .mkString}</tr></thead>
           |<tbody>${rows.mkString}</tbody></table>""".stripMargin
      )
    }

  // Reads what the course and milestone pages show (the courses, and each
  // project's results and aliases) from the local mirror, all at once,
  // telling `log` how it goes. It doesn't update the mirror: `grader mirror`
  // does.
  def warm(log: String => Unit): Unit = {
    def secs(since: Long): String = f"${(System.nanoTime() - since) / 1e9}%.1fs"
    val started = System.nanoTime()
    log("reading courses_config")
    val cs = courses
    val names = for {
      (c, rc) <- cs.toSeq
      p <- rc.projects.keys.toSeq.map(ProjectName(_))
      suffix <- Seq("__results", "__aliases")
    } yield s"${project_repo(c, p)}$suffix"
    log(s"reading ${names.size} repos for ${cs.size} courses")
    val done = java.util.concurrent.atomic.AtomicInteger()
    names
      .map { n =>
        Thread
          .ofVirtual()
          .nn
          .start { () =>
            val at = System.nanoTime()
            val how = Try(look(data.source.master(n))) match {
              case Success(Some(_)) => "ok"
              case Success(None)    => "no such repo"
              case Failure(e)       => s"failed: ${e.toString}"
            }
            log(
              s"  [${done.incrementAndGet()}/${names.size}] $n: $how (${secs(at)})"
            )
          }
          .nn
      }
      .foreach(_.join())
    val _ = Try(cs.toSeq.foreach((c, rc) => look_course(c, rc)))
    log(s"read in ${secs(started)}")
  }

  private def project_of(c: CourseName, p: ProjectName): Option[RawProject] =
    look(data.project(c, p))

  private def milestone_page(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone
  ): Option[String] =
    project_of(c, p).map { rp =>
      val st = project_state(rp)
      val by_alias = aliases(c, p).map(_.swap)
      val crumbs =
        Seq("Courses" -> "../../../", c.value -> "../../", p.value -> "../")
      val title = s"$c / $p / ${milestone_name(milestone)}"
      results(c, p, milestone) match {
        case Left(why)   => shell(title, crumbs, s"<p class=bad>${h(why)}</p>")
        case Right(None) =>
          shell(
            title,
            crumbs,
            s"<p class=muted>Nothing published for ${milestone_name(milestone)} yet.</p>"
          )
        case Right(Some((rs, when))) =>
          val keep_going = st.keeps_going(milestone)
          val max = rp.iterations.max
          val graded = rs.toSeq.map { (alias, r) =>
            val csid = by_alias.get(alias)
            val name = csid
              .map(c => s"""<a href="${enc(c.value)}/">${h(c.value)}</a>""")
              .getOrElse("<span class=muted>?</span>")
            s"""<tr><td>$name</td><td class=num>${h(
                alias.toString
              )}</td>${student_cells(r, keep_going, max)}</tr>"""
          }
          val missing = by_alias.toSeq.filterNot((a, _) => rs.contains(a)).map {
            (alias, csid) =>
              s"""<tr><td><a href="${enc(csid.value)}/">${h(
                  csid.value
                )}</a></td><td class=num>${h(
                  alias.toString
                )}</td><td colspan=9 class=muted>not in the published results</td></tr>"""
          }
          val pr = progress(rs.values.map(_.outcomes.values), keep_going, max)
          shell(
            title,
            crumbs,
            s"""${tabs(
                "by student",
                "by student" -> "./",
                "by test" -> "by-test/"
              )}
               |<p>${pr.students} students · ${pr.tests} tests · ${pr.runs} runs done, ${pr.to_go} to go ${bar(
                pr.runs,
                pr.runs + pr.to_go
              )} · ${pr.rate} of runs passed · ${percent(
                pr.happy,
                pr.pairs
              )} of tests passing</p>
               |<p class=muted>${h(st.description)}: ${
                if (keep_going) "every test gets all"
                else "a test stops at its first failure, else gets all"
              } $max runs · committed ${when
                .map(w => h(fmt_iso(w)))
                .getOrElse("?")}</p>
               |<table class=sortable><thead><tr><th>student</th><th class=num>alias</th>$student_heads</tr></thead>
               |<tbody>${(graded ++ missing).mkString}</tbody></table>""".stripMargin
          )
      }
    }

  private def flags(pi: RedactedPrepareInfo): String =
    Seq(
      Option.when(pi.from_commit_file)(
        """<span class=tag title="graded the commit its commit id file names">commit id file</span>"""
      ),
      pi.gap.map(gap_tag),
      Option.when(pi.has_report)("""<span class=tag>report</span>""")
    ).flatten.mkString(" ")

  // the student's published results for the milestone, and what goes with them
  private case class StudentView(
      rp: RawProject,
      alias: Option[Alias],
      results: Option[RedactedStudentResults],
      own: Option[(StudentResults, Tree)]
  )

  private def student_view(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone,
      csid: CSID
  ): Option[StudentView] =
    project_of(c, p).map { rp =>
      val alias = aliases(c, p).get(csid)
      val rs = results(c, p, milestone).toOption.flatten.map(_._1)
      val mine = alias.flatMap(a => rs.flatMap(_.get(a)))
      // the student's own repo has the current milestone's results only
      // (a student's own results repo has the current milestone's)
      val own =
        if (milestone == project_state(rp).current) own_results(c, p, csid)
        else None
      StudentView(rp, alias, mine, own)
    }

  private def student_page(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone,
      csid: CSID
  ): Option[String] =
    student_view(c, p, milestone, csid).map { v =>
      val crumbs = Seq(
        "Courses" -> "../../../../",
        c.value -> "../../../",
        p.value -> "../../",
        milestone_name(milestone) -> "../"
      )
      val title = s"$c / $p / ${milestone_name(milestone)} / ${csid.value}"
      val st = project_state(v.rp)
      val keep_going = st.keeps_going(milestone)
      val due = v.rp.deadline(csid, milestone)
      val ext = v.rp.extension_days(csid, milestone)
      val commit = v.results match {
        case None =>
          "<p class=muted>Not in this milestone's published results.</p>"
        case Some(r) =>
          val pi = r.prepare_info
          val source = v.own
            .map(_._1.prepare_info.commit_source)
            .filter(_ => v.own.exists(_._1.prepare_info.sha == pi.sha)) match {
            case Some(CommitSource.FromFile(file, named)) =>
              s"named in <code>${h(file)}</code> (${h(named)})"
            case Some(CommitSource.Latest) => latest_source(milestone)
            case None                      =>
              if (pi.from_commit_file)
                s"named in <code>${h(milestone.commit_id_file.getOrElse("its commit id file"))}</code>"
              else latest_source(milestone)
          }
          s"""<dl>
             |<dt>graded commit</dt><dd class=mono>${h(pi.sha)}</dd>
             |<dt>chosen</dt><dd>$source</dd>
             |<dt>committed</dt><dd>${h(fmt_z(pi.commit_time))}</dd>
             |<dt>pushed</dt><dd>${pi.push_time
              .map(t => h(fmt_i(t)))
              .getOrElse("<span class=muted>unknown</span>")}${pi.gap
              .map(g => " " + gap_note(g))
              .getOrElse("")}</dd>
             |<dt>deadline</dt><dd>${h(fmt(due))}${
              if (ext > 0) s" <span class=muted>(with $ext extension day${
                  if (ext == 1) "" else "s"
                })</span>"
              else ""
            }</dd>
             |<dt>days late</dt><dd>${
              if (pi.days_late > 0) f"${pi.days_late}%.2f" else "on time"
            }</dd>
             |</dl>""".stripMargin
      }
      val history =
        commit_history(c, p, csid, milestone, v.results.map(_.prepare_info.sha))
      val tests = v.results.toSeq.flatMap(_.outcomes.toSeq).map { (t, o) =>
        val counts =
          OutcomeStatus.values.toSeq.map(s => o.outcomes.count(_._1 == s))
        val times = o.min_max.map((a, b) => f"$a%.2f – $b%.2f s").getOrElse("")
        s"""<tr><td><a href="${enc(t.external_name)}/">${h(
            t.external_name
          )}</a></td>
           |<td>${o.worst.map(status_badge).getOrElse("")}</td>
           |<td class=num>${o.outcomes.size}</td>${counts
            .map(n => s"<td class=num>${if (n == 0) "" else n}</td>")
            .mkString}
           |<td class=num>${remaining(o, keep_going, v.rp.iterations.max)}</td>
           |<td>${times}</td><td>${o.last
            .map(status_badge)
            .getOrElse("")}</td></tr>""".stripMargin
      }
      shell(
        title,
        crumbs,
        s"""<p>${v.alias
            .map(a => s"alias ${h(a.toString)}")
            .getOrElse("<span class=muted>no alias</span>")}</p>
           |<h2>Commit</h2>$commit
           |<h2>${h(
            milestone.commit_id_file
              .map(f => s"$f history")
              .getOrElse("Recent commits on master")
          )}</h2>$history
           |<h2>Tests</h2>
           |${
            if (tests.isEmpty) "<p class=muted>none</p>"
            else
              s"""<table class=sortable><thead><tr><th>test</th><th>overall</th><th class=num>runs</th>${OutcomeStatus.values.toSeq
                  .map(s => s"<th class=num>${s.toString}</th>")
                  .mkString}<th class=num>to go</th><th>times</th><th>last</th></tr></thead>
                |<tbody>${tests.mkString}</tbody></table>""".stripMargin
          }""".stripMargin
      )
    }

  ///////////// tests /////////////

  // What there is to know about a test in a milestone
  private case class TestView(
      name: String,
      // None: one of the course's own (an override)
      author: Option[CSID],
      // the milestones it's one of the tests of
      milestones: Seq[String],
      bad: Boolean,
      // whether grading the milestone runs it
      runs_here: Boolean,
      // each student's outcome for it, by alias
      outcomes: SortedMap[Alias, RedactedOutcome]
  ) {
    def ran: Int = outcomes.size
    def passing: Int = outcomes.values.count(_.is_happy)
    def failing: Int = outcomes.values.count(o =>
      o.outcomes.nonEmpty && !o.outcomes.exists(_._1.isHappy)
    )
    // passed some runs, not others
    def flaky: Int =
      outcomes.values.count(o => o.outcomes.exists(_._1.isHappy) && !o.is_happy)
    def runs: Int = outcomes.values.map(_.outcomes.size).sum
    def runs_passed: Int =
      outcomes.values.map(_.outcomes.count(_._1.isHappy)).sum
  }

  // the milestone's tests: those published in the tests repo, and any its
  // results ran
  private def test_views(
      c: CourseName,
      p: ProjectName,
      rp: RawProject,
      milestone: Milestone,
      rs: SortedMap[Alias, RedactedStudentResults]
  ): Seq[TestView] = {
    val by_alias = aliases(c, p).map((csid, a) => a.toString -> csid)
    val published = look(data.published_tests(c, p))
    val ran = rs.values.flatMap(_.outcomes.keys.map(_.external_name))
    val set = project_state(rp).tests(milestone)
    (published ++ ran).distinct.sorted.map { name =>
      val author = by_alias.get(name)
      def in(names: Seq[String]) =
        names.contains(name) || author.exists(a => names.contains(a.value))
      val milestones = rp.milestones.filter(m => in(m.tests)).map(_.name)
      val bad = in(rp.bad_tests)
      TestView(
        name = name,
        author = author,
        milestones = milestones,
        bad = bad,
        runs_here = set match {
          case TestSet.All      => true
          case TestSet.Good     => !bad
          case TestSet.Chosen   => milestones.nonEmpty && !bad
          case TestSet.Of(name) => milestones.contains(name) && !bad
        },
        outcomes =
          rs.flatMap((a, r) => r.outcomes.get(RedactedTestId(name)).map(a -> _))
      )
    }
  }

  private def test_tags(t: TestView, own: Option[RedactedOutcome]): String =
    Seq(
      Option.when(t.milestones.nonEmpty)(
        t.milestones.map(m => s"<span class=tag>${h(m)}</span>").mkString
      ),
      Option.when(t.bad)(
        """<span class="tag warn" title="in bad_tests: ignored">bad</span>"""
      ),
      Option.when(!t.runs_here)(
        """<span class="tag" title="grading this milestone doesn't run it">not run here</span>"""
      ),
      Option.when(t.flaky > 0)(
        s"""<span class="tag warn" title="passed some runs, not others, for ${t.flaky} students">flaky</span>"""
      ),
      Option.when(t.ran > 0 && t.passing == 0)(
        """<span class="tag warn">no one passes</span>"""
      ),
      Option.when(own.exists(!_.is_happy))(
        """<span class="tag warn">its author's code fails it</span>"""
      )
    ).flatten.mkString(" ")

  // when the test was pushed, marked if after `milestone`'s deadline
  private def pushed(
      tc: TestCommit,
      rp: RawProject,
      milestone: Milestone
  ): String = {
    val cutoff =
      ZonedDateTime
        .of(rp.deadline(milestone), ZoneId.systemDefault)
        .nn
        .toInstant
        .nn
    tc.push_time match {
      case None    => "<span class=muted>unknown</span>"
      case Some(t) =>
        val late = Option.when(t.isAfter(cutoff))(
          """ <span class="tag warn">after the deadline</span>"""
        )
        val gap =
          Try(ZonedDateTime.parse(tc.commit_time).nn.toInstant.nn).toOption
            .flatMap(ct => PushGap.of(ct, Some(t)))
            .map(g => " " + gap_note(g))
        h(fmt_i(t)) + late.getOrElse("") + gap.getOrElse("")
    }
  }

  private def tests_page(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone
  ): Option[String] =
    project_of(c, p).map { rp =>
      val crumbs = Seq(
        "Courses" -> "../../../../",
        c.value -> "../../../",
        p.value -> "../../",
        milestone_name(milestone) -> "../"
      )
      val title = s"$c / $p / ${milestone_name(milestone)} / tests"
      results(c, p, milestone) match {
        case Left(why)   => shell(title, crumbs, s"<p class=bad>${h(why)}</p>")
        case Right(None) =>
          shell(
            title,
            crumbs,
            s"<p class=muted>Nothing published for ${milestone_name(milestone)} yet.</p>"
          )
        case Right(Some((rs, _))) =>
          val views = test_views(c, p, rp, milestone, rs)
          val _ = Try(
            look_all(
              views.flatMap(_.author).map(data.test_commit(c, p, _, milestone))
            )
          )
          val rows = views.map { t =>
            val tc = t.author.flatMap(test_commit(c, p, _, milestone))
            val own = t.author
              .flatMap(a => aliases(c, p).get(a))
              .flatMap(a => t.outcomes.get(a))
            val author = t.author match {
              case Some(a) =>
                s"""<a href="../${enc(a.value)}/">${h(a.value)}</a>"""
              case None => "<span class=muted>course</span>"
            }
            s"""<tr><td><a href="${enc(t.name)}/">${h(
                t.name
              )}</a></td><td>$author</td>
               |<td class=mono>${tc
                .map(x => h(x.sha.take(10)))
                .getOrElse("")}</td>
               |<td>${tc.map(x => h(fmt_iso(x.commit_time))).getOrElse("")}</td>
               |<td>${tc.map(pushed(_, rp, milestone)).getOrElse("")}</td>
               |<td class=num>${t.passing}/${t.ran}</td><td class=num>${t.failing}</td><td class=num>${t.flaky}</td>
               |<td class=num>${percent(t.runs_passed, t.runs)}</td>
               |<td>${test_tags(t, own)}${tc
                .filter(_.later_changes > 0)
                .map(x =>
                  s""" <span class=tag title="changes after the deadline are ignored">changed ${x.later_changes}× later</span>"""
                )
                .getOrElse("")}</td></tr>""".stripMargin
          }
          val chosen = views.count(t => t.milestones.nonEmpty && !t.bad)
          shell(
            title,
            crumbs,
            s"""${tabs("by test", "by student" -> "../", "by test" -> "./")}
               |<p>${views.size} tests · $chosen chosen · ${views.count(
                _.bad
              )} bad · ${views.count(_.runs_here)} run in ${milestone_name(
                milestone
              )} · tests as of ${h(fmt(rp.deadline(milestone)))}</p>
               |<table class=sortable><thead><tr><th>test</th><th>author</th><th>commit</th><th>committed</th><th>pushed</th>
               |<th class=num title="students who passed all its runs, of those it ran on">passing</th><th class=num title="students who failed all its runs">failing</th><th class=num title="students who passed some runs, not others">flaky</th>
               |<th class=num>runs passed</th><th>notes</th></tr></thead>
               |<tbody>${rows.mkString}</tbody></table>""".stripMargin
          )
      }
    }

  private def one_test_page(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone,
      name: String
  ): Option[String] =
    for {
      rp <- project_of(c, p)
      (rs, _) <- results(c, p, milestone).toOption.flatten
      t <- test_views(c, p, rp, milestone, rs).find(_.name == name)
    } yield {
      val crumbs = Seq(
        "Courses" -> "../../../../../",
        c.value -> "../../../../",
        p.value -> "../../../",
        milestone_name(milestone) -> "../../",
        "tests" -> "../"
      )
      val by_alias = aliases(c, p).map(_.swap)
      val tc = t.author.flatMap(test_commit(c, p, _, milestone))
      val own = t.author
        .flatMap(a => aliases(c, p).get(a))
        .flatMap(a => t.outcomes.get(a))
      val st = project_state(rp)
      val commit = (t.author, tc) match {
        case (None, _) => "<dt>author</dt><dd>the course (an override)</dd>"
        case (Some(a), None) =>
          s"""<dt>author</dt><dd><a href="../../${enc(a.value)}/">${h(
              a.value
            )}</a></dd><dd class=muted>its commit can't be found in the submission repo</dd>"""
        case (Some(a), Some(x)) =>
          s"""<dt>author</dt><dd><a href="../../${enc(a.value)}/">${h(
              a.value
            )}</a></dd>
             |<dt>test last changed in</dt><dd class=mono>${h(x.sha)}</dd>
             |<dt>committed</dt><dd>${h(fmt_iso(x.commit_time))}</dd>
             |<dt>pushed</dt><dd>${pushed(x, rp, milestone)}</dd>
             |<dt>taken from</dt><dd><span class=mono>${h(
              x.rev
            )}</span> <span class=muted>(the last commit on master before ${h(
              fmt(rp.deadline(milestone))
            )})</span></dd>
             |<dt>changed later</dt><dd>${
              if (x.later_changes == 0) "no"
              else
                s"${x.later_changes} commits on master change it after that: ignored"
            }</dd>""".stripMargin
      }
      val chosen =
        if (t.bad) "no: it's in bad_tests"
        else
          t.milestones match {
            case Seq() => "no"
            case ps    => s"for ${ps.mkString(" and ")}"
          }
      val keep_going = st.keeps_going(milestone)
      val rows = t.outcomes.toSeq.map { (alias, o) =>
        val who = by_alias.get(alias) match {
          case Some(s) =>
            s"""<a href="../../${enc(s.value)}/${enc(name)}/">${h(
                s.value
              )}</a>"""
          case None => "<span class=muted>?</span>"
        }
        val counts =
          OutcomeStatus.values.toSeq.map(s => o.outcomes.count(_._1 == s))
        s"""<tr><td>$who${
            if (t.author.exists(a => by_alias.get(alias).contains(a)))
              " <span class=tag>author</span>"
            else ""
          }</td>
           |<td class=num>${h(alias.toString)}</td><td>${o.worst
            .map(status_badge)
            .getOrElse("")}</td>
           |<td class=num>${o.outcomes.size}</td>${counts
            .map(n => s"<td class=num>${if (n == 0) "" else n}</td>")
            .mkString}
           |<td class=num>${remaining(o, keep_going, rp.iterations.max)}</td>
           |<td>${o.min_max
            .map((a, b) => f"$a%.2f – $b%.2f s")
            .getOrElse("")}</td><td>${o.last
            .map(status_badge)
            .getOrElse("")}</td></tr>""".stripMargin
      }
      shell(
        s"$c / $p / ${milestone_name(milestone)} / tests / $name",
        crumbs,
        s"""<dl>$commit
           |<dt>chosen</dt><dd>$chosen</dd>
           |<dt>run in ${milestone_name(milestone)}</dt><dd>${
            if (t.runs_here) "yes" else "no"
          }</dd>
           |<dt>passing</dt><dd>${t.passing} of ${t.ran} students · ${t.failing} fail every run · ${t.flaky} flaky · ${percent(
            t.runs_passed,
            t.runs
          )} of runs passed</dd>
           |<dt>notes</dt><dd>${test_tags(t, own)}</dd>
           |</dl>
           |<table class=sortable><thead><tr><th>student</th><th class=num>alias</th><th>overall</th><th class=num>runs</th>${OutcomeStatus.values.toSeq
            .map(s => s"<th class=num>${s.toString}</th>")
            .mkString}<th class=num>to go</th><th>times</th><th>last</th></tr></thead>
           |<tbody>${rows.mkString}</tbody></table>""".stripMargin
      )
    }

  private def latest_source(milestone: Milestone): String = milestone match {
    case Milestone.Live => "the head of master"
    case _              =>
      s"the last commit on master before the deadline${milestone.commit_id_file
          .fold("")(f => s" (<code>${h(f)}</code> names none)")}"
  }

  // When the commit id file changed, and what it named (or, for Live
  // results, master's last commits), from the submission repo
  private def commit_history(
      c: CourseName,
      p: ProjectName,
      csid: CSID,
      milestone: Milestone,
      graded: Option[String]
  ): String =
    look(data.commit_history(c, p, csid, milestone)) match {
      case None       => "<p class=muted>The submission repo can't be read.</p>"
      case Some(rows) =>
        def mark(sha: String) =
          if (graded.contains(sha)) " <span class=tag>graded</span>" else ""
        milestone.commit_id_file match {
          case Some(file) =>
            if (rows.isEmpty)
              s"<p class=muted><code>${h(file)}</code> was never committed: the last commit before the deadline is graded.</p>"
            else {
              val trs = rows.map { (r: CommitRow) =>
                val what = r.content match {
                  case None     => "<span class=muted>deleted</span>"
                  case Some("") => "<span class=muted>empty</span>"
                  case Some(id) =>
                    s"<span class=mono>${h(id)}</span>${r.named.map(mark).getOrElse(" <span class=\"tag warn\">names no commit</span>")}"
                }
                s"""<tr><td>${h(fmt_iso(r.when))}</td><td class=mono>${h(
                    r.sha.take(10)
                  )}</td><td>${h(r.subject)}</td><td>$what</td></tr>"""
              }
              s"""<table><thead><tr><th>changed</th><th>in commit</th><th>message</th><th>names</th></tr></thead><tbody>${trs.mkString}</tbody></table>"""
            }
          case None =>
            val trs = rows.map { r =>
              s"""<tr><td>${h(fmt_iso(r.when))}</td><td class=mono>${h(
                  r.sha.take(10)
                )}${mark(r.sha)}</td><td>${h(r.subject)}</td></tr>"""
            }
            s"""<table><thead><tr><th>committed</th><th>commit</th><th>message</th></tr></thead><tbody>${trs.mkString}</tbody></table>"""
        }
    }

  private def outcome_of(
      v: StudentView,
      test: String
  ): Option[RedactedOutcome] =
    v.results.flatMap(_.outcomes.get(RedactedTestId(test)))

  private def test_page(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone,
      csid: CSID,
      test: String
  ): Option[String] =
    student_view(c, p, milestone, csid).flatMap { v =>
      outcome_of(v, test).map { o =>
        val crumbs = Seq(
          "Courses" -> "../../../../../",
          c.value -> "../../../../",
          p.value -> "../../../",
          milestone_name(milestone) -> "../../",
          csid.value -> "../"
        )
        val rows = o.outcomes.zipWithIndex.map { case ((status, time), i) =>
          s"""<tr><td class=num><a href="${i + 1}">${i + 1}</a></td><td>${status_badge(
              status
            )}</td><td class=num>${time
              .map(t => f"$t%.2f s")
              .getOrElse("")}</td></tr>"""
        }
        val to_go = remaining(
          o,
          project_state(v.rp).keeps_going(milestone),
          v.rp.iterations.max
        )
        shell(
          s"$c / $p / ${milestone_name(milestone)} / ${csid.value} / $test",
          crumbs,
          s"""<p>${o.outcomes.size} runs, ${o.outcomes.count(
              _._1 == OutcomeStatus.pass
            )} passed · $to_go to go${o.min_max
              .map((a, b) => f" · $a%.2f – $b%.2f s")
              .getOrElse("")}</p>
             |<table class=sortable><thead><tr><th class=num>run</th><th>outcome</th><th class=num>time</th></tr></thead><tbody>${rows.mkString}</tbody></table>""".stripMargin
        )
      }
    }

  private def run_page(
      c: CourseName,
      p: ProjectName,
      milestone: Milestone,
      csid: CSID,
      test: String,
      n: Int
  ): Option[String] =
    student_view(c, p, milestone, csid).flatMap { v =>
      outcome_of(v, test).filter(o => n >= 1 && n <= o.outcomes.size).map { o =>
        val (status, time) = o.outcomes(n - 1)
        // at .../<test>/<n>: "./" is the test's page
        val crumbs = Seq(
          "Courses" -> "../../../../../",
          c.value -> "../../../../",
          p.value -> "../../../",
          milestone_name(milestone) -> "../../",
          csid.value -> "../",
          test -> "./"
        )
        val sha = v.results.map(_.prepare_info.sha).getOrElse("")
        // the student's own repo keeps the output of each test's last run,
        // if it's the same commit and as many runs
        val output = v.own
          .filter((own, _) =>
            own.prepare_info.sha == sha && own.outcomes
              .get(RedactedTestId(test))
              .exists(_.outcomes.size == o.outcomes.size)
          )
          .filter(_ => n == o.outcomes.size)
          .map { (_, tree) =>
            val s = state()
            val files = for {
              ext <- Seq(
                "result",
                "time",
                "ok",
                "out",
                "err",
                "stdout",
                "stderr"
              )
              name = s"$test.$ext"
              bytes <- s.read(tree, os.RelPath(name))
            } yield (name, bytes)
            if (files.isEmpty)
              "<p class=muted>No output was kept for this run.</p>"
            else
              files.map { (name, bytes) =>
                val size = bytes.length
                val limit = 200_000
                val text = String(bytes.take(limit), UTF_8)
                s"""<h3>${h(name)} <span class=muted>(${size} bytes${
                    if (size > limit) s", first $limit shown" else ""
                  })</span></h3><pre>${h(text)}</pre>"""
              }.mkString
          }
          .getOrElse(
            if (
              milestone == project_state(v.rp).current && n == o.outcomes.size
            )
              "<p class=muted>Its output isn't in the student's results repo (it holds the last published run).</p>"
            else "<p class=muted>Only the last run's output is kept, for the current milestone's results.</p>"
          )
        shell(
          s"$c / $p / ${milestone_name(milestone)} / ${csid.value} / $test / run $n",
          crumbs,
          s"""<dl><dt>outcome</dt><dd>${status_badge(status)}</dd>
             |<dt>time</dt><dd>${time
              .map(t => f"$t%.3f s")
              .getOrElse("<span class=muted>unknown</span>")}</dd>
             |<dt>commit</dt><dd class=mono>${h(sha)}</dd>
             |<dt>run</dt><dd>$n of ${o.outcomes.size}</dd></dl>
             |<p>${
              if (n > 1) s"""<a href="${n - 1}">← run ${n - 1}</a>""" else ""
            } ${
              if (n < o.outcomes.size)
                s"""<a href="${n + 1}">run ${n + 1} →</a>"""
              else ""
            }</p>
             |<h2>Output</h2>$output""".stripMargin
        )
      }
    }

  ///////////// html /////////////

  // A push gap: pushed long after the commit is a red flag (its commit time
  // may have been set back); committed after the push is just informative
  // (the student's clock is ahead).
  private def gap_tag(g: PushGap): String = g match {
    case PushGap.PushedLater(d) =>
      s"""<span class="tag bad" title="pushed ${fmt_gap(
          d
        )} after it was committed: its commit time may have been set back">pushed late</span>"""
    case PushGap.CommittedLater(d) =>
      s"""<span class=tag title="committed ${fmt_gap(
          d
        )} after it was pushed: the student's clock is probably ahead">clock ahead</span>"""
  }

  private def gap_note(g: PushGap): String = g match {
    case PushGap.PushedLater(d) =>
      s"""<span class="tag bad" title="its commit time may have been set back">pushed ${fmt_gap(
          d
        )} after the commit</span>"""
    case PushGap.CommittedLater(d) =>
      s"""<span class=tag title="the student's clock is probably ahead">committed ${fmt_gap(
          d
        )} after the push</span>"""
  }

  private def fmt_gap(d: java.time.Duration): String = {
    val m = d.toMinutes
    if (m >= 24 * 60) s"${m / (24 * 60)}d ${m / 60 % 24}h"
    else if (m >= 60) s"${m / 60}h ${m % 60}m"
    else s"${m}m"
  }

  private def status_badge(s: OutcomeStatus): String =
    s"""<span class="st st-${s.toString}">${s.toString}</span>"""

  private def bar(done: Int, total: Int): String = {
    val pct = if (total == 0) 100 else math.round(done * 100.0 / total).toInt
    s"""<span class=bar title="$done of $total runs"><span style="width:$pct%"></span></span>"""
  }

  private def percent(a: Int, b: Int): String =
    if (b == 0) "" else f"${a * 100.0 / b}%.1f%%"

  private val minutes = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").nn
  private def fmt(t: LocalDateTime): String = t.format(minutes).nn
  private def fmt_z(t: ZonedDateTime): String =
    t.withZoneSameInstant(ZoneId.systemDefault).nn.format(minutes).nn
  private def fmt_i(t: Instant): String = fmt_z(
    t.atZone(ZoneId.systemDefault).nn
  )
  private def fmt_iso(s: String): String =
    Try(fmt_z(ZonedDateTime.parse(s.trim).nn)).getOrElse(s)

  // the course's projects, by name
  private def projects_of(rc: RawCourse): Seq[(ProjectName, RawProject)] =
    rc.projects.toSeq.map((p, rp) => (ProjectName(p), rp))

  private def h(s: String): String =
    s.flatMap {
      case '&'  => "&amp;"
      case '<'  => "&lt;"
      case '>'  => "&gt;"
      case '"'  => "&quot;"
      case '\'' => "&#39;"
      case c    => c.toString
    }

  // a path segment, and back
  private def enc(s: String): String =
    URLEncoder.encode(s, UTF_8).nn.replace("+", "%20")
  private def decode(s: String): String =
    URLDecoder.decode(s.replace("+", "%2B"), UTF_8).nn
  private def shell(
      title: String,
      crumbs: Seq[(String, String)],
      body: String
  ): String =
    s"""<!doctype html><html lang="en"><head><meta charset="utf-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1">
       |<title>${h(
        title
      )} · status</title><style>${Status.css}</style></head><body>
       |<header><nav>${(crumbs.map((t, l) =>
        s"""<a href="$l">${h(t)}</a>"""
      ) :+ s"<span>${h(title.split(" / ").last)}</span>").mkString(" › ")}</nav>
       |<span class=muted>from the committed repos, as of ${h(
        LocalDateTime.now().nn.format(minutes).nn
      )}</span></header>
       |<main><h1>${h(
        title
      )}</h1>$body</main><script>${Status.js}</script></body></html>""".stripMargin
}

object Status {
  // src/main/resources/ag/grader/status.{css,js}, through our own class
  // loader (the server's threads' context loader can't see them under sbt)
  private def resource(name: String): String =
    os.read(
      os.resource(using
        os.ResourceRoot.ClassLoader(classOf[Status].getClassLoader.nn)
      ) / "ag" / "grader" / name
    )
  lazy val css: String = resource("status.css")
  lazy val js: String = resource("status.js")
}
