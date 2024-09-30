package ag.grader

import scala.collection.{mutable, SortedMap, SortedSet}
import upickle.default.{ReadWriter}
import ag.rules.{
  Maker,
  Periodic,
  Rule,
  SignedPath,
  given_ReadWriter_SortedMap,
  run,
  say
}

@upickle.implicits.allowUnknownKeys(false)
case class NotificationConfig(
    send_to_student: Boolean,
    cc: Option[CSID],
    key_update: Boolean,
    result_update: Boolean,
    repo_create: Boolean,
    site_base: Option[String]
) derives ReadWriter {

  private def doit(
      to: CSID,
      cc: Option[CSID],
      subject: => String,
      contents: => String
  ): Unit = {
    println(s"----- sending $send_to_student $to $cc $subject")
    os.proc(
      "mail",
      cc match {
        case Some(cc) => Seq("-c", cc.value)
        case None     => Seq()
      },
      "-s",
      subject,
      to.value
    ).run(check = true, stdin = contents)
  }

  private def send(
      csid: CSID,
      subject: => String,
      contents: => String
  ): Unit = {
    (send_to_student, cc) match {
      case (true, Some(cc)) if csid != cc =>
        doit(csid, Some(cc), subject, contents)
      case (true, _) =>
        doit(csid, None, subject, contents)
      case (false, Some(cc)) =>
        doit(cc, None, subject, contents)
      case (false, None) =>
        say(s"not sending '$subject to $csid'")
    }
  }

  def send_key_update(course: Course, csid: CSID, server: RemoteServer): Unit =
    if (key_update) {
      send(
        csid,
        s"key updated for ${course.course_name}::$csid",
        s"""
      |To check your connection:
      |    ssh ${server.ssh_uri} info
      """.stripMargin
      )
    } else {
      say(
        s"    key update notification disabled for ${course.course_name}::$csid"
      )
    }

  def send_repo_created(
      project: Project,
      repo_info: RepoInfo,
      csid: CSID
  ): Unit = if (repo_create) {
    val msg =
      s"git clone ${repo_info.server.git_uri(project.work_repo_name(csid))}"
    send(
      csid,
      msg,
      s"""
      | to clone:
      |      $msg
      """.stripMargin
    )
  } else {
    say(
      s"    repo creation notification is disabled for ${project.course.course_name}::${project.project_name}::$csid"
    )

  }

  def send_result_update(
      project: Project,
      csid: CSID,
      server: RemoteServer,
      student_results: RedactedStudentResults
  ): Unit = if (result_update) {
    val course_name = project.course.course_name
    val project_name = project.project_name
    val alias = student_results.alias

    val n_tests = student_results.outcomes.size
    val n_pass = student_results.outcomes.values
      .filter(_.outcome == Some(OutcomeStatus.Pass))
      .size

    val short_sha = student_results.prepare_info.sha.substring(6).nn
    val has_report = if (student_results.prepare_info.has_report) "+" else "-"
    val has_test = if (student_results.has_test) "+" else "-"

    val subject =
      s"[$n_pass/$n_tests:${has_test}T:${has_report}R] ${course_name}_${project_name}_${csid} [$short_sha]"

    val not_passing = (for {
      (_, outcome) <- student_results.outcomes
      if outcome.outcome != Some(OutcomeStatus.Pass)
    } yield s"${outcome.test_id.external_name} ... ${outcome.outcome.map(_.label).getOrElse("?")}")
      .mkString("\n")

    val web_page = site_base match {
      case Some(site_base) =>
        s"$site_base/${course_name}_${project_name}.html"
      case None =>
        ""
    }

    val contents = s"""
      |more information at: $web_page
      |your alias: ${alias.getOrElse("?")}
      |your details: git clone ${server.git_uri(
                       project.student_results_repo_name(csid)
                     )}
      |all tests: git clone ${server.git_uri(project.tests_repo_name)}
      |summary for project: git clone ${server.git_uri(
                       project.project_results_repo_name
                     )}
      |
      |$not_passing
      """.stripMargin

    send(
      csid,
      subject,
      contents
    )

  } else {
    say(
      s"    result update notification is disabled for ${project.course.course_name}::${project.project_name}::${csid}"
    )
  }
}

@upickle.implicits.allowUnknownKeys(false)
case class RawCourseNotSorted(
    active: Boolean,
    notifications: NotificationConfig,
    projects: Map[String, RawProject],
    staff: SortedSet[CSID] = SortedSet()
) derives ReadWriter {
  lazy val sorted: RawCourse = RawCourse(
    active = active,
    notifications = notifications,
    projects = projects.to(SortedMap),
    staff = staff
  )
}

@upickle.implicits.allowUnknownKeys(false)
case class RawCourse(
    active: Boolean,
    notifications: NotificationConfig,
    projects: SortedMap[String, RawProject],
    staff: SortedSet[CSID] = SortedSet()
) derives ReadWriter

case class Course(course_name: String) derives ReadWriter {

  private val scope = os.RelPath(course_name)

  lazy val staff_group_name: String = s"@${course_name}_staff"

  lazy val raw: Maker[RawCourse] = Gitolite.raw_course(course_name)

  lazy val notifications: Maker[NotificationConfig] =
    Rule(raw, scope)(_.notifications)

  lazy val projects: Maker[SortedMap[String, Project]] =
    Rule(raw, scope) { rc =>
      val s = for {
        pn <- rc.projects.keySet.toSeq
      } yield (pn, Project(this, pn))
      SortedMap(s*)
    }

  lazy val active: Maker[Boolean] = Rule(raw, scope) { rc =>
    rc.active
  }

  lazy val staff: Maker[SortedSet[CSID]] = Rule(raw, scope) { rc => rc.staff }

  lazy val active_projects: Maker[SortedMap[String, Project]] =
    Rule(Maker.select(projects.map(_.values.toSeq))(_.active), scope) {
      projects =>
        projects.map(p => (p.project_name, p)).to(SortedMap)
    }

  def project(project_name: String): Project =
    Project(this, project_name)

  // Get a sorted list of csids in the dropbox
  lazy val dropbox: Maker[SortedMap[CSID, String]] =
    Rule(Periodic(60000) *: Config.dropbox_path, scope) {
      case (_, Some(dropbox)) =>
        val base = os.home / dropbox / course_name
        val pairs = for {
          f <- os.list(base)
          if os.isFile(f)
          ext = f.ext
          if ext == "pub"
          id = f.baseName
        } yield {
          val owner = os.owner(f).getName
          if (owner != id) {
            throw Exception(s"owner:$owner != csid:$id")
          }
          (CSID(id), os.read(f).trim.nn)
        }
        pairs.to(SortedMap)
      case (_, None) =>
        SortedMap()
    }

  /*
   * Update keys in gitolite-admin
   *
   * Key updates are automatic (when a students drops a new key in the dropbox)
   * Key deletion is manual and need to be done in both places
   */

  lazy val publish_keys: Maker[SignedPath[SortedMap[CSID, String]]] =
    SignedPath.rule(
      Gitolite.repo_info("gitolite-admin") *: dropbox *: notifications.peek,
      SortedSet(".git"),
      scope
    ) { case (dir, (g, dropbox, notifications)) =>
      val added = g.update(
        path = dir,
        fork_from = None,
        msg = "updated keys",
        readers = Seq(),
        writers = Seq()
      ) { _ =>

        val added = mutable.SortedSet[CSID]()

        for {
          // all keys in dropbox
          (csid, new_key) <- dropbox.toSeq
          key_file_name = s"$csid.pub"
          key_file_path = dir / "keydir" / key_file_name
          // if the key doesn't exist or is different
          if !os
            .exists(key_file_path) || os.read(key_file_path).trim.nn != new_key
        } {
          // If we make it here then the key needs to be added or updated
          say(s"adding $csid to gitolite-admin")
          os.write.over(key_file_path, new_key)
          added.add(csid)
        }
        added
      }

      for (csid <- added) {
        notifications.send_key_update(this, csid, g.server)
      }

      dropbox

    }

  lazy val enrollment_repo_name = s"${course_name}__enrollment"
  def enrollment_file(dir: os.Path): os.Path = dir / "enrollment.json"

  // read the enrollment repo

  lazy val enrollment: Maker[SortedMap[CSID, String]] =
    Rule(Gitolite.mirror(enrollment_repo_name), scope) { sp =>
      if (sp.data) {
        val f = enrollment_file(sp.path)
        if (os.isFile(f)) {
          upickle.default.read[SortedMap[CSID, String]](
            os.read(enrollment_file(sp.path))
          )
        } else {
          say(s"enrollment file $f doesn't exist")
          SortedMap()
        }
      } else {
        SortedMap[CSID, String]()
      }
    }

  // create/update the enrollment repo
  lazy val publish_enrollment: Maker[SignedPath[SortedMap[CSID, String]]] =
    SignedPath.rule(
      Gitolite.repo_info(enrollment_repo_name) *: publish_keys,
      SortedSet(".git"),
      scope
    ) { case (dir, (g, keys)) =>
      g.update(
        path = dir,
        fork_from = Some("empty"),
        msg = "enrolling students",
        readers = Seq(staff_group_name),
        writers = Seq()
      ) { _ =>
        os.write.over(
          enrollment_file(dir),
          upickle.default.write(keys.data, indent = 2)
        )
        keys.data
      }
    }

}

object Course {
  given Ordering[Course] = Ordering.by(_.course_name)

  lazy val all: Maker[Seq[Course]] =
    for {
      courses <- Gitolite.course_names
    } yield for {
      cn <- courses.toSeq
    } yield Course(cn)

  lazy val active_courses: Maker[Seq[Course]] = Maker.select(all)(_.active)

}
