package ag.grader

import upickle.default.{ReadWriter}
import ag.rules.{run, say}

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
      contents: => String,
      can_send: Boolean
  ): Unit = {
    if (can_send) {
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
    } else {
      println(s"----- not sending $send_to_student $to $cc $subject")
    }
  }

  private def send(
      csid: CSID,
      subject: => String,
      contents: => String,
      can_send: Boolean
  ): Unit = {
    (send_to_student, cc) match {
      case (true, Some(cc)) if csid != cc =>
        doit(csid, Some(cc), subject, contents, can_send)
      case (true, _) =>
        doit(csid, None, subject, contents, can_send)
      case (false, Some(cc)) =>
        doit(cc, None, subject, contents, can_send)
      case (false, None) =>
        say(s"not sending '$subject to $csid'")
    }
  }

  def send_key_update(
      course: Course,
      csid: CSID,
      server: RemoteServer,
      can_send: Boolean
  ): Unit =
    if (key_update) {
      send(
        csid,
        s"key updated for ${course.course_name}::$csid",
        s"""
      |To check your connection:
      |    ssh ${server.ssh_uri} info
      """.stripMargin,
        can_send
      )
    } else {
      say(
        s"    key update notification disabled for ${course.course_name}::$csid"
      )
    }

  def send_repo_created(
      project: Project,
      repo_info: RepoInfo,
      csid: CSID,
      can_send: Boolean,
      path: os.Path
  ): Unit = if (repo_create) {
    val subject =
      s"git clone ${repo_info.server.git_uri(project.work_repo_name(csid))}"
    val readme = os.read(path / "README")
    send(
      csid,
      subject,
      s"""$readme
         |
         |To clone:
         |    $subject
      """.stripMargin,
      can_send
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
      student_results: RedactedStudentResults,
      can_send: Boolean
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
      contents,
      can_send
    )

  } else {
    say(
      s"    result update notification is disabled for ${project.course.course_name}::${project.project_name}::${csid}"
    )
  }
}
