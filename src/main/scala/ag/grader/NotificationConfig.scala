package ag.grader

import ag.common.{say, shout}
import upickle.default.ReadWriter
import ag.rules.check

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
      ag.rules.refuse_if_read_only(s"send mail to ${to.toString}")
      shout(
        s"sending $send_to_student ${to.toString} ${cc.toString} $subject"
      )
      os.proc(
        "mail",
        cc match {
          case Some(cc) => Seq("-c", cc.value)
          case None     => Seq()
        },
        "-s",
        subject,
        to.value
      ).check(stdin = contents)
    } else {
      shout(
        s"----- not sending $send_to_student ${to.toString} ${cc.toString} $subject"
      )
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
        say(s"not sending '$subject to ${csid.toString}'")
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
        s"key updated for ${course.course_name}::${csid.toString}",
        s"""
      |To check your connection:
      |    ssh ${server.ssh_uri} info
      """.stripMargin,
        can_send
      )
    } else {
      say(
        s"    key update notification disabled for ${course.course_name}::${csid.toString}"
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
      s"    repo creation notification is disabled for ${project.course.course_name}::${project.project_name}::${csid.toString}"
    )

  }
}
