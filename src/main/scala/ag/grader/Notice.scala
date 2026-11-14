package ag.grader

import upickle.default.ReadWriter

// An email to send once the repo it's about is synced (see Outbox.sync):
// to `to`, copied to `cc`. The r3 form of NotificationConfig's key and
// repo emails (result emails aren't sent any more).
@upickle.implicits.allowUnknownKeys(false)
final case class Notice(to: CSID, cc: Option[CSID], what: Notice.What)
    derives ReadWriter {

  def subject(server: RemoteServer): String = what match {
    case Notice.What.KeyUpdated(course, csid) =>
      s"key updated for $course::${csid.toString}"
    case Notice.What.RepoCreated(repo, _) =>
      s"git clone ${server.git_uri(repo)}"
  }

  def body(server: RemoteServer): String = what match {
    case Notice.What.KeyUpdated(_, _) =>
      s"""
      |To check your connection:
      |    ssh ${server.ssh_uri} info
      """.stripMargin
    case Notice.What.RepoCreated(_, readme) =>
      s"""$readme
         |
         |To clone:
         |    ${subject(server)}
      """.stripMargin
  }

  // Mails it, if `can_send` (config.json's can_send_mail); otherwise says
  // what it would have sent
  def send(server: RemoteServer, can_send: Boolean, log: String => Unit): Unit =
    if (can_send) {
      ag.rules.refuse_if_read_only(s"send mail to ${to.toString}")
      log(s"mailing ${to.toString}: ${subject(server)}")
      val _ = os
        .proc(
          "mail",
          cc.toSeq.flatMap(c => Seq("-c", c.value)),
          "-s",
          subject(server),
          to.value
        )
        .call(stdin = body(server))
    } else
      log(s"not mailing (can_send_mail) ${to.toString}: ${subject(server)}")
}

object Notice {
  enum What derives ReadWriter {
    // a student's key, added or changed in gitolite-admin
    case KeyUpdated(course: CourseName, csid: CSID)
    // a student's repo made, which starts with `readme`
    case RepoCreated(repo: String, readme: String)
  }

  // Who's told about `what`, which is for `csid`, as the course's settings
  // say (as NotificationConfig.send): the student (copying cc), or just cc,
  // or nobody
  def routed(n: NotificationConfig, csid: CSID, what: What): Seq[Notice] =
    (n.send_to_student, n.cc) match {
      case (true, Some(cc)) if csid != cc => Seq(Notice(csid, Some(cc), what))
      case (true, _)                      => Seq(Notice(csid, None, what))
      case (false, Some(cc))              => Seq(Notice(cc, None, what))
      case (false, None)                  => Seq()
    }
}
