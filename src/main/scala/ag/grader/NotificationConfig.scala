package ag.grader

import upickle.default.ReadWriter

// Whom a course tells about what (courses.json's notifications); see
// Notice.routed for how it's applied
@upickle.implicits.allowUnknownKeys(false)
case class NotificationConfig(
    send_to_student: Boolean,
    cc: Option[CSID],
    key_update: Boolean,
    result_update: Boolean,
    repo_create: Boolean,
    site_base: Option[String]
) derives ReadWriter
