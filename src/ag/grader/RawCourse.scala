package ag.grader

import scala.collection.{SortedMap, SortedSet}
import upickle.default.{ReadWriter}
import ag.rules.{given_ReadWriter_SortedMap}

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
