package ag.grader

import language.experimental.namedTuples

import scala.collection.{SortedMap, SortedSet}
import upickle.default.ReadWriter

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
