package ag.grader

import language.experimental.namedTuples

import scala.collection.{SortedMap, SortedSet}
import upickle.default.ReadWriter
import ag.common.given_ReadWriter_SortedMap

@upickle.implicits.allowUnknownKeys(false)
case class RawCourse(
    active: Boolean,
    notifications: NotificationConfig,
    projects: SortedMap[String, RawProject],
    staff: SortedSet[CSID] = SortedSet()
) derives ReadWriter
