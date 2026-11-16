package ag.grader.repos

import ag.grader.targets.Course

import ag.grader.name.CourseName

import ag.grader.json.{RawCourse, RawCourseNotSorted}

import ag.grader.gitolite.ReadOnlyRepo

import ag.common.{
  ToRelPath,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.rules.{Source, Target}

import scala.collection.{SortedMap, SortedSet}
import scala.util.matching.Regex

// courses_config, which the staff keep: what the courses and their projects
// are (courses.json). A course's settings are in Course, a project's in
// Project: each setting a target of its own, so what reads one isn't worked
// out again when another changes. `course` is where the rest start.
final case class CoursesConfig(source: Source)
    extends ReadOnlyRepo[CoursesConfig] derives ToRelPath {
  def repo_name: String = "courses_config"

  // the course named `c` (whether or not courses.json has it)
  def course(c: CourseName): Course = Course(c, source)

  // courses.json, or no courses if courses_config or the file isn't there
  lazy val courses: Target[SortedMap[CourseName, RawCourse]] =
    target((m = inspect)) { (producer, deps) =>
      (for {
        c <- deps.m
        json <- producer.read_text(
          producer.commit(c).tree,
          os.rel / "courses.json"
        )
      } yield upickle.default
        .read[Map[String, RawCourseNotSorted]](json)
        .map((c, rc) => (CourseName(c), rc.sorted))
        .to(SortedMap)).getOrElse(SortedMap())
    }

  lazy val course_names: Target[SortedSet[CourseName]] =
    target((cs = courses))((_, deps) => deps.cs.keySet)

  private lazy val all_active_courses: Target[SortedSet[CourseName]] =
    complex_target[SortedSet[CourseName]] { tracker =>
      val names = tracker.track(course_names).toSeq
      val flags =
        tracker.track_all(names.map(course(_).active))
      tracker.run_if_needed(_ =>
        names.zip(flags).collect { case (n, true) => n }.to(SortedSet)
      )
    }

  // the active courses whose names match the regex (any: all)
  lazy val active_courses: Regex => Target[SortedSet[CourseName]] = fun {
    (re: Regex) =>
      target((all = all_active_courses))((_, deps) =>
        deps.all.filter(c => re.matches(c.value))
      )
  }
}

object CoursesConfig {
  // matches every name: for active_courses, active_projects and
  // Course.enrolled, when all are wanted
  val any: Regex = ".*".r
}
