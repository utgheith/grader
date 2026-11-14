package ag.grader.repos

import ag.common.{ToRelPath, given_ReadWriter_SortedSet}
import ag.grader.{CSID, CourseName, NotificationConfig, ProjectName, RawCourse}
import ag.r3.Target

import scala.collection.SortedSet
import scala.util.matching.Regex

// A course's settings, from courses_config (see CoursesConfig). They fail
// if courses.json doesn't have the course: `find` says if it's there.
final case class CourseConfig(c: CourseName, env: Env)
    extends Instance[CourseConfig] derives ToRelPath {

  lazy val find: Target[Option[RawCourse]] =
    target((cs = CoursesConfig(env).courses))((_, deps) => deps.cs.get(c))

  private lazy val course: Target[RawCourse] =
    target((rc = find))((_, deps) =>
      deps.rc.getOrElse(throw Exception(s"no course $c in courses_config"))
    )

  lazy val active: Target[Boolean] =
    target((rc = course))((_, deps) => deps.rc.active)
  lazy val staff: Target[SortedSet[CSID]] =
    target((rc = course))((_, deps) => deps.rc.staff)
  lazy val notifications: Target[NotificationConfig] =
    target((rc = course))((_, deps) => deps.rc.notifications)
  lazy val project_names: Target[SortedSet[ProjectName]] =
    target((rc = course))((_, deps) =>
      deps.rc.projects.keySet.map(ProjectName(_))
    )

  private lazy val all_active_projects: Target[SortedSet[ProjectName]] =
    complex_target[SortedSet[ProjectName]] { tracker =>
      val names = tracker.track(project_names).toSeq
      val flags =
        tracker.track_all(names.map(p => ProjectConfig(c, p, env).active))
      tracker.run_if_needed(_ =>
        names.zip(flags).collect { case (n, true) => n }.to(SortedSet)
      )
    }

  // its active projects whose names match the regex
  lazy val active_projects: Regex => Target[SortedSet[ProjectName]] = fun {
    (re: Regex) =>
      target((all = all_active_projects))((_, deps) =>
        deps.all.filter(p => re.matches(p.value))
      )
  }
}
