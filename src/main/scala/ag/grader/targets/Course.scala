package ag.grader.targets

import ag.grader.web.ReportInputs

import ag.grader.name.{CSID, CourseName, ProjectName}

import ag.grader.json.{RawCourse, NotificationConfig}

import ag.common.{
  ToRelPath,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.grader.repos.{ClassRepo, CoursesConfig}
import ag.rules.{DerivedScope, Source, Target}

import scala.collection.{SortedMap, SortedSet}
import scala.util.matching.Regex

// A course's targets: its settings, from courses_config (see
// CoursesConfig), which fail if courses.json doesn't have the course
// (`find` says if it's there), and what's worked out from them.
final case class Course(c: CourseName, source: Source)
    extends DerivedScope[Course] derives ToRelPath {

  // its project named `p` (whether or not courses.json has it)
  def project(p: ProjectName): Project = Project(this, p)

  lazy val find: Target[Option[RawCourse]] =
    target((cs = CoursesConfig(source).courses))((_, deps) => deps.cs.get(c))

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
        tracker.track_all(names.map(project(_).active))
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

  // ---- enrollment ------------------------------------------------------

  // csid -> their ssh key, from the course's enrollment (see
  // repos.EnrollmentRepo)
  lazy val enrollment: Target[SortedMap[CSID, String]] =
    target(
      (m = source.branch(ClassRepo.name_of(c), ClassRepo.enrollment_branch))
    ) { (producer, deps) =>
      deps.m.flatMap(m =>
        producer.read_text(producer.commit(m).tree, os.rel / "enrollment.json")
      ) match {
        case Some(json) => upickle.default.read[SortedMap[CSID, String]](json)
        case None       =>
          if (deps.m.isDefined)
            producer.log(
              s"${ClassRepo.name_of(c)}'s ${ClassRepo.enrollment_branch} has no enrollment.json\n"
            )
          SortedMap[CSID, String]()
      }
    }

  lazy val enrolled_csids: Target[SortedSet[CSID]] =
    target((e = enrollment))((_, deps) => deps.e.keySet)

  // its enrolled students whose CSIDs match `re`
  lazy val enrolled: Regex => Target[SortedSet[CSID]] = fun { (re: Regex) =>
    target((e = enrolled_csids))((_, deps) =>
      deps.e.filter(csid => re.matches(csid.value))
    )
  }

  // ---- report pages ----------------------------------------------------

  // what every active project's report pages show (see
  // Project.report_inputs)
  lazy val site_inputs: Target[Seq[ReportInputs]] =
    complex_target[Seq[ReportInputs]] { tracker =>
      val projects =
        tracker.track(active_projects(CoursesConfig.any)).toSeq
      val inputs =
        tracker.track_all(
          projects.map(project(_).report_inputs)
        )
      tracker.run_if_needed(_ => inputs)
    }
}
