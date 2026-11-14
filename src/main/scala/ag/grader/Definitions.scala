package ag.grader

import ag.common.{
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.r3.{Scope, Source, Target}

import java.time.LocalDateTime
import scala.collection.{SortedMap, SortedSet}
import scala.util.matching.Regex

// What the courses and their projects are, as r3 targets on `source` (a
// local mirror of the git server): courses_config's courses.json, and whether
// each project's repo is there. The r3 form of what ag.r2 works out in
// Gitolite (raw_courses, raw_course, raw_project), Course and Project's
// settings; shared by the status pages (StatusData) and grading.
//
// Courses and projects are named by CourseName and ProjectName (see Names). Each
// setting is a target of its own, so what reads one isn't worked out again
// when another changes. A project's settings fail if courses.json doesn't
// have it (as ag.r2's do): find_project is the one that says if it's there.
final class Definitions(val source: Source) extends Scope("definitions") {

  def project_repo(c: CourseName, p: ProjectName): String = s"${c}_$p"

  // courses.json, or no courses if courses_config or the file isn't there
  lazy val courses: Target[SortedMap[CourseName, RawCourse]] =
    target((m = source.master("courses_config"))) { (producer, deps) =>
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

  lazy val find_course: CourseName => Target[Option[RawCourse]] = fun {
    (c: CourseName) => target((cs = courses))((_, deps) => deps.cs.get(c))
  }

  private lazy val course: CourseName => Target[RawCourse] = fun {
    (c: CourseName) =>
      target((rc = find_course(c)))((_, deps) =>
        deps.rc.getOrElse(throw Exception(s"no course $c in courses_config"))
      )
  }

  lazy val course_active: CourseName => Target[Boolean] = fun {
    (c: CourseName) =>
      target((rc = course(c)))((_, deps) => deps.rc.active)
  }

  lazy val course_staff: CourseName => Target[SortedSet[CSID]] = fun {
    (c: CourseName) => target((rc = course(c)))((_, deps) => deps.rc.staff)
  }

  lazy val notifications: CourseName => Target[NotificationConfig] = fun {
    (c: CourseName) =>
      target((rc = course(c)))((_, deps) => deps.rc.notifications)
  }

  lazy val project_names: CourseName => Target[SortedSet[ProjectName]] = fun {
    (c: CourseName) =>
      target((rc = course(c)))((_, deps) =>
        deps.rc.projects.keySet.map(ProjectName(_))
      )
  }

  private lazy val all_active_courses: Target[SortedSet[CourseName]] =
    complex_target { tracker =>
      val names = tracker.track(course_names).toSeq
      val flags = tracker.track_all(names.map(course_active))
      tracker.run_if_needed(_ =>
        names.zip(flags).collect { case (n, true) => n }.to(SortedSet)
      )
    }

  // the active courses whose names match `re` (Definitions.any: all)
  lazy val active_courses: Regex => Target[SortedSet[CourseName]] = fun {
    (re: Regex) =>
      target((all = all_active_courses))((_, deps) =>
        deps.all.filter(c => re.matches(c.value))
      )
  }

  lazy val find_project
      : (CourseName, ProjectName) => Target[Option[RawProject]] = fun {
    (c: CourseName, p: ProjectName) =>
      target((cs = courses))((_, deps) =>
        deps.cs.get(c).flatMap(_.projects.get(p.value))
      )
  }

  private lazy val project: (CourseName, ProjectName) => Target[RawProject] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = find_project(c, p)))((_, deps) =>
        deps.rp.getOrElse(
          throw Exception(s"no project $c/$p in courses_config")
        )
      )
    }

  // whether the project's repo is in the mirror (has a master)
  lazy val project_repo_exists: (CourseName, ProjectName) => Target[Boolean] =
    fun { (c: CourseName, p: ProjectName) =>
      target((m = source.master(project_repo(c, p))))((_, deps) =>
        deps.m.isDefined
      )
    }

  // the course and the project are active, and the project's repo is there
  lazy val project_active: (CourseName, ProjectName) => Target[Boolean] = fun {
    (c: CourseName, p: ProjectName) =>
      target(
        (
          course = course_active(c),
          rp = project(c, p),
          repo = project_repo_exists(c, p)
        )
      )((_, deps) => deps.course && deps.rp.active && deps.repo)
  }

  private lazy val all_active_projects
      : CourseName => Target[SortedSet[ProjectName]] =
    fun { (c: CourseName) =>
      complex_target[SortedSet[ProjectName]] { tracker =>
        val names = tracker.track(project_names(c)).toSeq
        val flags = tracker.track_all(names.map(p => project_active(c, p)))
        tracker.run_if_needed(_ =>
          names.zip(flags).collect { case (n, true) => n }.to(SortedSet)
        )
      }
    }

  // the course's active projects whose names match `re`
  lazy val active_projects
      : (CourseName, Regex) => Target[SortedSet[ProjectName]] = fun {
    (c: CourseName, re: Regex) =>
      target((all = all_active_projects(c)))((_, deps) =>
        deps.all.filter(p => re.matches(p.value))
      )
  }

  // --- a project's settings, one target each ---------------------------

  lazy val phase1_tests
      : (CourseName, ProjectName) => Target[SortedSet[String]] = fun {
    (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) =>
        deps.rp.phase1_tests.to(SortedSet)
      )
  }
  lazy val phase1_weight: (CourseName, ProjectName) => Target[Int] = fun {
    (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.phase1_weight)
  }
  lazy val phase2_tests
      : (CourseName, ProjectName) => Target[SortedSet[String]] = fun {
    (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) =>
        deps.rp.phase2_tests.to(SortedSet)
      )
  }
  lazy val phase2_weight: (CourseName, ProjectName) => Target[Int] = fun {
    (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.phase2_weight)
  }
  lazy val test_weights: (CourseName, ProjectName) => Target[Seq[Weight]] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.test_weights)
    }
  lazy val bad_tests: (CourseName, ProjectName) => Target[SortedSet[String]] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.bad_tests.to(SortedSet))
    }
  lazy val cores: (CourseName, ProjectName) => Target[Int] = fun {
    (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.cores)
  }
  lazy val iterations: (CourseName, ProjectName) => Target[Iterations] = fun {
    (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.iterations)
  }
  lazy val test_cutoff: (CourseName, ProjectName) => Target[LocalDateTime] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.test_cutoff)
    }
  lazy val code_cutoff: (CourseName, ProjectName) => Target[LocalDateTime] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.code_cutoff)
    }
  lazy val extensions: (CourseName, ProjectName) => Target[Extensions] = fun {
    (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.extensions)
  }
  lazy val test_extensions
      : (CourseName, ProjectName) => Target[SortedSet[String]] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) =>
        deps.rp.test_extensions.to(SortedSet)
      )
    }
  // the name of its docker file (building the image is grading's business)
  lazy val docker_file: (CourseName, ProjectName) => Target[Option[String]] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.docker_file)
    }
  lazy val project_staff: (CourseName, ProjectName) => Target[SortedSet[CSID]] =
    fun { (c: CourseName, p: ProjectName) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.staff.to(SortedSet))
    }

  // `csid`'s deadline for grading their code in `phase`, with any extension
  // they have (as Project.deadline)
  lazy val deadline
      : (CourseName, ProjectName, CSID, Phase) => Target[LocalDateTime] =
    fun { (c: CourseName, p: ProjectName, csid: CSID, phase: Phase) =>
      target(
        (
          first = test_cutoff(c, p),
          second = code_cutoff(c, p),
          ext = extensions(c, p)
        )
      ) { (_, deps) =>
        phase
          .deadline(deps.first, deps.second)
          .getOrElse(deps.second)
          .plusDays(deps.ext.days(csid, phase))
          .nn
      }
    }
}

object Definitions {
  // matches every name: for active_courses, active_projects and
  // Submissions.enrolled, when all are wanted
  val any: Regex = ".*".r
}
