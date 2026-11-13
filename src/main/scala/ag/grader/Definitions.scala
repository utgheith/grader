package ag.grader

import ag.common.{
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.r3.{Scope, Source, Target}

import java.time.LocalDateTime
import scala.collection.{SortedMap, SortedSet}

// What the courses and their projects are, as r3 targets on `source` (a
// local mirror of the git server): courses_config's courses.json, and whether
// each project's repo is there. The r3 form of what ag.r2 works out in
// Gitolite (raw_courses, raw_course, raw_project), Course and Project's
// settings; shared by the status pages (StatusData) and grading.
//
// Courses and projects are named by strings, as in the repos' names. Each
// setting is a target of its own, so what reads one isn't worked out again
// when another changes. A project's settings fail if courses.json doesn't
// have it (as ag.r2's do): find_project is the one that says if it's there.
final class Definitions(val source: Source) extends Scope("definitions") {

  def project_repo(c: String, p: String): String = s"${c}_$p"

  // courses.json, or no courses if courses_config or the file isn't there
  lazy val courses: Target[SortedMap[String, RawCourse]] =
    target((m = source.master("courses_config"))) { (producer, deps) =>
      (for {
        c <- deps.m
        json <- producer.read_text(
          producer.commit(c).tree,
          os.rel / "courses.json"
        )
      } yield upickle.default
        .read[Map[String, RawCourseNotSorted]](json)
        .view
        .mapValues(_.sorted)
        .to(SortedMap)).getOrElse(SortedMap())
    }

  lazy val course_names: Target[SortedSet[String]] =
    target((cs = courses))((_, deps) => deps.cs.keySet)

  lazy val find_course: String => Target[Option[RawCourse]] = fun {
    (c: String) => target((cs = courses))((_, deps) => deps.cs.get(c))
  }

  private lazy val course: String => Target[RawCourse] = fun { (c: String) =>
    target((rc = find_course(c)))((_, deps) =>
      deps.rc.getOrElse(throw Exception(s"no course $c in courses_config"))
    )
  }

  lazy val course_active: String => Target[Boolean] = fun { (c: String) =>
    target((rc = course(c)))((_, deps) => deps.rc.active)
  }

  lazy val course_staff: String => Target[SortedSet[CSID]] = fun {
    (c: String) => target((rc = course(c)))((_, deps) => deps.rc.staff)
  }

  lazy val notifications: String => Target[NotificationConfig] = fun {
    (c: String) => target((rc = course(c)))((_, deps) => deps.rc.notifications)
  }

  lazy val project_names: String => Target[SortedSet[String]] = fun {
    (c: String) =>
      target((rc = course(c)))((_, deps) => deps.rc.projects.keySet)
  }

  lazy val active_courses: Target[SortedSet[String]] = complex_target {
    tracker =>
      val names = tracker.track(course_names).toSeq
      val flags = tracker.track_all(names.map(course_active))
      tracker.run_if_needed(_ =>
        names.zip(flags).collect { case (n, true) => n }.to(SortedSet)
      )
  }

  lazy val find_project: (String, String) => Target[Option[RawProject]] = fun {
    (c: String, p: String) =>
      target((cs = courses))((_, deps) =>
        deps.cs.get(c).flatMap(_.projects.get(p))
      )
  }

  private lazy val project: (String, String) => Target[RawProject] = fun {
    (c: String, p: String) =>
      target((rp = find_project(c, p)))((_, deps) =>
        deps.rp.getOrElse(
          throw Exception(s"no project $c/$p in courses_config")
        )
      )
  }

  // whether the project's repo is in the mirror (has a master)
  lazy val project_repo_exists: (String, String) => Target[Boolean] = fun {
    (c: String, p: String) =>
      target((m = source.master(project_repo(c, p))))((_, deps) =>
        deps.m.isDefined
      )
  }

  // the course and the project are active, and the project's repo is there
  lazy val project_active: (String, String) => Target[Boolean] = fun {
    (c: String, p: String) =>
      target(
        (
          course = course_active(c),
          rp = project(c, p),
          repo = project_repo_exists(c, p)
        )
      )((_, deps) => deps.course && deps.rp.active && deps.repo)
  }

  lazy val active_projects: String => Target[SortedSet[String]] = fun {
    (c: String) =>
      complex_target[SortedSet[String]] { tracker =>
        val names = tracker.track(project_names(c)).toSeq
        val flags = tracker.track_all(names.map(p => project_active(c, p)))
        tracker.run_if_needed(_ =>
          names.zip(flags).collect { case (n, true) => n }.to(SortedSet)
        )
      }
  }

  // --- a project's settings, one target each ---------------------------

  lazy val phase1_tests: (String, String) => Target[SortedSet[String]] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) =>
        deps.rp.phase1_tests.to(SortedSet)
      )
  }
  lazy val phase1_weight: (String, String) => Target[Int] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.phase1_weight)
  }
  lazy val phase2_tests: (String, String) => Target[SortedSet[String]] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) =>
        deps.rp.phase2_tests.to(SortedSet)
      )
  }
  lazy val phase2_weight: (String, String) => Target[Int] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.phase2_weight)
  }
  lazy val test_weights: (String, String) => Target[Seq[Weight]] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.test_weights)
  }
  lazy val bad_tests: (String, String) => Target[SortedSet[String]] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.bad_tests.to(SortedSet))
  }
  lazy val cores: (String, String) => Target[Int] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.cores)
  }
  lazy val iterations: (String, String) => Target[Iterations] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.iterations)
  }
  lazy val test_cutoff: (String, String) => Target[LocalDateTime] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.test_cutoff)
  }
  lazy val code_cutoff: (String, String) => Target[LocalDateTime] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.code_cutoff)
  }
  lazy val extensions: (String, String) => Target[Extensions] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.extensions)
  }
  lazy val test_extensions: (String, String) => Target[SortedSet[String]] =
    fun { (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) =>
        deps.rp.test_extensions.to(SortedSet)
      )
    }
  // the name of its docker file (building the image is grading's business)
  lazy val docker_file: (String, String) => Target[Option[String]] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.docker_file)
  }
  lazy val project_staff: (String, String) => Target[SortedSet[CSID]] = fun {
    (c: String, p: String) =>
      target((rp = project(c, p)))((_, deps) => deps.rp.staff.to(SortedSet))
  }

  // `csid`'s deadline for grading their code in `phase`, with any extension
  // they have (as Project.deadline)
  lazy val deadline: (String, String, CSID, Phase) => Target[LocalDateTime] =
    fun { (c: String, p: String, csid: CSID, phase: Phase) =>
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
