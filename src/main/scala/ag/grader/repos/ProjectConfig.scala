package ag.grader.repos

import ag.common.{
  ToRelPath,
  given_ReadWriter_LocalDateTime,
  given_ReadWriter_SortedSet
}
import ag.grader.{
  CSID,
  CourseName,
  Extensions,
  Iterations,
  Phase,
  ProjectName,
  RawProject,
  Weight
}
import ag.r3.Target

import java.time.LocalDateTime
import scala.collection.SortedSet

// A project's settings, from courses_config (see CoursesConfig), as Project
// has them in ag.r2. They fail if courses.json doesn't have the project:
// `find` says if it's there.
final case class ProjectConfig(c: CourseName, p: ProjectName, env: Env)
    extends Instance[ProjectConfig] derives ToRelPath {

  lazy val find: Target[Option[RawProject]] =
    target((cs = CoursesConfig(env).courses))((_, deps) =>
      deps.cs.get(c).flatMap(_.projects.get(p.value))
    )

  private lazy val project: Target[RawProject] =
    target((rp = find))((_, deps) =>
      deps.rp.getOrElse(throw Exception(s"no project $c/$p in courses_config"))
    )

  // the course and the project are active, and the project's repo is there
  lazy val active: Target[Boolean] =
    target(
      (
        course = CourseConfig(c, env).active,
        rp = project,
        repo = ProjectRepo(c, p, env).has_master
      )
    )((_, deps) => deps.course && deps.rp.active && deps.repo)

  lazy val phase1_tests: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.phase1_tests.to(SortedSet))
  lazy val phase1_weight: Target[Int] =
    target((rp = project))((_, d) => d.rp.phase1_weight)
  lazy val phase2_tests: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.phase2_tests.to(SortedSet))
  lazy val phase2_weight: Target[Int] =
    target((rp = project))((_, d) => d.rp.phase2_weight)
  lazy val test_weights: Target[Seq[Weight]] =
    target((rp = project))((_, d) => d.rp.test_weights)
  lazy val bad_tests: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.bad_tests.to(SortedSet))
  lazy val cores: Target[Int] =
    target((rp = project))((_, d) => d.rp.cores)
  lazy val iterations: Target[Iterations] =
    target((rp = project))((_, d) => d.rp.iterations)
  lazy val test_cutoff: Target[LocalDateTime] =
    target((rp = project))((_, d) => d.rp.test_cutoff)
  lazy val code_cutoff: Target[LocalDateTime] =
    target((rp = project))((_, d) => d.rp.code_cutoff)
  lazy val extensions: Target[Extensions] =
    target((rp = project))((_, d) => d.rp.extensions)
  lazy val test_extensions: Target[SortedSet[String]] =
    target((rp = project))((_, d) => d.rp.test_extensions.to(SortedSet))
  // the name of its docker file (building the image is grading's business)
  lazy val docker_file: Target[Option[String]] =
    target((rp = project))((_, d) => d.rp.docker_file)
  lazy val staff: Target[SortedSet[CSID]] =
    target((rp = project))((_, d) => d.rp.staff.to(SortedSet))

  // a student's deadline for grading their code in a phase, with any
  // extension they have (as Project.deadline)
  lazy val deadline: (CSID, Phase) => Target[LocalDateTime] = fun {
    (csid: CSID, phase: Phase) =>
      target(
        (first = test_cutoff, second = code_cutoff, ext = extensions)
      ) { (_, deps) =>
        phase
          .deadline(deps.first, deps.second)
          .getOrElse(deps.second)
          .plusDays(deps.ext.days(csid, phase))
          .nn
      }
  }
}
