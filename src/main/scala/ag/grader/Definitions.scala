package ag.grader

import ag.r3.{Source, Target}

import java.time.LocalDateTime
import scala.collection.{SortedMap, SortedSet}
import scala.util.matching.Regex

// The course and project definitions, as they were before they moved to
// repos.CoursesConfig and repos.ProjectRepo: kept for the layers and
// commands that use them, until they use those directly.
final class Definitions(val source: Source) {
  private val env = repos.Env(source)
  private def course(c: CourseName) = repos.CourseConfig(c, env)
  private def project(c: CourseName, p: ProjectName) =
    repos.ProjectConfig(c, p, env)

  def project_repo(c: CourseName, p: ProjectName): String =
    repos.ProjectRepo.name_of(c, p)

  def courses: Target[SortedMap[CourseName, RawCourse]] =
    repos.CoursesConfig(env).courses
  def course_names: Target[SortedSet[CourseName]] =
    repos.CoursesConfig(env).course_names
  def find_course(c: CourseName): Target[Option[RawCourse]] = course(c).find
  def course_active(c: CourseName): Target[Boolean] = course(c).active
  def course_staff(c: CourseName): Target[SortedSet[CSID]] = course(c).staff
  def notifications(c: CourseName): Target[NotificationConfig] =
    course(c).notifications
  def project_names(c: CourseName): Target[SortedSet[ProjectName]] =
    course(c).project_names
  def active_courses(re: Regex): Target[SortedSet[CourseName]] =
    repos.CoursesConfig(env).active_courses(re)
  def find_project(c: CourseName, p: ProjectName): Target[Option[RawProject]] =
    project(c, p).find
  def project_repo_exists(c: CourseName, p: ProjectName): Target[Boolean] =
    repos.ProjectRepo(c, p, env).has_master
  def project_active(c: CourseName, p: ProjectName): Target[Boolean] =
    project(c, p).active
  def active_projects(
      c: CourseName,
      re: Regex
  ): Target[SortedSet[ProjectName]] =
    course(c).active_projects(re)

  def phase1_tests(c: CourseName, p: ProjectName): Target[SortedSet[String]] =
    project(c, p).phase1_tests
  def phase1_weight(c: CourseName, p: ProjectName): Target[Int] =
    project(c, p).phase1_weight
  def phase2_tests(c: CourseName, p: ProjectName): Target[SortedSet[String]] =
    project(c, p).phase2_tests
  def phase2_weight(c: CourseName, p: ProjectName): Target[Int] =
    project(c, p).phase2_weight
  def test_weights(c: CourseName, p: ProjectName): Target[Seq[Weight]] =
    project(c, p).test_weights
  def bad_tests(c: CourseName, p: ProjectName): Target[SortedSet[String]] =
    project(c, p).bad_tests
  def cores(c: CourseName, p: ProjectName): Target[Int] = project(c, p).cores
  def iterations(c: CourseName, p: ProjectName): Target[Iterations] =
    project(c, p).iterations
  def test_cutoff(c: CourseName, p: ProjectName): Target[LocalDateTime] =
    project(c, p).test_cutoff
  def code_cutoff(c: CourseName, p: ProjectName): Target[LocalDateTime] =
    project(c, p).code_cutoff
  def extensions(c: CourseName, p: ProjectName): Target[Extensions] =
    project(c, p).extensions
  def test_extensions(
      c: CourseName,
      p: ProjectName
  ): Target[SortedSet[String]] =
    project(c, p).test_extensions
  def docker_file(c: CourseName, p: ProjectName): Target[Option[String]] =
    project(c, p).docker_file
  def project_staff(c: CourseName, p: ProjectName): Target[SortedSet[CSID]] =
    project(c, p).staff
  def deadline(
      c: CourseName,
      p: ProjectName,
      csid: CSID,
      phase: Phase
  ): Target[LocalDateTime] = project(c, p).deadline(csid, phase)
}

object Definitions {
  // see repos.CoursesConfig.any
  val any: Regex = repos.CoursesConfig.any
}
