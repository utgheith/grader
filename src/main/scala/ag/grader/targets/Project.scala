package ag.grader.targets

import ag.grader.{Milestone, ProjectState}

import ag.grader.web.{ReportInputs, HtmlGen}

import ag.grader.name.{Alias, CSID, CourseName, ProjectName}

import ag.grader.json.{
  RawProject,
  RawMilestone,
  Iterations,
  Weight,
  RedactedStudentResults,
  TestId
}

import ag.common.{
  NoNulls,
  ToRelPath,
  given_ReadWriter_SortedMap,
  given_ReadWriter_SortedSet
}
import ag.grader.repos.{CoursesConfig, ProjectRepo}
import ag.rules.{
  Commit,
  Context,
  DerivedScope,
  Producer,
  Source,
  Target,
  Tree,
  periodic
}

import java.time.LocalDateTime
import scala.collection.{SortedMap, SortedSet}
import scala.util.Try

// A project's targets: its settings, from courses_config (see
// CoursesConfig), which fail if courses.json doesn't have the project
// (`find` says if it's there), and what's worked out from them.
final case class Project(course: Course, p: ProjectName)
    extends DerivedScope[Project] derives ToRelPath {
  val source: Source = course.source
  def c: CourseName = course.c
  // as the grader names it: course_project
  def name: String = s"${c}_$p"

  // a student's targets for it, and its targets for a milestone
  def submission(csid: CSID): Submission = Submission(this, csid)
  def milestone(m: Milestone): ProjectMilestone = ProjectMilestone(this, m)

  lazy val find: Target[Option[RawProject]] =
    target((cs = CoursesConfig(source).courses))((_, deps) =>
      deps.cs.get(c).flatMap(_.projects.get(p.value))
    )

  // its settings (for ProjectMilestone and SubmissionMilestone too)
  private[grader] lazy val raw: Target[RawProject] =
    target((rp = find))((_, deps) =>
      deps.rp.getOrElse(throw Exception(s"no project $c/$p in courses_config"))
    )

  // the course and the project are active, and the project's repo is there
  lazy val active: Target[Boolean] =
    target(
      (
        course = course.active,
        rp = raw,
        repo = ProjectRepo(this).has_master
      )
    )((_, deps) => deps.course && deps.rp.active && deps.repo)

  lazy val test_weights: Target[Seq[Weight]] =
    target((rp = raw))((_, d) => d.rp.test_weights)
  lazy val bad_tests: Target[SortedSet[String]] =
    target((rp = raw))((_, d) => d.rp.bad_tests.to(SortedSet))
  lazy val cores: Target[Int] =
    target((rp = raw))((_, d) => d.rp.cores)
  lazy val iterations: Target[Iterations] =
    target((rp = raw))((_, d) => d.rp.iterations)
  lazy val milestones: Target[Seq[RawMilestone]] =
    target((rp = raw))((_, d) => d.rp.milestones)
  lazy val test_extensions: Target[SortedSet[String]] =
    target((rp = raw))((_, d) => d.rp.test_extensions.to(SortedSet))
  // the name of its docker file (building the image is grading's business)
  lazy val docker_file: Target[Option[String]] =
    target((rp = raw))((_, d) => d.rp.docker_file)
  lazy val staff: Target[SortedSet[CSID]] =
    target((rp = raw))((_, d) => d.rp.staff.to(SortedSet))

  // ---- submissions, and what the grader published, read back -------

  // the enrolled students' submissions
  lazy val submissions: Target[SortedMap[CSID, Commit]] =
    complex_target[SortedMap[CSID, Commit]] { tracker =>
      val csids = tracker.track(course.enrolled_csids).toSeq
      val subs =
        tracker.track_all(
          csids.map(submission(_).submission)
        )
      tracker.run_if_needed(_ =>
        csids
          .zip(subs)
          .collect { case (csid, Some(s)) => (csid, s) }
          .to(SortedMap)
      )
    }

  lazy val students_with_submission: Target[SortedSet[CSID]] =
    target((s = submissions))((_, deps) => deps.s.keySet)

  // The project's aliases as they should be: the published ones, and a new
  // one for each student with a submission who has none, numbered on from
  // the highest (as Project.publish_aliases).
  lazy val aliases: Target[SortedMap[CSID, Alias]] =
    target(
      (published = published_aliases, csids = students_with_submission)
    ) { (_, deps) =>
      val fresh = deps.csids.toSeq.filterNot(deps.published.contains)
      val next = deps.published.values.foldLeft(0)(_ max _.value) + 1
      deps.published ++ fresh.zipWithIndex.map((csid, i) =>
        (csid, Alias(next + i))
      )
    }

  // the project's aliases (none if the repo or file isn't there, or can't
  // be read)
  lazy val published_aliases: Target[SortedMap[CSID, Alias]] =
    target((m = source.master(ag.grader.repos.AliasesRepo.name_of(c, p)))) {
      (producer, deps) =>
        Project
          .text_on(producer, deps.m, "aliases.json")
          .flatMap(json =>
            Try(upickle.default.read[SortedMap[CSID, Alias]](json)).toOption
          )
          .getOrElse(SortedMap[CSID, Alias]())
    }

  // ---- the tests -----------------------------------------------------

  // the override repo's tests: the names it has a file (or directory) of
  // for every test extension
  lazy val override_test_names: Target[SortedSet[String]] =
    target(
      (
        ov = source.master(ag.grader.repos.OverrideRepo.name_of(c, p)),
        ext = test_extensions
      )
    ) { (producer, deps) =>
      val found = for {
        o <- deps.ov.toSeq
        n <- producer.list(producer.commit(o).tree).getOrElse(Seq())
        if !n.startsWith(".")
        (base, ext) = split(n)
        if deps.ext.contains(ext)
      } yield (base, ext)
      found
        .groupBy(_._1)
        .filter(_._2.size == deps.ext.size)
        .keySet
        .to(SortedSet)
    }

  // The image the project's tests run in, if it has a docker file: named by
  // the file's contents, which are read on every run (the file is in the
  // grader's directory, not on the mirror). It's built when a test needs it.
  lazy val docker_image: Target[Option[String]] =
    complex_target[Option[String]] { tracker =>
      val image = tracker
        .track(docker_file)
        .map(f => tracker.track(DockerImage(f, source).image))
      tracker.run_if_needed(_ => image)
    }

  // ---- grading -------------------------------------------------------

  // Where the project is relative to its deadlines (as Project.state):
  // worked out again every minute as time passes; what depends on it reruns
  // only when it changes.
  lazy val state: Target[ProjectState] =
    target(
      (ms = milestones, clock = periodic(60 * 1000L))
    ) { (_, deps) => ProjectState.at(LocalDateTime.now().nn, deps.ms) }

  // The results files of the milestones the project publishes now (see
  // ProjectState.publishes), by file name (Milestone.results_file)
  lazy val results_files: Int => Target[
    SortedMap[String, SortedMap[Alias, RedactedStudentResults]]
  ] = fun { (n: Int) =>
    complex_target[
      SortedMap[String, SortedMap[Alias, RedactedStudentResults]]
    ] { tracker =>
      val st = tracker.track(state)
      val all_milestones = st.all_milestones.filter(st.publishes)
      val results =
        tracker.track_all(
          all_milestones.map(milestone(_).results(n))
        )
      tracker.run_if_needed(_ =>
        all_milestones.map(_.results_file).zip(results).to(SortedMap)
      )
    }
  }

  // What a project's report pages show (as r2's HtmlGen.page): its published
  // results, from the mirror, and its settings
  lazy val report_inputs: Target[ReportInputs] =
    complex_target[ReportInputs] { tracker =>
      val st = tracker.track(state)
      val results = tracker.track_all(
        st.all_milestones.map(milestone(_).published_results)
      )
      val deps = tracker.track_all(
        (
          ms = milestones,
          weights = test_weights,
          bad_tests = bad_tests,
          enrollment = course.enrollment,
          aliases = this.aliases,
          course_staff = course.staff,
          project_staff = staff,
          projects = course.active_projects(ag.grader.repos.CoursesConfig.any)
        )
      )
      tracker.run_if_needed { _ =>
        ReportInputs(
          title = name,
          project_name = p.value,
          milestones = deps.ms,
          current = st.current,
          enrollments = deps.enrollment.size,
          results = st.all_milestones
            .zip(results)
            .map((milestone, r) => (milestone, r, st.publishes(milestone))),
          weights = deps.weights,
          bad_tests = deps.bad_tests,
          staff_aliases = HtmlGen.staff_aliases(
            deps.aliases,
            deps.course_staff ++ deps.project_staff
          ),
          tabs = HtmlGen.tabs(c.value, p.value, deps.projects.map(_.value))
        )
      }
    }

  // ---- helpers -------------------------------------------------------

  // a name's base and extension, as os.Path's baseName and ext
  private def split(name: String): (String, String) =
    name.lastIndexOf('.') match {
      case -1 => (name, "")
      case i  => (name.take(i), name.drop(i + 1))
    }

}

object Project {
  // Where `path` in `tree` leads, its links followed (as Project.copy_test's
  // os.followLink: tests share files that way, t1.dir -> t0.dir); None if
  // it leads out of the tree, or round in circles.
  private[grader] def follow(
      ctx: Producer[?],
      tree: Tree,
      path: os.RelPath,
      hops: Int = 40
  ): Option[os.RelPath] =
    ctx.link_target(tree, path) match {
      case None                         => Some(path)
      case Some(_) if hops == 0         => None
      case Some(t) if t.startsWith("/") => None
      case Some(t)                      =>
        val segments = t
          .split('/')
          .filter(s => s.nonEmpty && s != ".")
          .foldLeft(Option(path.segments.dropRight(1).toList)) {
            case (Some(at), "..") => Option.when(at.nonEmpty)(at.dropRight(1))
            case (Some(at), s)    => Some(at :+ s)
            case (None, _)        => None
          }
        segments.flatMap(ss =>
          follow(ctx, tree, os.RelPath(ss.mkString("/")), hops - 1)
        )
    }
  // the tests `names` names, by external or internal name
  private[grader] def named(
      ids: SortedSet[TestId],
      names: SortedSet[String]
  ): SortedSet[TestId] =
    ids.filter(t =>
      names.contains(t.external_name) || names.contains(t.internal_name)
    )

  // a file on master, if the repo and the file are there
  private[grader] def text_on(
      ctx: Context[?],
      m: Option[Commit],
      file: String
  ): Option[String] =
    m.flatMap(c => ctx.read_text(ctx.commit(c).tree, os.RelPath(file)))

  private[grader] def readable[A: upickle.default.Reader](
      json: Option[String],
      name: String
  ): Boolean =
    json.exists(j =>
      Try(NoNulls.check(upickle.default.read[A](j), name)).isSuccess
    )
}
