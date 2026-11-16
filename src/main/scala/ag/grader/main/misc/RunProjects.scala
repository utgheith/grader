package ag.grader.main.misc

import ag.grader.Milestone
import ag.grader.targets.Project
import ag.grader.web.HtmlGen

// Picks the active projects to run, from the local mirror (as
// publish_results did: with no -p, one with work to do, picked by
// priority, see Priority; named ones as far as their work goes; --tries:
// all of them, to that many), and works out their results: every milestone
// they publish (its tests and keep-going from the project's state), and
// each student's in the current milestone. The tries are a limit, not a promise: a test
// runs up to that many times, and stops sooner at a failure unless it
// keeps going. Publishes nothing. Says how it went; the projects, each
// with its limit.
//
// With `narrow`, only the students -s picks, the tests -t picks, and the
// milestone `milestone` (if given) are run, and only their outcomes worked out.
private[main] def run_projects(
    s: ag.rules.State,
    commonArgs: CommonArgs,
    tries: Option[Int],
    narrow: Boolean,
    milestone: Option[Milestone]
): Seq[(Project, Int)] = {
  val source = source_of(commonArgs.workspace)
  val candidates = active_projects(s, commonArgs, source)
  def state(project: Project) = s.track(project.state)
  def all_milestones(project: Project): Seq[Milestone] =
    state(project).all_milestones.filter(state(project).publishes)
  def work(project: Project): Option[(ag.grader.Band, Int)] = {
    def has_work(milestone: Milestone)(n: Int) =
      all_milestones(project).contains(milestone) && s.track(
        project.milestone(milestone).has_work(n)
      )
    val (live, milestones) =
      all_milestones(project).partition(_ == Milestone.Live)
    ag.grader.Priority.band(
      state(project).is_live,
      s.track(project.iterations),
      n => milestones.exists(has_work(_)(n)),
      n => live.exists(has_work(_)(n))
    )
  }
  def report(project: Project, w: Option[(ag.grader.Band, Int)]) =
    println(
      s"${project.name}: ${state(project).description}; ${w.fold("up to date")(
          (band, n) => s"work to do (${band.description}, up to $n tries)"
        )}"
    )
  val named = commonArgs.projects_.regex != ".*"
  val projects: Seq[(Project, Int)] =
    tries match {
      case Some(n)                               => candidates.map((_, n))
      case None if named || candidates.size <= 1 =>
        for (project <- candidates) yield {
          val w = work(project)
          report(project, w)
          (project, w.fold(s.track(project.iterations).max)(_._2))
        }
      case None =>
        val busy = for {
          project <- candidates
          w = work(project)
          _ = report(project, w)
          (band, n) <- w
        } yield ((project, n), project.p.value, band)
        val picked = ag.grader.Priority.pick(busy).toSeq
        if (picked.isEmpty) println("nothing to do")
        picked
    }
  for ((project, n) <- projects) {
    val st = state(project)
    val run_milestones = milestone.fold(all_milestones(project))(Seq(_))
    println(
      s"running ${project.name}, up to $n tries: ${st.description}; " +
        run_milestones
          .map(milestone =>
            s"${HtmlGen.milestone_label(milestone)}: ${st.tests(milestone).description}" +
              (if (st.keeps_going(milestone)) ", keep-going" else "")
          )
          .mkString("; ")
    )
    val started = ag.grader.targets.runs_started()
    if (narrow) {
      val f = s.tally
      val outs = s.track_all(
        for {
          milestone <- run_milestones
          csid <- project.students_with_submission.tally(s).toSeq
          if f.keep("matching -s", commonArgs.students_.matches(csid.value))
          graded = project.submission(csid).milestone(milestone)
          // a commit to grade for the milestone
          _ <- graded.prepare_info.tally(s).toSeq
          id <- project
            .milestone(milestone)
            .ids_for(st.tests(milestone))
            .tally(s)
            .toSeq
          if f.keep(
            "matching -t",
            commonArgs.tests_.matches(id.external_name) ||
              commonArgs.tests_.matches(id.internal_name)
          )
        } yield graded.run_one(st.keeps_going(milestone), n)(id)
      )
      val results = outs.map(_.value)
      println(
        s"${project.name}: ${(ag.grader.targets.runs_started() - started).toString} runs; " +
          s"${results.count(_.is_happy)} of ${results.size} test results passed"
      )
      f.report("run", results.size, commonArgs.verbose.value)
    } else {
      val students = s.track(project.students_with_submission).toSeq
      val files = s.track(project.results_files(n))
      val current = s.track_all(
        students.map(csid =>
          project.submission(csid).milestone(st.current).results(n)
        )
      )
      val outcomes = current.flatten.flatMap(_.outcomes.values)
      println(
        s"${project.name}: ${(ag.grader.targets.runs_started() - started).toString} runs; " +
          s"${files.keys.mkString(", ")} for ${students.size} students; ${HtmlGen.milestone_label(st.current)}: " +
          s"${outcomes.count(_.is_happy)} of ${outcomes.size} test results passed"
      )
    }
  }
  projects
}
