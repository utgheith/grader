package ag.grader.main.misc

import ag.grader.{Milestone, Priority, Round}
import ag.grader.targets.Project
import ag.grader.web.HtmlGen

// Picks the active projects to run, from the local mirror, and runs them.
// With no -p, one round (see Round) of one project: the most urgent of
// their next rounds (see Priority), so a run is time-boxed and the loop
// around it does the rest; named ones (or, with `narrow`, all: -s or -t
// asks for that student's or test's work wherever it is), each its own
// next round; with --tries, all of them, every milestone they publish, to
// that many. Each milestone's tests and keep-going come from the project's
// state. The tries are a limit, not a promise: a test runs up to that many
// times, and stops sooner at a failure unless it keeps going. Publishes
// nothing. Says how it went; the projects run.
//
// With `narrow`, only the students -s picks, the tests -t picks, and the
// milestone `milestone` (if given, else every one published) are run, to
// the next round's tries (or the most a test gets), and only their
// outcomes worked out.
private[main] def run_projects(
    s: ag.rules.State,
    commonArgs: CommonArgs,
    tries: Option[Int],
    narrow: Boolean,
    milestone: Option[Milestone]
): Seq[Project] = {
  val source = source_of(commonArgs.workspace)
  val candidates = active_projects(s, commonArgs, source)
  def state(project: Project) = s.track(project.state)
  def all_milestones(project: Project): Seq[Milestone] =
    state(project).all_milestones.filter(state(project).publishes)
  def max(project: Project) = s.track(project.iterations).max
  def next(project: Project): Option[Round] = {
    val r = Priority.round(
      state(project),
      max(project),
      (m, n) => s.track(project.milestone(m).has_work(n))
    )
    println(
      s"${project.name}: ${state(project).description}; ${r
          .fold("up to date")(r => s"next: ${r.description}")}"
    )
    r
  }
  val named = commonArgs.projects_.regex != ".*" || narrow
  // each with the milestones to run and their tries
  val projects: Seq[(Project, Seq[Milestone], Int)] =
    tries match {
      case Some(n)        => candidates.map(p => (p, all_milestones(p), n))
      case None if narrow =>
        for (project <- candidates)
          yield (
            project,
            milestone.fold(all_milestones(project))(Seq(_)),
            next(project).fold(max(project))(_.tries)
          )
      case None if named || candidates.size <= 1 =>
        for (project <- candidates) yield {
          val r = next(project)
          (project, r.fold(Seq())(_.milestones), r.fold(0)(_.tries))
        }
      case None =>
        val busy = for {
          project <- candidates
          r <- next(project)
        } yield ((project, r.milestones, r.tries), project.p.value, r)
        val picked = Priority.pick(busy).toSeq
        if (picked.isEmpty) println("nothing to do")
        picked
    }
  for ((project, run_milestones, n) <- projects if run_milestones.nonEmpty) {
    val st = state(project)
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
        } yield graded.run_one(st.keeps_going(milestone), n, id)
      )
      val results = outs.map(_.value)
      println(
        s"${project.name}: ${(ag.grader.targets.runs_started() - started).toString} runs; " +
          s"${results.count(_.is_happy)} of ${results.size} test results passed"
      )
      f.report("run", results.size, commonArgs.verbose.value)
    } else {
      val students = s.track(project.students_with_submission).toSeq
      val results = s.track_all(
        for {
          milestone <- run_milestones
          csid <- students
        } yield project.submission(csid).milestone(milestone).results(n)
      )
      val outcomes = results.flatten.flatMap(_.outcomes.values)
      println(
        s"${project.name}: ${(ag.grader.targets.runs_started() - started).toString} runs; " +
          s"${run_milestones.map(HtmlGen.milestone_label).mkString(", ")} for ${students.size} students: " +
          s"${outcomes.count(_.is_happy)} of ${outcomes.size} test results passed"
      )
    }
  }
  projects.map(_._1)
}
