package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_projects,
  in_r3_workspace
}

import ag.grader.Milestone
import ag.grader.web.HtmlGen

// What `grade` would run for each picked active project in a milestone, as
// things stand: the test set and keep-going the state and milestone give (and
// the set shown, if it's another), and the tests themselves, those -t
// picks (by external or internal name, as run's -t).
object MilestoneTests {
  def apply(commonArgs: CommonArgs, milestone: Option[Milestone]): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val tests_re = commonArgs.tests_
      val f = s.tally
      var found = 0
      for (project <- active_projects(s, commonArgs, source)) {
        val state = s.track(project.state)
        val chosen = milestone.getOrElse(state.current)
        val tests = state.tests(chosen)
        val shown = state.shows(chosen)
        val ids = project
          .milestone(chosen)
          .ids_for(tests)
          .tally(s)
          .toSeq
          .filter(id =>
            f.keep(
              "matching -t",
              tests_re.matches(id.external_name) ||
                tests_re.matches(id.internal_name)
            )
          )
        println(
          s"\n${project.name} (${HtmlGen.milestone_label(chosen)}): ${state.description}; " +
            (if (state.publishes(chosen)) "published"
             else "not published yet") +
            s"; ${tests.description} tests (${ids.size})" +
            (if (shown != tests) s", shown: ${shown.description}" else "") +
            s", keep-going ${if (state.keeps_going(chosen)) "on" else "off"}"
        )
        for (id <- ids) println(s"  ${id.external_name}")
        found += ids.size
      }
      f.report("milestone_tests", found, commonArgs.verbose.value)
    }
}
