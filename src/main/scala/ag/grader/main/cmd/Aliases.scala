package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  AliasSortMode,
  source_of,
  active_projects,
  in_r3_workspace
}

// Each picked enrolled student's alias in each picked active project, as
// `grade` would publish it (marked if it isn't published yet; ? if they
// have none: no submission yet). -s picks students by CSID; -t picks them
// by their own test, named by alias outside and CSID inside (as run's -t),
// so a student with no alias has no test for it to pick.
object Aliases {
  def apply(commonArgs: CommonArgs, sortMode: AliasSortMode): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val tests_re = commonArgs.tests_
      val f = s.tally
      var shown = 0
      for (project <- active_projects(s, commonArgs, source)) {
        val aliases = project.aliases.tally(s)
        val published = s.track(project.published_aliases)
        val csids = project.course
          .enrolled(commonArgs.students_)
          .tally(s)
          .toSeq
          .filter(csid =>
            f.keep(
              "matching -t",
              tests_re.regex == ".*" || aliases
                .get(csid)
                .exists(a =>
                  tests_re.matches(a.toString) || tests_re.matches(csid.value)
                )
            )
          )
        val sorted = sortMode match {
          case AliasSortMode.CSID  => csids
          case AliasSortMode.Alias =>
            csids.sortBy(aliases.get(_).fold(Int.MaxValue)(_.value))
        }
        val base_name = project.name
        val width =
          sorted
            .map(_.value.length)
            .maxOption
            .getOrElse(0) + base_name.length + 1
        println(s"\n$base_name:")
        for (csid <- sorted) {
          val name =
            String.format(s"%-${width}s", s"${base_name}_${csid.value}")
          val alias = aliases.get(csid) match {
            case None                                       => "?"
            case Some(a) if published.get(csid).contains(a) => a.toString
            case Some(a) => s"${a.toString} (not published yet)"
          }
          println(s"$name  $alias")
        }
        shown += sorted.size
      }
      f.report("aliases", shown, commonArgs.verbose.value)
    }
}
