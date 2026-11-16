package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_projects,
  in_r3_workspace
}

import ag.grader.Milestone
import scala.collection.SortedMap
import scala.util.matching.Regex

// Each student's score for `milestone`: the weights of the milestone's tests (Live:
// every milestone's) they passed at least once in `tries` tries, halved if
// late (as r2's compute_scores, for one round), from the local mirror
object Scores {
  def apply(
      commonArgs: CommonArgs,
      tries: Int,
      milestone: Option[Milestone]
  ): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      for (project <- active_projects(s, commonArgs, source)) {
        val chosen = milestone.getOrElse(s.track(project.state).current)
        val weights = s.track(project.test_weights)
        val weight = (for {
          t <- s
            .track(project.milestone(chosen).test_ids)
            .toSeq
          w <- weights
          if Regex(w.pattern).matches(t.external_name)
        } yield (t, w.weight)).to(SortedMap)
        val total = weight.values.sum
        println(s"${project.name} after 1 runs, total $total:")
        for (
          (csid, passed) <- s.track(
            project.milestone(chosen).scores(tries)
          )
        ) {
          val raw = passed.toSeq.map(weight).sum
          val late = s.track(
            project.submission(csid).milestone(chosen).is_late
          )
          val score = if (late) raw.toDouble / 2.0 else raw.toDouble
          println(
            f"    ${csid.toString} ${score.toString} ${100.0 * (score / total.toDouble)}%.2f ${
                if (late) "late" else "ontime"
              }"
          )
        }
      }
    }
}
