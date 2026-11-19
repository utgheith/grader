package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  source_of,
  active_courses,
  in_r3_workspace
}

// Serves each picked active course's site on Cloudflare (see
// SiteDeployment), if config.json's cloudflare is enabled: again only when
// its pages change. `grade` does this for the courses it grades; this is
// for serving a site by hand. The site is worked out as `grade` would
// publish it (see repos.SiteRepo), without running anything.
object PushToCloudflare {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val host = ag.grader.Config.cloudflare.filter(_.enabled)
      if (host.isEmpty)
        println("cloudflare isn't enabled in config.json: sites not served")
      val cs = active_courses(s, commonArgs, source)
      for (course <- cs) {
        val at =
          host.map(h =>
            s.track(ag.grader.cloudflare.SiteDeployment(course, h).deployed)
          )
        println(
          s"${course.c}: site " + at.fold("not served")(u => s"served at $u")
        )
      }
      s.tally.report("push_to_cloudflare", cs.size, commonArgs.verbose.value)
    }
}
