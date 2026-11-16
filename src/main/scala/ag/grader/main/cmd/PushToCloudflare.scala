package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  outbox_of,
  source_of,
  active_courses,
  in_r3_workspace
}

// Records each picked active course's site in the outbox, for `sync` to
// push, and serves it on Cloudflare (see repos.SiteRepo and
// SiteDeployment), if config.json's cloudflare is enabled: again only
// when its pages change. The r3 form of push_to_cloudflare.
object PushToCloudflare {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val host = ag.grader.Config.cloudflare.filter(_.enabled)
      if (host.isEmpty)
        println("cloudflare isn't enabled in config.json: sites not served")
      val ob = outbox_of(commonArgs.workspace)
      val cs = active_courses(s, commonArgs, source)
      for (course <- cs) {
        val todo =
          ob.record(s.track(ag.grader.repos.SiteRepo(course).latest))
        val at =
          host.map(h =>
            s.track(ag.grader.cloudflare.SiteDeployment(course, h).deployed)
          )
        println(
          s"${course.c}: site ${if (todo) "to push" else "up to date"}; " +
            at.fold("not served")(u => s"served at $u")
        )
      }
      println(s"${ob.pending.size} pending in all")
      s.tally.report("push_to_cloudflare", cs.size, commonArgs.verbose.value)
    }
}
