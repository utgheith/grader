package ag.grader.main.cmd

import ag.grader.main.misc.status_pages

// Serves the status pages (and nothing else) until stopped. It only reads
// the local mirror (see `sync`), in its own workspace, so it can go on
// beside runs, which serve the same pages while they're up.
object Status {
  def apply(workspace: os.Path): Unit =
    ag.grader.Config.web match {
      case None =>
        println(
          "the status pages need config.json's \"web\" (see ag.web.WebConfig)"
        )
      case Some(web) =>
        // its own command output: a run may be using the workspace's
        ag.rules.use_std_out_err(workspace / "status" / "std_out_err")
        status_pages(workspace).foreach { pages =>
          pages.warm(println)
          if (!ag.web.WebServer.serve_pages(workspace, web, pages))
            println(
              s"if a run is serving port ${web.port}, its server has the status pages too, at /status/"
            )
        }
    }
}
