package ag.grader.main.cmd

import ag.grader.main.misc.outbox_of

// Pushes the outbox to the git server (see ag.grader.Outbox), then brings
// the local mirror up to date. Needs config.json's can_push_repo.
object Sync {
  def apply(workspace: os.Path): Unit =
    if (!ag.grader.Config.pushes_allowed)
      println("config.json doesn't allow pushing (can_push_repo)")
    else {
      val ob = outbox_of(workspace)
      println(s"${ob.pending.size} repos to push")
      ob.sync(
        ag.grader.Config.gitolite_server,
        ag.grader.Config.mail_allowed,
        println
      )
      println(s"${ob.pending.size} still pending")
    }
}
