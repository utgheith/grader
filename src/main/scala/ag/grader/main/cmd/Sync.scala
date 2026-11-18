package ag.grader.main.cmd

import ag.grader.main.misc.{mirror_root, outbox_of}

// Brings the local mirror of the git server up to date (see
// ag.grader.gitolite.LocalMirror), first pushing the outbox to the server
// (see ag.grader.Outbox) if config.json allows it (can_push_repo).
object Sync {
  def apply(workspace: os.Path): Unit = {
    val server = ag.grader.Config.gitolite_server
    val ob = outbox_of(workspace)
    if (ag.grader.Config.pushes_allowed) {
      println(s"${ob.pending.size} repos to push")
      ob.sync(server, ag.grader.Config.mail_allowed, println)
      println(s"${ob.pending.size} still pending")
    } else {
      println(
        s"not pushing (config.json's can_push_repo): ${ob.pending.size} repos pending"
      )
      ag.grader.gitolite.LocalMirror
        .update(server, mirror_root(workspace), println)
    }
  }
}
