package ag.grader.main.cmd

import ag.grader.main.misc.mirror_root

// Brings the local mirror of the git server up to date (see
// ag.grader.gitolite.LocalMirror): run it now and then, e.g. from a timer.
object Mirror {
  def apply(workspace: os.Path): Unit =
    ag.grader.gitolite.LocalMirror.update(
      ag.grader.Config.gitolite_server,
      mirror_root(workspace),
      println
    )
}
