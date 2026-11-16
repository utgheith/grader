package ag.grader.main.cmd

import ag.grader.main.misc.{CommonArgs, mirror_root, in_r3_workspace}

// every repo's master on the local mirror, as of its last sync: "repo
// sha", as the server's `history`
object History {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      for (
        (repo, id) <- s.track(
          ag.rules.Source(mirror_root(commonArgs.workspace)).masters
        )
      )
        println(s"$repo $id")
    }
}
