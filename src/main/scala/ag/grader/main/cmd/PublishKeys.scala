package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  outbox_of,
  source_of,
  dropbox_of,
  in_r3_workspace
}

import ag.grader.repos.CoursesConfig

// Records the dropbox's keys in gitolite-admin in the outbox, for `sync`
// to push (see repos.KeysRepo): every active course's, since they share
// the repo; -c picks the courses whose key counts are shown.
object PublishKeys {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val source = source_of(commonArgs.workspace)
      val dropbox = dropbox_of()
      if (dropbox.root.isEmpty)
        println("no dropbox_path in config.json: keys left alone")
      else {
        for {
          c <- s
            .track(CoursesConfig(source).active_courses(commonArgs.courses_))
            .toSeq
        } {
          println(s"\n------ $c -------")
          println(s.track(dropbox.keys(c)).size)
        }
        val ob = outbox_of(commonArgs.workspace)
        val p = s.track(ag.grader.repos.KeysRepo(source, dropbox).latest)
        val todo = ob.record(p)
        println(
          s"gitolite-admin: ${if (todo) "to push" else "up to date"}; " +
            s"${p.value.notices.size} to tell; ${ob.pending.size} pending in all"
        )
      }
    }
}
