package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  publishing,
  source_of,
  dropbox_of,
  in_r3_workspace
}

import ag.grader.repos.CoursesConfig

// Publishes the dropbox's keys in gitolite-admin (see Publishing) (see repos.KeysRepo): every active course's, since they share
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
        publishing(commonArgs.workspace) { pub =>
          val p = s.track(ag.grader.repos.KeysRepo(source, dropbox).latest)
          val todo = pub.record(p)
          println(
            s"gitolite-admin: ${if (todo) "to push" else "up to date"}; " +
              s"${p.value.notices.size} to tell"
          )
        }
      }
    }
}
