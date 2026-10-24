package ag.events

// Runs the event watchers against a local copy of the live server's repos
// (~/grader-live-copy/repositories, built from a grader workspace's mirror),
// in the gitolite test container -- never against the live server. Prints
// counts and timings only: repo names and their contents identify students.
//
//   sbt "Test/runMain ag.events.LiveCopyExperiment [course]"
object LiveCopyExperiment {
  def main(args: Array[String]): Unit = {
    val course = args.headOption.getOrElse("cs439c_f26")
    val seed = os.home / "grader-live-copy"
    val conf =
      """@me = admin
        |repo @all
        |    RW+ = @me
        |    C = @me
        |repo grader
        |    R = @me
        |""".stripMargin

    def timed[A](what: String)(f: => A): A = {
      val start = System.nanoTime()
      val a = f
      println(f"  $what: ${(System.nanoTime() - start) / 1e9}%.1fs")
      a
    }

    println(s"starting a server with the copy of ${seed.toString}")
    val server = timed("start")(GitoliteServer(conf, Nil, seed = Some(seed)))
    try {
      val observed = timed("observe")(
        Gitolite.observe(server.server, server.port, server.ssh("admin"))
      )
      println(
        s"  ${observed.size} repos, ${observed.count(_._2.isDefined)} with history"
      )

      // the enrollment repo stands in for the dropbox: CSID -> public key
      val enrollment_dir = os.temp.dir(deleteOnExit = true)
      val _ = os
        .proc(
          "git",
          "clone",
          "-q",
          server.url(s"${course}__enrollment"),
          enrollment_dir
        )
        .call(env = server.git_env("admin"))
      val enrolled = upickle.default.read[Map[String, String]](
        os.read(enrollment_dir / "enrollment.json")
      )
      println(s"  ${enrolled.size} enrolled")

      val dir = os.temp.dir(deleteOnExit = true)
      val log = GitEventLog(
        server.url(s"${course}__private"),
        dir / "private",
        PrivateLog.kinds,
        env = server.git_env("admin")
      )
      given KeyStore = KeyStore(dir / "keys.json")
      val view = CourseState.view(log, Some(dir / "view.json"))

      for (round <- 1 to 2) {
        println(s"round $round")
        val new_keys = timed("dropbox (enrollment)")(
          DropboxWatcher(view).poll(enrolled)
        )
        val (changes, unknown) =
          timed("git")(GitWatcher(course, view).poll(observed))
        println(
          s"  ${new_keys.count(_.event.isInstanceOf[KeySubmitted])} key(s), ${changes.size} repo change(s), $unknown repo(s) of unknown students"
        )
      }
      println(s"private log: ${log.end} events")
    } finally server.close()
  }
}
