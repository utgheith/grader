package ag.grader.main.cmd

import ag.grader.main.misc.{
  CommonArgs,
  outbox_of,
  source_of,
  dropbox_of,
  in_r3_workspace
}

import ag.grader.repos.CoursesConfig

// Works out, from the local mirror, what the repos the arguments pick
// should hold (ag.grader.repos), and records what differs in the
// outbox, for `sync` to push: the students' keys and each active course's
// enrollment (from the dropbox), grades repo and site; each project's
// override, aliases, submitted tests and tests repos; and each enrolled
// student's work repo for each active project. The results repos are
// `grade`'s. Run it again after a sync: what's read from the mirror (the
// tests from the override repo, the students from the enrollment) takes
// a round to get there.
object Outbox {
  def apply(commonArgs: CommonArgs): Unit =
    in_r3_workspace(commonArgs) { s =>
      val course_re = commonArgs.courses_
      val project_re = commonArgs.projects_
      val student_re = commonArgs.students_
      val source = source_of(commonArgs.workspace)
      val dropbox = dropbox_of()
      val config = CoursesConfig(source)
      val picked = for {
        c <- s.track(config.course_names).toSeq
        if course_re.matches(c.value)
        course = config.course(c)
        p <- s.track(course.project_names).toSeq
        if project_re.matches(p.value)
      } yield course.project(p)
      val courses =
        s.track(config.active_courses(course_re)).toSeq.map(config.course)
      // with no dropbox, the keys and enrollments are left alone: they're
      // written from it, and an empty one would unenroll everyone
      val has_dropbox = dropbox.root.isDefined
      if (!has_dropbox)
        println(
          "no dropbox_path in config.json: keys and enrollments left alone"
        )
      // each enrolled student's repo for each active project
      val work_repos = for {
        course <- courses
        p <- s.track(course.active_projects(project_re)).toSeq
        csid <- s.track(course.enrolled(student_re)).toSeq
      } yield ag.grader.repos
        .WorkRepo(course.project(p).submission(csid))
        .latest
      val wanted = s.track_all(
        (if (has_dropbox)
           ag.grader.repos.KeysRepo(source, dropbox).latest +: courses.map(
             ag.grader.repos.EnrollmentRepo(_, dropbox).latest
           )
         else Seq()) ++
          courses.flatMap(course =>
            Seq(
              ag.grader.repos.GradesRepo(course).latest,
              ag.grader.repos.SiteRepo(course).latest
            )
          ) ++
          picked.flatMap(ag.grader.repos.Grade.project) ++
          work_repos
      )
      val ob = outbox_of(commonArgs.workspace)
      val n = wanted.count(ob.record)
      println(
        s"${courses.size} courses, ${picked.size} projects, ${work_repos.size} work repos; " +
          s"${wanted.size} repos, $n to push; ${ob.pending.size} pending in all"
      )
    }
}
