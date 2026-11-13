package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r2.{
  Scope,
  Target,
  ToRelPath,
  WithData,
  create_data,
  periodic,
  run_if_needed,
  say
}

import scala.collection.{SortedMap, SortedSet}
import upickle.default.ReadWriter

@upickle.implicits.allowUnknownKeys(false)
case class Course(course_name: String)
    extends GitoliteScope(ToRelPath(course_name)) derives ReadWriter {

  lazy val staff_group_name: String = s"@${course_name}_staff"

  private lazy val raw: Target[RawCourse] = Gitolite.raw_course(course_name)

  lazy val notifications: Target[NotificationConfig] =
    target((c = raw))((_, deps) => deps.c.notifications)

  lazy val projects: Target[SortedMap[String, Project]] =
    target((rc = raw)) { (_, deps) =>
      val s = for {
        pn <- deps.rc.projects.keySet.toSeq
      } yield (pn, Project(this, pn))
      SortedMap(s*)
    }

  lazy val active: Target[Boolean] = target((rc = raw)) { (_, deps) =>
    deps.rc.active
  }

  lazy val staff: Target[SortedSet[CSID]] = target((rc = raw)) { (_, deps) =>
    deps.rc.staff
  }

  lazy val active_projects: Target[SortedMap[String, Project]] =
    complex_target { tracker =>
      val ps = tracker.track(projects)
      val active_flags = tracker.track_all(ps.values.toSeq.map(_.active))
      run_if_needed(tracker) { _ =>
        ps.values
          .zip(active_flags)
          .filter(_._2)
          .map(p => (p._1.project_name, p._1))
          .to(SortedMap)
      }
    }

  def project(project_name: String): Project =
    Project(this, project_name)

  // Get a sorted list of csids in the dropbox
  lazy val dropbox: Target[SortedMap[CSID, String]] =
    target((periodic = periodic(60000), dropbox_path = Config.dropbox_path)) {
      case (_, (_, Some(dropbox))) =>
        val base = os.home / dropbox / course_name
        val pairs = for {
          f <- os.list(base)
          if os.isFile(f)
          ext = f.ext
          if ext == "pub"
          id = f.baseName
        } yield {
          val owner = os.owner(f).getName
          if (owner != id) {
            throw Exception(s"owner:$owner != csid:$id")
          }
          (CSID(id), os.read(f).trim.nn)
        }
        pairs.to(SortedMap)
      case (_, (_, None)) =>
        SortedMap()
    }

  /*
   * Update keys in gitolite-admin
   *
   * Key updates are automatic (when a student drops a new key in the dropbox)
   * Key deletion is manual and need to be done in both places
   */

  lazy val publish_keys: Target[WithData[SortedMap[CSID, String]]] =
    gitolite_target(
      repo = "gitolite-admin",
      msg = "updated keys",
      fork_from = None
    )(
      (
        dropbox = dropbox,
        notifications = notifications.peek,
        can_send_mail = Config.can_send_mail
      )
    ) { (_, deps, repo) =>
      for {
        // all keys in dropbox
        (csid, new_key) <- deps.dropbox.toSeq
        key_file_name = s"${csid.toString}.pub"
        key_file_path = repo.dir / "keydir" / key_file_name
        // if the key doesn't exist or is different
        if !os
          .exists(key_file_path) || os
          .read(key_file_path)
          .trim
          .nn != new_key
      } {
        // If we make it here then the key needs to be added or updated
        println(s"adding ${csid.toString} to gitolite-admin")
        os.write.over(key_file_path, new_key)
        repo.after_push {
          deps.notifications.send_key_update(
            this,
            csid,
            repo.info.server,
            deps.can_send_mail
          )
        }
      }

      deps.dropbox
    }

  private lazy val enrollment_repo_name = s"${course_name}__enrollment"
  private def enrollment_file(dir: os.Path): os.Path = dir / "enrollment.json"

  // read the enrollment repo

  lazy val enrollment: Target[SortedMap[CSID, String]] =
    target((sp = Gitolite.mirror(enrollment_repo_name))) { (producer, deps) =>
      if (deps.sp.value) {
        val f = enrollment_file(deps.sp.get_data_path(producer))
        if (os.isFile(f)) {
          upickle.default.read[SortedMap[CSID, String]](
            os.read(enrollment_file(deps.sp.get_data_path(producer)))
          )
        } else {
          say(producer, s"enrollment file ${f.toString} doesn't exist")
          SortedMap()
        }
      } else {
        SortedMap[CSID, String]()
      }
    }

  lazy val enrolled_csids: Target[SortedSet[CSID]] =
    target((enrollment = enrollment)) { (_, deps) =>
      deps.enrollment.keySet
    }

  // create/update the enrollment repo
  lazy val publish_enrollment: Target[WithData[SortedMap[CSID, String]]] =
    gitolite_target(
      repo = enrollment_repo_name,
      msg = "enrolling students",
      readers = Seq(staff_group_name)
    )((keys = publish_keys)) { (_, deps, repo) =>
      os.write.over(
        enrollment_file(repo.dir),
        upickle.default.write(deps.keys.value, indent = 2)
      )
      deps.keys.value
    }

  lazy val site_repo_name: String = s"${course_name}__site"

  // The report pages of all active projects, kept in a git repo so the
  // site is under source control: each change is a commit, and history is
  // preserved. Pages of projects that are no longer active stay in the repo.
  lazy val publish_site: Target[WithData[SortedSet[String]]] =
    complex_gitolite_target(
      repo = site_repo_name,
      msg = "update site",
      readers = Seq("@all")
    ) { tracker =>
      val projects = tracker.track(active_projects).values.toSeq
      val pages = tracker.track_all(projects.map(HtmlGen(_).page))

      (producer, repo) => {
        val names = for {
          page <- pages
          f <- os.list(page.get_data_path(producer))
        } yield {
          os.copy.over(f, repo.dir / f.last)
          f.last
        }
        // the front page lists every project's (latest) page in the repo,
        // including those of projects that are no longer active
        val all_pages = os
          .list(repo.dir)
          .map(_.last)
          .filter(f =>
            f.endsWith(".html") && f != "index.html" && !HtmlGen
              .is_phase_page(f)
          )
        os.write.over(
          repo.dir / "index.html",
          HtmlGen.render_index(course_name, all_pages)
        )
        names.to(SortedSet)
      }
    }

  // Deploys the site repo's pages whenever they change (or cloudflare's
  // settings do), and serves them at <project>.<domain> (see
  // CloudflareConfig.project_of) when a
  // domain is configured. The data directory holds exactly what was
  // deployed; the value is the site's address, or None if not deployed.
  lazy val push_to_cloudflare: Target[WithData[Option[String]]] =
    target(
      (
        site = publish_site,
        enabled = Config.cloudflare_enabled,
        domain = Config.cloudflare_domain,
        prefix = Config.cloudflare_project_prefix
      )
    ) { (producer, deps) =>
      create_data(producer)(_ => false) { dir =>
        // the deployed files: the repo's, minus .git and other dot-files;
        // wrangler runs from `dir`, so its own .wrangler cache stays out
        val site_dir = dir / "site"
        os.makeDir.all(site_dir)
        for {
          f <- os.list(deps.site.get_data_path(producer))
          if !f.last.startsWith(".")
        } os.copy.into(f, site_dir)

        Config.cloudflare.filter(_ => deps.enabled) match {
          case Some(cf) =>
            say(
              producer,
              s"deploying ${site_repo_name} to ${cf.project_of(course_name)}"
            )
            Some(cf.publish(site_dir, course_name))
          case None =>
            say(
              producer,
              "cloudflare is not enabled in config.json, not deploying"
            )
            None
        }
      }
    }

  private lazy val grades_repo_name = s"${course_name}__grades"

  lazy val create_grades_repo: Target[WithData[Unit]] =
    gitolite_target(
      repo = grades_repo_name,
      msg = "creating grades repo",
      readers = Seq(staff_group_name),
      writers = Seq(staff_group_name)
    )(EmptyTuple) { (_, _, repo) =>
      if repo.forked then {
        os.write(
          repo.dir / "README.md",
          f"""
                 |# Grading data for `$course_name`
                 |
                 |This is a per-course workspace for synchronizing grading data
                 |used by the grading scripts between TAs. The grader itself
                 |automatically creates this repo, but does not otherwise use
                 |it; that may change in the future.
                 |""".stripMargin
        )
      }
    }
}

object Course extends Scope(os.RelPath(".")) {
  given Ordering[Course] = Ordering.by(_.course_name)

  lazy val all: Target[Seq[Course]] =
    target((course_names = Gitolite.course_names)) { (_, deps) =>
      deps.course_names.toSeq.map(name => Course(name))
    }

  lazy val active_courses: Target[Seq[Course]] = complex_target { tracker =>
    val all_courses: Seq[Course] = tracker.track(all)
    val active_flags = tracker.track_all(all_courses.map(_.active))

    run_if_needed(tracker) { _ =>
      all_courses.zip(active_flags).filter(_._2).map(_._1)
    }
  }

}
