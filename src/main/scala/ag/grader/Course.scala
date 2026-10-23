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
  say,
  update_data
}

import ag.task.Task

import scala.collection.{SortedMap, SortedSet, mutable}
import upickle.default.ReadWriter

case class Course(course_name: String) extends Scope(ToRelPath(course_name))
    derives ReadWriter {

  lazy val staff_group_name: String = s"@${course_name}_staff"

  private lazy val raw: Target[RawCourse] = Gitolite.raw_course(course_name)

  lazy val notifications: Target[NotificationConfig] =
    target(raw)(_.notifications)

  lazy val projects: Target[SortedMap[String, Project]] =
    target(raw) { rc =>
      val s = for {
        pn <- rc.projects.keySet.toSeq
      } yield (pn, Project(this, pn))
      SortedMap(s*)
    }

  lazy val active: Target[Boolean] = target(raw) { rc =>
    rc.active
  }

  lazy val staff: Target[SortedSet[CSID]] = target(raw) { rc => rc.staff }

  lazy val active_projects: Target[SortedMap[String, Project]] =
    complex_target {
      val ps = projects.guilty
      val active_flags = for {
        active_flags <- ps.values.map(_.active.guilty)
      } yield active_flags
      run_if_needed {
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
    target(periodic(60000), Config.dropbox_path) {
      case (_, Some(dropbox)) =>
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
      case (_, None) =>
        SortedMap()
    }

  /*
   * Update keys in gitolite-admin
   *
   * Key updates are automatic (when a student drops a new key in the dropbox)
   * Key deletion is manual and need to be done in both places
   */

  lazy val publish_keys: Target[WithData[SortedMap[CSID, String]]] =
    target(
      Gitolite.repo_info(
        "gitolite-admin"
      ),
      dropbox,
      notifications.peek,
      Config.can_send_mail,
      Config.can_push_repo
    ) { (g, dropbox, notifications, can_send_mail, can_push_repo) =>
      create_data(skip = _.lastOpt.contains(".git")) { dir =>
        val added = g.update(
          path = dir,
          fork_from = None,
          msg = "updated keys",
          readers = Seq(),
          writers = Seq(),
          can_push_repo
        ) { _ =>
          val added = mutable.SortedSet[CSID]()

          for {
            // all keys in dropbox
            (csid, new_key) <- dropbox.toSeq
            key_file_name = s"${csid.toString}.pub"
            key_file_path = dir / "keydir" / key_file_name
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
            added.add(csid)
          }
          added
        }

        for (csid <- added) {
          notifications.send_key_update(this, csid, g.server, can_send_mail)
        }

        dropbox
      }

    }

  private lazy val enrollment_repo_name = s"${course_name}__enrollment"
  private def enrollment_file(dir: os.Path): os.Path = dir / "enrollment.json"

  // read the enrollment repo

  lazy val enrollment: Target[SortedMap[CSID, String]] =
    target(Gitolite.mirror(enrollment_repo_name)) { sp =>
      if (sp.value) {
        val f = enrollment_file(sp.get_data_path)
        if (os.isFile(f)) {
          upickle.default.read[SortedMap[CSID, String]](
            os.read(enrollment_file(sp.get_data_path))
          )
        } else {
          say(s"enrollment file ${f.toString} doesn't exist")
          SortedMap()
        }
      } else {
        SortedMap[CSID, String]()
      }
    }

  lazy val enrolled_csids: Target[SortedSet[CSID]] = target(enrollment) {
    enrollment => enrollment.keySet
  }

  // create/update the enrollment repo
  lazy val publish_enrollment: Target[WithData[SortedMap[CSID, String]]] =
    target(
      Gitolite.repo_info(
        enrollment_repo_name
      ),
      publish_keys,
      Config.can_push_repo
    ) { (g, keys, can_push_repo) =>
      update_data[SortedMap[CSID, String]](skip = {
        _.lastOpt.contains(".git")
      }) { dir =>
        g.update(
          path = dir,
          fork_from = Some("empty"),
          msg = "enrolling students",
          readers = Seq(staff_group_name),
          writers = Seq(),
          can_push_repo
        ) { _ =>
          os.write.over(
            enrollment_file(dir),
            upickle.default.write(keys.value, indent = 2)
          )
        }
        keys.value
      }
    }

  lazy val site_repo_name: String = s"${course_name}__site"

  // The report pages of all active projects, kept in a git repo so the
  // site is under source control: each change is a commit, and history is
  // preserved. Pages of projects that are no longer active stay in the repo.
  lazy val publish_site: Target[WithData[SortedSet[String]]] =
    complex_target {
      val repo_info = Gitolite.repo_info(site_repo_name).track
      val pages = active_projects.guilty.values.toSeq.map { p =>
        val g = HtmlGen(p)
        (g.page_name, g.page.guilty)
      }
      val can_push_repo = Config.can_push_repo.track

      run_if_needed {
        val repo_info_v = repo_info.block
        update_data(_.lastOpt.contains(".git")) { dir =>
          repo_info_v.update(
            path = dir,
            fork_from = Some("empty"),
            msg = "update site",
            readers = Seq("@all"),
            writers = Seq(),
            can_push_repo.block
          ) { _ =>
            for ((name, page) <- pages) {
              os.copy.over(page.get_data_path / name, dir / name)
            }
            // the front page lists every page in the repo, including those
            // of projects that are no longer active
            val all_pages = os
              .list(dir)
              .map(_.last)
              .filter(f => f.endsWith(".html") && f != "index.html")
            os.write.over(
              dir / "index.html",
              HtmlGen.render_index(course_name, all_pages)
            )
            pages.map(_._1).to(SortedSet)
          }
        }
      }
    }

  // Each course is its own Cloudflare Pages project: a deploy replaces a
  // project's whole site, so courses sharing one would erase each other.
  // Project names allow only lowercase letters, digits and dashes.
  lazy val cloudflare_project: String =
    course_name.toLowerCase.nn.map(c => if (c.isLetterOrDigit) c else '-')

  // Deploys the site repo's pages whenever they change (or cloudflare's
  // settings do), and serves them at <cloudflare_project>.<domain> when a
  // domain is configured. The data directory holds exactly what was
  // deployed; the value is the site's address, or None if not deployed.
  lazy val push_to_cloudflare: Target[WithData[Option[String]]] =
    target(
      publish_site,
      Config.cloudflare_enabled,
      Config.cloudflare_domain
    ) { (site, enabled, domain) =>
      create_data(_ => false) { dir =>
        // the deployed files: the repo's, minus .git and other dot-files;
        // wrangler runs from `dir`, so its own .wrangler cache stays out
        val site_dir = dir / "site"
        os.makeDir.all(site_dir)
        for {
          f <- os.list(site.get_data_path)
          if !f.last.startsWith(".")
        } os.copy.into(f, site_dir)

        Config.cloudflare.filter(_ => enabled) match {
          case Some(cf) =>
            say(s"deploying ${site_repo_name} to $cloudflare_project")
            cf.deploy(site_dir, cloudflare_project)
            val host = domain match {
              case Some(zone) =>
                val host = s"$cloudflare_project.$zone"
                cf.ensure_domain(cloudflare_project, host, zone)
                host
              case None => cf.pages_host(cloudflare_project)
            }
            Some(s"https://$host")
          case None =>
            say("cloudflare is not enabled in config.json, not deploying")
            None
        }
      }
    }

  private lazy val grades_repo_name = s"${course_name}__grades"

  lazy val create_grades_repo: Target[WithData[Unit]] =
    target(
      Gitolite.repo_info(grades_repo_name),
      Config.can_push_repo
    ) { (g, can_push_repo) =>
      create_data(skip = _.lastOpt.contains(".git")) { dir =>
        g.update(
          path = dir,
          fork_from = Some("empty"),
          msg = "creating grades repo",
          readers = Seq(staff_group_name),
          writers = Seq(staff_group_name),
          can_push_repo
        ) { newly_created =>
          if newly_created then {
            os.write(
              dir / "README.md",
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
    }
}

object Course extends Scope(os.RelPath(".")) {
  given Ordering[Course] = Ordering.by(_.course_name)

  lazy val all: Target[Seq[Course]] = target(Gitolite.course_names) {
    course_names =>
      course_names.toSeq.map(name => Course(name))
  }

  lazy val active_courses: Target[Seq[Course]] = complex_target {
    val all_courses: Seq[Course] = all.guilty
    val active_flag_tasks = all_courses.map(_.active.track)

    run_if_needed {
      val active_flags = Task.sequence(active_flag_tasks).block
      all_courses.zip(active_flags).filter(_._2).map(_._1)
    }
  }

}
