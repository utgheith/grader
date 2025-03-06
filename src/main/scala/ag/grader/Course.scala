package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r2.{
  eval,
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

import scala.collection.{SortedMap, SortedSet, mutable}
import upickle.default.ReadWriter

import scala.concurrent.Future

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
      val ps = projects.track
      val active_flags = for {
        projects <- ps
        active_flags <- Future.sequence(projects.values.map(_.active.track))
      } yield active_flags
      eval(ps, active_flags) { (projects, active_flags) =>
        projects.values
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
            key_file_name = s"$csid.pub"
            key_file_path = dir / "keydir" / key_file_name
            // if the key doesn't exist or is different
            if !os
              .exists(key_file_path) || os
              .read(key_file_path)
              .trim
              .nn != new_key
          } {
            // If we make it here then the key needs to be added or updated
            println(s"adding $csid to gitolite-admin")
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
          say(s"enrollment file $f doesn't exist")
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
            upickle.default.write(keys, indent = 2)
          )
        }
        keys.value
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
    val all_future: Future[Seq[Course]] = all.track
    val active_flags_future: Future[Seq[Boolean]] =
      all_future.flatMap(all => Future.sequence(all.map(_.active.track)))
    run_if_needed {
      for {
        all <- all_future
        active_flags <- active_flags_future
      } yield all.zip(active_flags).filter(_._2).map(_._1)
    }
  }

}
