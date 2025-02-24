package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r2.Target

import scala.collection.{SortedMap, SortedSet, mutable}
import upickle.default.ReadWriter

case class Course(course_name: String) derives ReadWriter {

  private val scope = os.RelPath(course_name)

  lazy val staff_group_name: String = s"@${course_name}_staff"

  private lazy val raw: Target[RawCourse] = Gitolite.raw_course(course_name)

  lazy val notifications: Maker[NotificationConfig] =
    Rule(raw, scope)(_.notifications)

  lazy val projects: Maker[SortedMap[String, Project]] =
    Rule(raw, scope) { rc =>
      val s = for {
        pn <- rc.projects.keySet.toSeq
      } yield (pn, Project(this, pn))
      SortedMap(s*)
    }

  lazy val active: Maker[Boolean] = Rule(raw, scope) { rc =>
    rc.active
  }

  lazy val staff: Maker[SortedSet[CSID]] = Rule(raw, scope) { rc => rc.staff }

  lazy val active_projects: Maker[SortedMap[String, Project]] =
    Rule(Maker.select(projects.map(_.values.toSeq))(_.active), scope) {
      projects =>
        projects.map(p => (p.project_name, p)).to(SortedMap)
    }

  def project(project_name: String): Project =
    Project(this, project_name)

  // Get a sorted list of csids in the dropbox
  lazy val dropbox: Maker[SortedMap[CSID, String]] =
    Rule(Periodic(60000) *: Config.dropbox_path, scope) {
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

  lazy val publish_keys: Maker[SignedPath[SortedMap[CSID, String]]] =
    SignedPath.rule(
      Gitolite.repo_info(
        "gitolite-admin"
      ) *: dropbox *: notifications.peek *: Config.can_send_mail *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) { case (dir, (g, dropbox, notifications, can_send_mail, can_push_repo)) =>
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
            .exists(key_file_path) || os.read(key_file_path).trim.nn != new_key
        } {
          // If we make it here then the key needs to be added or updated
          say(s"adding $csid to gitolite-admin")
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

  private lazy val enrollment_repo_name = s"${course_name}__enrollment"
  private def enrollment_file(dir: os.Path): os.Path = dir / "enrollment.json"

  // read the enrollment repo

  lazy val enrollment: Maker[SortedMap[CSID, String]] =
    Rule(Gitolite.mirror(enrollment_repo_name), scope) { sp =>
      if (sp.data) {
        val f = enrollment_file(sp.path)
        if (os.isFile(f)) {
          upickle.default.read[SortedMap[CSID, String]](
            os.read(enrollment_file(sp.path))
          )
        } else {
          say(s"enrollment file $f doesn't exist")
          SortedMap()
        }
      } else {
        SortedMap[CSID, String]()
      }
    }

  // create/update the enrollment repo
  lazy val publish_enrollment: Maker[SignedPath[SortedMap[CSID, String]]] =
    SignedPath.rule(
      Gitolite.repo_info(
        enrollment_repo_name
      ) *: publish_keys *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) { case (dir, (g, keys, can_push_repo)) =>
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
          upickle.default.write(keys.data, indent = 2)
        )
        keys.data
      }
    }

  private lazy val grades_repo_name = s"${course_name}__grades"

  lazy val create_grades_repo: Maker[SignedPath[Unit]] =
    SignedPath.rule(
      Gitolite.repo_info(grades_repo_name) *: Config.can_push_repo,
      SortedSet(".git"),
      scope
    ) { case (dir, (g, can_push_repo)) =>
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

object Course {
  given Ordering[Course] = Ordering.by(_.course_name)

  lazy val all: Maker[Seq[Course]] =
    for {
      courses <- Gitolite.course_names
    } yield for {
      cn <- courses.toSeq
    } yield Course(cn)

  lazy val active_courses: Maker[Seq[Course]] = Maker.select(all)(_.active)

}
