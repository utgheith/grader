package ag.grader.repos

import ag.common.ToRelPath
import ag.grader.{Course, Notice, Publish}
import ag.rules.{Target, WithData}

// The students' keys in gitolite-admin, as they are in the dropbox: added
// or changed, never removed (as Course.publish_keys, for every active
// course at once, since they share the repo); each student whose key is
// added or changed is told (for the first course that has it), as their
// course says. It's never made.
final case class KeysRepo(env: Env) extends AdditiveRepo[KeysRepo]
    derives ToRelPath {
  def repo_name: String = KeysRepo.name
  def create: None.type = None
  val message = "updated keys"

  lazy val latest: Target[WithData[Publish]] =
    additive { tracker =>
      val courses =
        tracker
          .track(CoursesConfig(env).active_courses(CoursesConfig.any))
          .toSeq
      val keys = tracker.track_all(courses.map(env.dropbox.keys))
      val notifications =
        tracker.track_all(courses.map(Course(_, env).notifications))
      courses.lazyZip(keys).lazyZip(notifications).toSeq
    } { (courses, _, dir) =>
      for {
        (c, keys, n) <- courses
        (csid, key) <- keys.toSeq
        f = dir / "keydir" / s"${csid.value}.pub"
        if !os.exists(f) || os.read(f).trim.nn != key
        notice <- {
          os.write.over(f, key, createFolders = true)
          if (n.key_update)
            Notice.routed(n, csid, Notice.What.KeyUpdated(c, csid))
          else Seq()
        }
      } yield notice
    }
}

object KeysRepo {
  val name = "gitolite-admin"
}
