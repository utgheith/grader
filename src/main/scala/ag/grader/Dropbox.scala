package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.{Scope, Target, periodic}

import scala.collection.SortedMap

// The students' ssh keys, as they drop them in `root`/<course>/<csid>.pub
// (each file owned by its student, or by `keeper`: the grader's own user,
// who can put a key there for a student), as r3 targets: looked at again
// every minute. No root, or no directory for the course, no keys. The r3
// form of Course.dropbox.
final class Dropbox(
    val root: Option[os.Path],
    val keeper: String = System.getProperty("user.name").nn
) extends Scope("dropbox") {

  // A course's keys; None if there's no root, or no directory for the
  // course (which isn't the same as no keys: see Publishers.enrollment)
  lazy val course_keys: CourseName => Target[Option[SortedMap[CSID, String]]] =
    fun { (c: CourseName) =>
      target((clock = periodic(60 * 1000L))) { (_, _) =>
        for {
          root <- root
          dir = root / c.value
          if os.isDir(dir)
        } yield (for {
          f <- os.list(dir)
          if os.isFile(f) && f.ext == "pub"
        } yield {
          val id = f.baseName
          val owner = os.owner(f).getName
          if (owner != id && owner != keeper)
            throw Exception(s"owner:$owner != csid:$id (or $keeper)")
          (CSID(id), os.read(f).trim.nn)
        }).to(SortedMap)
      }
    }

  // a course's keys, none if it has no directory
  lazy val keys: CourseName => Target[SortedMap[CSID, String]] = fun {
    (c: CourseName) =>
      target((k = course_keys(c)))((_, deps) =>
        deps.k.getOrElse(SortedMap[CSID, String]())
      )
  }
}
