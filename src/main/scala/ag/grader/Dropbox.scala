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

  lazy val keys: String => Target[SortedMap[CSID, String]] = fun {
    (c: String) =>
      target((clock = periodic(60 * 1000L))) { (_, _) =>
        root.fold(SortedMap[CSID, String]()) { root =>
          (for {
            // a course without a directory has no keys yet
            f <- if (os.isDir(root / c)) os.list(root / c) else Seq()
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
  }
}
