package ag.grader

import ag.common.given_ReadWriter_SortedMap
import ag.r3.{Scope, Target, periodic}

import scala.collection.SortedMap

// The students' ssh keys, as they drop them in `root`/<course>/<csid>.pub
// (each file owned by its student), as r3 targets: looked at again every
// minute. No root, no keys. The r3 form of Course.dropbox.
final class Dropbox(val root: Option[os.Path]) extends Scope("dropbox") {

  lazy val keys: String => Target[SortedMap[CSID, String]] = fun {
    (c: String) =>
      target((clock = periodic(60 * 1000L))) { (_, _) =>
        root.fold(SortedMap[CSID, String]()) { root =>
          (for {
            f <- os.list(root / c)
            if os.isFile(f) && f.ext == "pub"
          } yield {
            val id = f.baseName
            val owner = os.owner(f).getName
            if (owner != id) throw Exception(s"owner:$owner != csid:$id")
            (CSID(id), os.read(f).trim.nn)
          }).to(SortedMap)
        }
      }
  }
}
