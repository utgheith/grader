package ag.cfg

import ag.common.Scoped
import upickle.default.{Reader, read}
import scala.reflect.ClassTag

val baseDir = Scoped(os.pwd / "configs")

inline def load[A: {ClassTag, Reader}]: A = {
  val ct = summon[ClassTag[A]]
  val nm = ct.runtimeClass.getName
  val path = baseDir.get / os.RelPath(nm)
  println(s"loading config from ${path.toString}")
  read[A](os.read(path))
}
