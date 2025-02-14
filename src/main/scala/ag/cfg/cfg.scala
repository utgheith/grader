package ag.cfg

import upickle.default.{Reader, read}

import scala.reflect.ClassTag

inline def load[A: {ClassTag, Reader}]: A = {
  val ct = summon[ClassTag[A]]
  val nm = ct.runtimeClass.getName
  val path = os.pwd / "configs" / os.RelPath(nm)
  println(s"loading config from $path")
  read[A](os.read(path))
}
