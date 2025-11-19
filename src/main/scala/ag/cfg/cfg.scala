package ag.cfg

import upickle.default.{Reader, read}

import scala.reflect.ClassTag

val configBase: ScopedValue[os.Path] = ScopedValue.newInstance()

inline def load[A: {ClassTag, Reader}]: A = {
  val ct = summon[ClassTag[A]]
  val nm = ct.runtimeClass.getName
  val path = configBase.orElse(os.pwd / "configs") / os.RelPath(nm)
  println(s"loading config from ${path.toString}")
  read[A](os.read(path))
}
