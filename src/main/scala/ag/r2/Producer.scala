package ag.r2

import scala.annotation.implicitNotFound

@implicitNotFound("no given Producer")
trait Producer[A] extends Context[A] {
  def producing: Target[A]
  // def old_state: OldState[A]

  lazy val target_path: os.Path = state.target_path(producing)
  lazy val saved_path: os.Path = state.saved_path(producing)
  lazy val dirty_path: os.Path = state.dirty_path(producing)
  lazy val data_path: os.Path = state.data_path(producing)
}
