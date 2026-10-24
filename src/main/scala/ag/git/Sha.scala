package ag.git

import upickle.default.ReadWriter

sealed trait Sha derives ReadWriter {
  val sha: String
}

object Sha {
  @upickle.implicits.allowUnknownKeys(false)
  case class Note(val sha: String) extends Sha
  @upickle.implicits.allowUnknownKeys(false)
  case class Commit(val sha: String) extends Sha

  implicit def orderingForSha[T <: Sha]: Ordering[T] = new Ordering[T] {

    override def compare(x: T, y: T): Int =
      Ordering[String].compare(x.sha, y.sha)
  }
}
