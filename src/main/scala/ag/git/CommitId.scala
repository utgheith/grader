package ag.git

import upickle.default.ReadWriter

sealed trait CommitId derives ReadWriter {
  final def parent(back: Int = 1): CommitId.Parent = this match {
    case CommitId.Parent(base, b) => CommitId.Parent(base, b + back)
    case simple: CommitId.Simple  => CommitId.Parent(simple, back)
  }
  def name: String
}

object CommitId {

  sealed trait Simple extends CommitId derives ReadWriter

  case object HEAD extends Simple {
    override val name: String = "HEAD"
  }

  case class SHA(sha: Sha.Commit) extends Simple {
    override def name: String = sha.sha
  }

  case class Name(override val name: String) extends Simple {}

  case class Parent(base: Simple, back: Int) extends CommitId {
    override lazy val name: String = s"${base.name}~$back"
  }
}
