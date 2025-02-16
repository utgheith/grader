package ag.git

sealed trait Repository

object Repository {
  case class Name(name: String) extends Repository {
    override def toString: String = name
  }
  case class Url(url: GitUrl) extends Repository {
    override def toString: String = url.toString
  }

  given Conversion[Repository, os.Shellable] = r =>
    os.Shellable(Seq(r.toString))
}
