package ag.git

sealed trait GitUrl {
  def host: String
  def port: Option[Int]

}

object GitUrl {
  case class Ssh(
      user: Option[String],
      host: String,
      port: Option[Int],
      path: os.RelPath
  ) extends GitUrl {
    override lazy val toString: String = {
      val u = user.map(x => s"$x@").getOrElse("")
      val p = port.map(x => s":$x").getOrElse("")
      s"ssh://$u$host$p/$path"
    }
  }
  def ssh(
      user: String | Null = null,
      host: String,
      port: Int | Null = null,
      path: os.RelPath
  ): Ssh =
    Ssh(
      user = if (user == null) None else Some(user),
      host = host,
      port = if (port == null) None else Some(port),
      path = path
    )

  case class Git(host: String, port: Option[Int], path: os.RelPath)
      extends GitUrl {
    override lazy val toString: String = {
      val p = port.map(x => s":$x").getOrElse("")
      s"git://$host$p/$path"
    }
  }

  case class Http(
      secure: Boolean,
      host: String,
      port: Option[Int],
      path: os.RelPath
  ) extends GitUrl {
    override lazy val toString: String = {
      val p = port.map(x => s":$x").getOrElse("")
      val s = if (secure) "s" else ""
      s"http$s://$host$p/$path"
    }
  }

  case class Ftp(
      secure: Boolean,
      host: String,
      port: Option[Int],
      path: os.RelPath
  ) extends GitUrl {
    override lazy val toString: String = {
      val p = port.map(x => s":$x").getOrElse("")
      val s = if (secure) "s" else ""
      s"ftp$s://$host$p/$path"
    }
  }

  given Conversion[GitUrl, os.Shellable] = url =>
    os.Shellable(Seq(url.toString))
}
