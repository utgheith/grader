package ag.grader.main.misc

import ag.grader.Milestone
import mainargs.{Flag, ParserForClass, TokensReader, arg}
import scala.util.matching.Regex

given TokensReader.Simple[os.Path] with {
  override def shortName: String = "path"

  /** */
  override def read(strings: Seq[String]): Either[String, os.Path] = if (
    strings.isEmpty
  ) {
    Right(os.pwd / "workspace")
  } else {
    val one = strings.mkString("/")
    val p = if (one.startsWith("/")) {
      os.Path(one)
    } else {
      os.pwd / os.RelPath(one)
    }
    Right(p)
  }
}

given TokensReader.Simple[Regex] with {
  override def shortName: String = "regex"

  override def alwaysRepeatable: Boolean = true

  /** */
  override def read(strings: Seq[String]): Either[String, Regex] = if (
    strings.isEmpty
  ) {
    Right(""".*""".r)
  } else {
    Right(Regex(strings.mkString("|")))
  }
}

// a milestone's name, or live
given TokensReader.Simple[Milestone] with {
  override def shortName: String = "milestone|live"
  override def read(strings: Seq[String]): Either[String, Milestone] =
    strings.headOption.filter(_.nonEmpty) match {
      case Some(s) => Right(Milestone.of(s))
      case None    => Left("a milestone is a milestone's name, or live")
    }
}

enum AliasSortMode:
  case CSID
  case Alias

given TokensReader.Simple[AliasSortMode] with {
  override def shortName: String = "mode"
  override def allowEmpty: Boolean = false
  override def alwaysRepeatable: Boolean = false

  /** */
  override def read(strings: Seq[String]): Either[String, AliasSortMode] =
    strings.head match
      case "alias" => Right(AliasSortMode.Alias)
      case "csid"  => Right(AliasSortMode.CSID)
      case s       =>
        Left(s"Invalid sort mode '$s'; possible modes are 'alias' or 'csid'")
}

//@main
case class CommonArgs(
    @arg(name = "courses", short = 'c', doc = "courses to consider (regex)")
    courses_ : Regex = """.*""".r,
    @arg(short = 'p', doc = "projects to consider (regex)")
    projects_ : Regex = """.*""".r,
    @arg(short = 's', doc = "students to consider (regex)")
    students_ : Regex = """.*""".r,
    @arg(short = 't', doc = "tests to consider (regex)")
    tests_ : Regex = """.*""".r,
    @arg(short = 'v', doc = "verbose output")
    verbose: Flag = Flag(false),
    @arg(doc =
      "r3: when done, link every target used under <workspace>/r3/browse/<its path>/@, to look at its value and data"
    )
    browse: Flag = Flag(false),
    @arg(doc =
      "rebuild what this command asks for, even if its saved results are current (their inputs are reused)"
    )
    force: Flag = Flag(false),
    workspace: os.Path = os.pwd / "workspace"
)

object CommonArgs {
  given ParserForClass[CommonArgs] = ParserForClass[CommonArgs]
}
