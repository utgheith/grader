package ag.git

import ag.cmd.{CallableCommand, Command}
import os.{CommandResult, Shellable}

import scala.language.implicitConversions

// git [-C <dir>] ...
case class git(C: os.Path | Null = null) extends Command {

  override def parent: Option[Command] = None
  override def my_part: os.Shellable = Seq[os.Shellable]("git", opt("-C", C))
  override def my_remote: Boolean = false

  // git ... fetch <repository> <refspec>
  case class fetch(repository: Repository, refspec: RefSpec)
      extends CallableCommand[Unit] {
    override def parent: Option[Command] = Some(git.this)
    override def my_part: os.Shellable =
      Seq[os.Shellable]("fetch", repository, refspec)
    override def my_remote: Boolean = repository match {
      case u: Repository.Url  => u.url.is_remote
      case _: Repository.Name => true // TODO: check name
    }
    override def translate(res: CommandResult): Unit = ()
  }

  private type NotesOut = Seq[(note_sha: Sha.Note, commit_sha: Sha.Commit)]

  // git ... notes [--ref=<refspec>] ...
  case class notes(ref: String | Null = null)
      extends CallableCommand[NotesOut] {

    override def my_remote = false
    override def parent: Option[Command] = Some(git.this)
    override def my_part: os.Shellable =
      Seq[os.Shellable]("notes", opt("--ref", ref))
    override def translate(res: os.CommandResult): NotesOut = {
      for {
        line <- res.out
          .lines()
      } yield {
        line.split(' ') match {
          case Array(note_sha, commit_sha) =>
            (note_sha = Sha.Note(note_sha), commit_sha = Sha.Commit(commit_sha))
          case _ => throw Exception(s"bad line from 'git notes list': $line")
        }
      }
    }

    // git ... notes ... show <object>
    case class show(obj: CommitId | Null = null)
        extends CallableCommand[Seq[String]] {
      override def parent: Option[Command] = Some(notes.this)
      override def my_remote: Boolean = false
      override def my_part: os.Shellable = if (obj == null) Seq() else obj.name
      override def translate(res: os.CommandResult): Seq[String] =
        res.out.lines()
    }

    // git ... notes ... list

  }
}
