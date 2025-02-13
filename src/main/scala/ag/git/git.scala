package ag.git

import os.{CommandResult, Shellable}
import scala.language.experimental.namedTuples
import scala.language.implicitConversions

// git [-C <dir>] ...
case class git(C: os.Path | Null = null) extends Command {

  override def my_part: os.Shellable = Seq[os.Shellable]("git", opt("-C", C))

  // git ... fetch <repository> <refspec>
  case class fetch(repository: Repository, refspec: RefSpec)
      extends CallableCommand[Unit]
      with SubCommand {
    override def parent: Command = git.this
    override def my_part: os.Shellable =
      Seq[os.Shellable]("fetch", repository, refspec)
    override def translate(res: CommandResult): Unit = ()
  }

  type NotesOut = Seq[(note_sha: Sha.Note, commit_sha: Sha.Commit)]

  // git ... notes [--ref=<refspec>] ...
  case class notes(ref: String | Null = null)
      extends CallableCommand[NotesOut]
      with SubCommand {

    override def parent: Command = git.this
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
          case x => throw Exception(s"bad line from 'git notes list': $line")
        }
      }
    }

    // git ... notes ... list

  }
}
