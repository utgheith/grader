package ag.rules

import upickle.default.ReadWriter

import scala.NamedTuple.NamedTuple

// A tree pinned by its id: a target's data, in the workspace's store, or a
// commit's files in a Source repo (`at`: the repo's directory). It's read
// through a Context (read, list, checkout), never through a path that could
// change under the reader.
final case class Tree(id: String, at: Option[String] = None) derives ReadWriter

// a commit in a Source repo (`at`), pinned by its id
final case class Commit(at: String, id: String) derives ReadWriter

// what a Context tells about a Commit
@upickle.implicits.allowUnknownKeys(false)
final case class CommitInfo(
    id: String,
    parents: Seq[String],
    // the committer's time, with their offset (as git's %cI)
    committed: String,
    message: String,
    tree: Tree
) derives ReadWriter {
  def subject: String = message.linesIterator.nextOption().getOrElse("")
}

@upickle.implicits.allowUnknownKeys(false)
final case class WithData[A](value: A, data: Tree)

object WithData {
  given [A: ReadWriter] => ReadWriter[WithData[A]] =
    ReadWriter.derived[WithData[A]]
}

// A target is a named, persistent, lazy computation: it runs
// again only if it never ran, or something it depends on changed.
trait TargetBase {
  // its unique name
  val path: os.RelPath
  // where its value is saved (with its log, and its mark if forced): its
  // path, but for a keyed target asked for in several ways (see
  // Keyed.keyed_target), which are targets of their own that all keep one
  // value, saved at the key, and take turns at it
  val stored: os.RelPath
  // the definition that made it: its path without scope and parameters
  val kind: os.RelPath
  val is_peek: Boolean
  // what a person is shown for it (e.g. on the status screen), if not its
  // path: for one whose path says little, like a keyed target's hash
  val label: Option[String]
  def display: String = label.getOrElse(path.toString)
}

trait Target[A] extends TargetBase { outer =>
  def make(tracker: Tracker[A]): Result[A]

  def append(p: os.RelPath): Target[A] = new Target[A] {
    override val path: os.RelPath = outer.path / p
    override val stored: os.RelPath = outer.stored / p
    override val kind: os.RelPath = outer.kind
    override def make(tracker: Tracker[A]): Result[A] = outer.make(tracker)
    override val is_peek: Boolean = outer.is_peek
    override val label: Option[String] =
      outer.label.map(l => s"$l ${p.toString}")
  }

  // Tracks it, adding its value's measure to the run's tally (State.tally,
  // labeled by its kind) if there's a Measure for it. Meant for commands:
  // a target computing doesn't run when it's current, so what it would add
  // there depends on what ran before.
  inline def tally(tracker: Tracker[?]): A = {
    val a = tracker.track(this)
    scala.compiletime.summonFrom {
      case m: Measure[A] =>
        tracker.state.tally.add(Measure.label(kind), m.measure(a))
      case _ => ()
    }
    a
  }

  // the same target, read without becoming a dependency
  def peek: Target[A] =
    if (is_peek) this
    else
      new Target[A] {
        override val path: os.RelPath = outer.path
        override val stored: os.RelPath = outer.stored
        override val kind: os.RelPath = outer.kind
        override def make(tracker: Tracker[A]): Result[A] = outer.make(tracker)
        override val is_peek: Boolean = true
        override val label: Option[String] = outer.label
      }
}

// Many targets of one type, as a field of the named tuple given to target or
// Tracker.track_all. Its value is the targets' values, in order.
final class Many[A](val targets: Seq[Target[A]])

object Target {
  type Value[T] = T match {
    case Many[a]   => Seq[a]
    case Target[a] => a
  }

  type Values[N <: Tuple, V <: Tuple] = NamedTuple[N, Tuple.Map[V, Value]]

  def apply[A](
      p: os.RelPath,
      k: os.RelPath,
      l: Option[String] = None,
      s: Option[os.RelPath] = None
  )(
      f: Tracker[A] => Result[A]
  ): Target[A] = new Target[A] {
    override val path: os.RelPath = p
    override val stored: os.RelPath = s.getOrElse(p)
    override val kind: os.RelPath = k
    override def make(tracker: Tracker[A]): Result[A] = f(tracker)
    override val is_peek: Boolean = false
    override val label: Option[String] = l
  }
}

// a computed value and its signature: the id of its result in the store
final case class Result[+A](value: A, signature: ag.common.Signature)
