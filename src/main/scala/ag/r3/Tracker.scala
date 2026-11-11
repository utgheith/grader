package ag.r3

import ag.common.{
  NoNulls,
  Signature,
  given_ReadWriter_RelPath,
  given_ReadWriter_SortedMap
}
import ag.r2.Task
import org.eclipse.jgit.lib.ObjectId
import upickle.default.{ReadWriter, write}

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.concurrent.TrieMap
import scala.collection.{SortedMap, mutable}
import scala.NamedTuple.NamedTuple
import scala.util.control.NonFatal

// Reads targets for the target being computed (or for the program, as its
// State), recording them as its dependencies; as in ag.r2.
trait Tracker[A] extends Context[A] {

  enum Phase {
    case Open, Closing, Closed
  }

  private val added = TrieMap[os.RelPath, Task[Result[?]]]()
  private val done = mutable.Set[os.RelPath]()
  @volatile private var phase: Phase = Phase.Open

  private[r3] def check_open(): Unit =
    if (phase != Phase.Open)
      throw IllegalStateException(
        s"[${producing_opt.map(_.path).toString}] dependencies are closed"
      )

  def track[B](target: Target[B]): B = {
    val (task, run) = state.start(target, this, here = true)
    run()
    task.block.value.asInstanceOf[B]
  }

  def track_all[B](targets: Seq[Target[B]]): Seq[B] =
    read_all(targets).map(_.asInstanceOf[B])

  def track_all[N <: Tuple, V <: Tuple](targets: NamedTuple[N, V])(using
      Tuple.Union[V] <:< (Target[?] | Many[?])
  ): Target.Values[N, V] = {
    val fields = targets.toTuple.productIterator.toSeq
    val all = fields.flatMap {
      case t: Target[?] => Seq(t)
      case m: Many[?]   => m.targets
    }
    val values = read_all(all).iterator
    val out = fields.map {
      case _: Target[?] => values.next()
      case m: Many[?]   => values.take(m.targets.size).toList
    }
    Tuple.fromArray(out.toArray).asInstanceOf[Target.Values[N, V]]
  }

  // all of them finish, so their work is saved, before a failure is reported
  private def read_all(targets: Seq[Target[?]]): Seq[Any] = {
    val started = targets.zipWithIndex.map { (t, i) =>
      state.start(t, this, here = i == targets.size - 1)
    }
    started.foreach(_._2())
    started.map(_._1.block_try).map(_.get.value)
  }

  private[r3] def add_dependency(d: TargetBase, r: Task[Result[?]]): Unit = {
    if (phase == Phase.Closed)
      throw IllegalStateException(
        s"[${producing_opt.map(_.path).toString}] dependencies are closed"
      )
    if (!d.is_peek) added.update(d.path, r)
  }

  // what this target read, and their signatures: everything it reads, also
  // what those reads go on to read, has finished
  private lazy val dependencies: SortedMap[os.RelPath, Signature] = {
    phase = Phase.Closing
    while (done != added.keySet) {
      for ((p, r) <- added.toSeq if !done.contains(p)) {
        val _ = done.add(p)
        val _ = r.block
      }
    }
    phase = Phase.Closed
    added.toSeq.map((p, r) => (p, r.block.signature)).to(SortedMap)
  }

  // Reuses the target's saved result if what it depends on hasn't changed,
  // or computes it: `f` gets the saved result, if it's still good, and
  // returns it or a computation.
  def run(
      f: (Producer[A], Option[Result[A]]) => Some[Result[A]] | (() => A)
  )(using ReadWriter[A]): Result[A] = {
    val target = producing_opt.get
    val forced = os.exists(state.forced_path(target))
    val loaded = state.store.load(target.path)

    var reason = ""
    def because(why: String): Unit = if (reason.isEmpty) reason = why

    val saved: Option[Result[A]] = loaded match {
      case None =>
        because("it has no saved result")
        None
      case Some(_) if forced =>
        because("it was forced, or its last forced run failed")
        None
      case Some(l) =>
        try {
          val deps =
            upickle.default.read[SortedMap[os.RelPath, Signature]](
              String(l.deps, UTF_8)
            )
          val value = NoNulls.check(
            upickle.default.read[A](String(l.value, UTF_8)),
            s"the saved value of ${target.path.toString}"
          )
          if (deps.isEmpty) {
            because("it has no dependencies")
            None
          } else {
            val changed = dependencies.toSeq.collect {
              case (p, s) if !deps.get(p).contains(s) => p
            }
            if (changed.isEmpty)
              Some(Result(value, Signature(l.result.name.nn)))
            else {
              because(
                s"dependencies changed: ${changed.map(_.toString).mkString(", ")}"
              )
              None
            }
          }
        } catch {
          case NonFatal(e) =>
            because(s"its saved result can't be read: ${e.toString}")
            None
        }
    }

    val producer: Producer[A] = new Producer[A] {
      override val depth: Int = Tracker.this.depth
      override val route = Tracker.this.route
      override val state: State = Tracker.this.state
      override def producing_opt: Option[Target[A]] = Some(target)
      override def producing: Target[A] = target
      override private[r3] val previous_data: Option[ObjectId] =
        loaded.flatMap(_.data)
    }

    f(producer, saved) match {
      case Some(r)            => r
      case compute: (() => A) =>
        if (reason.isEmpty) because("it chose to run again")
        os.write.over(producer.log_path, "", createFolders = true)
        val value = compute()
        val id = state.store.save(
          path = target.path,
          old = loaded.map(_.commit),
          value = write(value, indent = 2).getBytes(UTF_8),
          deps = write(dependencies, indent = 2).getBytes(UTF_8),
          data = producer.data,
          message = reason
        )
        os.remove.all(state.forced_path(target))
        Result(value, Signature(id.name.nn))
    }
  }

  def run_if_needed(f: Producer[A] => A)(using ReadWriter[A]): Result[A] =
    run {
      case (_, saved @ Some(_)) => saved
      case (producer, None)     => () => f(producer)
    }
}
