package ag.r2

import ag.common.{Signature, Signer}
import upickle.default.{ReadWriter, read, write}

import scala.collection.{mutable, SortedMap}
import scala.collection.concurrent.TrieMap
import scala.NamedTuple.NamedTuple
import scala.util.control.NonFatal

trait Tracker[A] extends Context[A] {

  enum Phase {
    case Open
    case Closing
    case Closed

    def check(desired: Phase*): Unit = {
      val s = this;
      if (!desired.contains(s)) {
        throw new IllegalStateException(
          s"[${producing_opt.map(_.path).toString}] Invalid phase: ${s.toString}"
        )
      }
    }
  }

  private val added_dependencies = TrieMap[os.RelPath, Task[Result[?]]]()
  private val done = mutable.Set[os.RelPath]()
  @volatile
  private var phase: Phase = Phase.Open

  def check_phase(desired: Phase*): Unit = {
    phase.check(desired*)
  }

  // `target`'s current value, making it a dependency of the target this tracks
  // (of nothing, for the program's top level: its State). It's computed on this
  // thread, unless another reader got there first.
  def track[B](target: Target[B]): B = {
    val (task, run) = state.start(target, this, here = true)
    run()
    task.block.value.asInstanceOf[B]
  }

  // The same for many targets at once, each on its own thread. All of them
  // finish, so that their work is saved, before any failure is reported.
  def track_all[B](targets: Seq[Target[B]]): Seq[B] =
    read_all(targets).map(_.asInstanceOf[B])

  // ...and for a named tuple of Targets and Manys, which can be read at once
  // even when their types differ: the values have the same names.
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

  private def read_all(targets: Seq[Target[?]]): Seq[Any] = {
    // the last one can use this thread, which has to wait anyway
    val started = targets.zipWithIndex.map { (t, i) =>
      state.start(t, this, here = i == targets.size - 1)
    }
    started.foreach(_._2())
    val outcomes = started.map(_._1.block_try)
    outcomes.map(_.get.value)
  }

  private[r2] def add_dependency(d: TargetBase, fr: Task[Result[?]]): Unit = {
    phase.check(Phase.Open, Phase.Closing)
    if (d.is_peek) {
      Context.say(Some(this), s"does not depend on ${d.path.toString}")
    } else {
      Context.say(Some(this), s"depends on ${d.path.toString}")
      added_dependencies.update(d.path, fr)
    }
  }

  // Returns the known dependencies
  private lazy val dependencies: SortedMap[os.RelPath, Signature] = {
    phase.check(Phase.Open)
    phase = Phase.Closing
    /* compute transitive closure of all tracked dependencies */
    while (done != added_dependencies.keySet) {
      for {
        (p, r) <- added_dependencies.toSeq
      } {
        if (!done.contains(p)) {
          val _ = done.add(p)
          val _ = r.block
        }
      }
    }

    phase = Phase.Closed

    /* now we have everything */
    (for {
      (p, result) <- added_dependencies.toSeq
    } yield (p, result.block.signature)).to(SortedMap)
  }

  def run(
      f: (Producer[A], Option[Result[A]]) => Some[Result[A]] | (() => A)
  )(using ReadWriter[A]): Result[A] = {
    val producer: Producer[A] = new Producer[A] {
      override val depth: Int = Tracker.this.depth
      override val route = Tracker.this.route
      override val producing: Target[A] = Tracker.this.producing_opt.get
      override val state: State = Tracker.this.state

      override def producing_opt: Option[Target[A]] = Tracker.this.producing_opt
    }

    // why it has to run, if it does, and which dependencies changed
    var reason: Option[String] = None
    var changed: Seq[os.RelPath] = Seq()
    def because(why: String): Unit = if (reason.isEmpty) reason = Some(why)

    // check if we have a failed update
    if (os.exists(producer.dirty_path)) {
      say(producer, s"removing dirty ${producer.target_path.toString}")
      because("its last run failed, or it was forced")
      os.remove.all(producer.target_path)
    }

    val old_state: Option[Result[A]] = {

      // (1) let's find out if we have a saved value
      if (os.isFile(producer.saved_path)) {
        try {
          say(producer, "loading old state")
          val old = read[Saved[A]](os.read(producer.saved_path))
          // a null means an older format: treat it like any unreadable state
          val _ = ag.common.NoNulls.check(
            old.result.value,
            s"the saved value of ${producer.producing.path.toString}"
          )
          // (2) we seem to have one, check if the dependencies changed
          // ctx has the newly discovered dependencies
          // old has the dependency at the time the value was saved

          say(
            producer,
            s"depends on ${upickle.default.write(old.depends_on.toString, indent = 2)}"
          )

          say(producer, "checking dependencies")
          if (old.depends_on.isEmpty) {
            // special case: no dependenices => always recompute
            say(producer, "no dependencies, force recompute")
            because("it has no dependencies")
            None
          } else {
            if (
              // general case: check dependencies
              Tracker.this.dependencies
                .forall((p, s) => old.depends_on.get(p).contains(s))
            ) {
              say(producer, "keeping old state")
              Some(old.result)
            } else {
              say(producer, "dependencies changed for old state")
              because("dependencies changed")
              changed = Tracker.this.dependencies.toSeq.collect {
                case (p, s) if !old.depends_on.get(p).contains(s) => p
              }
              None
            }
          }
        } catch {
          case NonFatal(e) =>
            say(producer, s"load error ${e.toString}")
            because(s"its saved value can't be read: ${e.toString}")
            os.remove.all(producer.saved_path)
            None
        }
      } else {
        say(producer, s"no old state")
        because("it has no saved value")
        None
      }

    }

    f(producer, old_state) match {
      case Some(ra) =>
        state.graph.decided(producer.producing.path, "reused", None, Seq())
        ra
      case ffa: (() => A) =>
        // remove the old result
        // say(producer, "removing old result")
        // os.remove.all(producer.target_path)

        // Make a backup copy of the old result. This is useful if the computation fails
        // and we want to restore the old result
        if (os.exists(producer.target_path)) {
          say(producer, s"marking ${producer.producing.path.toString} as dirty")
          os.write(producer.dirty_path, "", createFolders = true)
        }

        // clear the log
        os.write.over(producer.log_path, "", createFolders = true)

        // Run the computation
        because("its saved value is current, but it chose to run again")
        state.graph.decided(producer.producing.path, "ran", reason, changed)
        state.control.checkpoint(producer.producing, Control.Gate.Run)
        state.graph.running(producer.producing.path)
        val new_value =
          try ffa()
          finally state.graph.ran(producer.producing.path)
        // We have a new result, store it on disk
        val new_result = Result(new_value, Signer.sign(new_value))
        val new_saved = Saved(new_result, Tracker.this.dependencies)
        say(producer, "writing new state")
        os.write.over(
          producer.saved_path,
          write(new_saved, indent = 2),
          createFolders = true
        )

        os.remove.all(producer.dirty_path)
        say(producer, s"${producer.producing.path.toString} done")
        new_result
    }

  }

  // pre-condition: ctx is populated with dependencies for this target
  // The simplest way to achieve this is to write code that looks like this:
  //  Target
  //       tracker.track(a)
  //       tracker.track(b)
  //       run_if_needed(tracker) { ... } // depends on a and b
  //

  def run_if_needed(
      f: Producer[A] => A
  )(using ReadWriter[A]): Result[A] = {
    run {
      case (_, sra @ Some(_)) =>
        sra
      case (producer, _) =>
        // We either didn't find a result on disk or we found one with changed dependencies.
        // In either case, we forget the old result and evaluate again
        () => f(producer)
    }
  }

}
