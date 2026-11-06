package ag.r2

import ag.common.{Signature, Signer}
import ag.task.Task
import upickle.default.{ReadWriter, read, write}

import scala.collection.{mutable, SortedMap}
import scala.collection.concurrent.TrieMap
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
  // (of nothing, for the program's top level: its State)
  def track[B](target: Target[B]): Task[B] = state.resolve(target, this)

  def guilty[B](target: Target[B]): B = track(target).block

  def add_dependency(d: TargetBase, fr: Task[Result[?]]): Unit = {
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
      f: (Producer[A], Option[Result[A]]) => Some[Result[A]] | (() => Task[A])
  )(using ReadWriter[A]): Task[Result[A]] = {
    val producer: Producer[A] = new Producer[A] {
      override val depth: Int = Tracker.this.depth
      override val route = Tracker.this.route
      override val producing: Target[A] = Tracker.this.producing_opt.get
      override val state: State = Tracker.this.state

      override def producing_opt: Option[Target[A]] = Tracker.this.producing_opt
    }

    // check if we have a failed update
    if (os.exists(producer.dirty_path)) {
      say(producer, s"removing dirty ${producer.target_path.toString}")
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
              None
            }
          }
        } catch {
          case NonFatal(e) =>
            say(producer, s"load error ${e.toString}")
            os.remove.all(producer.saved_path)
            None
        }
      } else {
        say(producer, s"no old state")
        None
      }

    }

    f(producer, old_state) match {
      case Some(ra) =>
        Task.successful(ra)
      case ffa: (() => Task[A]) =>
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
        ffa().map { new_value =>
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

  }

  // pre-condition: ctx is populated with dependencies for this target
  // The simplest way to achieve this is to write code that looks like this:
  //  Target
  //       tracker.track(a)
  //       tracker.track(b)
  //       run_if_needed(tracker) { ... } // depends on a and b
  //

  def run_if_needed(
      f: Producer[A] => Task[A]
  )(using ReadWriter[A]): Task[Result[A]] = {
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
