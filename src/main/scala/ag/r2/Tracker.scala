package ag.r2

import ag.common.{Fork, Signature, Signer}
import upickle.default.{ReadWriter, read, write}

import scala.annotation.implicitNotFound
import scala.collection.{SortedMap, mutable}
import scala.collection.concurrent.TrieMap
import scala.util.Try
import scala.util.control.NonFatal

@implicitNotFound("no given Tracker")
trait Tracker extends Context {

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

  private val added_dependencies = TrieMap[os.RelPath, Fork[Result[?]]]()
  private val done = mutable.Set[os.RelPath]()
  @volatile
  private var phase: Phase = Phase.Open

  def add_dependency(
      d: TargetBase,
      fr: Fork[Result[?]]
  ): Unit = {
    phase.check(Phase.Open, Phase.Closing)
    if (d.is_peek) {
      Context.say(Some(this), s"does not depend on ${d.path.toString}")
    } else {
      Context.say(Some(this), s"depends on ${d.path.toString}")
      added_dependencies.update(d.path, fr)
    }
  }

  // Returns the known dependencies
  private lazy val dependencies: Try[SortedMap[os.RelPath, Signature]] =
    Try {
      phase.check(Phase.Open)
      phase = Phase.Closing
      /* compute transitive closure of all tracked dependencies */
      while (done != added_dependencies.keySet) {
        for {
          (p, r) <- added_dependencies.toSeq
        } {
          if (!done.contains(p)) {
            val _ = done.add(p)
            val _ = r.join
          }
        }
      }

      phase = Phase.Closed

      /* now we have everything */
      val out = (for {
        (p, result) <- added_dependencies.toSeq
      } yield (p, result.join.signature)).to(SortedMap)
      out
    }

  def run[B](
      f: Producer[B] ?=> Option[Result[B]] => Some[
        Result[B]
      ] | (() => B)
  )(using ReadWriter[B]): Result[B] = {
    given producer: Producer[B] = new Producer[B] {
      override val depth: Int = Tracker.this.depth
      override val route: Seq[Target[?]] = Tracker.this.route
      override val producing: Target[?] = Tracker.this.producing_opt.get
      override val state: State = Tracker.this.state

      override def producing_opt: Option[Target[?]] =
        Tracker.this.producing_opt
    }

    // check if we have a failed update
    if (os.exists(producer.dirty_path)) {
      say(s"removing dirty ${producer.target_path.toString}")
      os.remove.all(producer.target_path)
    }

    val old_state: Option[Result[B]] = {

      // (1) let's find out if we have a saved value
      if (os.isFile(producer.saved_path)) {
        try {
          say("loading old state")
          val old = read[Saved[B]](os.read(producer.saved_path))
          // (2) we seem to have one, check if the dependencies changed
          // ctx has the newly discovered dependencies
          // old has the dependency at the time the value was saved

          say(
            s"depends on ${upickle.default.write(old.depends_on.toString, indent = 2)}"
          )

          say("checking dependencies")
          if (old.depends_on.isEmpty) {
            // special case: no dependenices => always recompute
            say("no dependencies, force recompute")
            None
          } else {
            if (
              // general case: check dependencies
              Tracker.this.dependencies.get
                .forall((p, s) => old.depends_on.get(p).contains(s))
            ) {
              say("keeping old state")
              Some(old.result)
            } else {
              say("dependencies changed for old state")
              None
            }
          }
        } catch {
          case NonFatal(e) =>
            say(s"load error ${e.toString}")
            os.remove.all(producer.saved_path)
            None
        }
      } else {
        say(s"no old state")
        None
      }

    }

    f(old_state) match {
      case Some(ra) =>
        ra
      case ffa: (() => B) =>
        // remove the old result
        // say("removing old result")
        // os.remove.all(producer.target_path)

        // Make a backup copy of the old result. This is useful if the computation fails
        // and we want to restore the old result
        if (os.exists(producer.target_path)) {
          say(s"marking ${producer.producing.path.toString} as dirty")
          os.write(producer.dirty_path, "", createFolders = true)
        }

        // clear the log
        os.write.over(producer.log_path, "", createFolders = true)

        val new_value = ffa()

        // We have a new result, store it on disk
        val new_result = Result(new_value, Signer.sign(new_value))
        val new_saved = Saved(new_result, Tracker.this.dependencies.get)
        say("writing new state")
        os.write.over(
          producer.saved_path,
          write(new_saved, indent = 2),
          createFolders = true
        )

        os.remove.all(producer.dirty_path)
        say(s"${producer.producing.path.toString} done")
        new_result

    }

  }

  // pre-condition: ctx is populated with dependencies for this target
  // The simplest way to achieve this is to write code that looks like this:
  //  Target
  //       a.track
  //       b.track
  //       run_if_needed { ... } // depends on a and b
  //

  def run_if_needed[B](
      f: (Producer[B]) ?=> B
  )(using ReadWriter[B]): Result[B] = {
    run {
      case sra @ Some(_) =>
        sra
      case _ =>
        // We either didn't find a result on disk or we found one with changed dependencies.
        // In either case, we forget the old result and evaluate again

        () => f.apply
    }
  }

}
