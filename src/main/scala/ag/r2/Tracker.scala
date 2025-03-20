package ag.r2

import ag.common.{block, Signature, Signer}
import upickle.default.{read, ReadWriter, write}

import scala.annotation.implicitNotFound
import scala.collection.SortedMap
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.control.NonFatal

@implicitNotFound("no given Tracker")
trait Tracker[A] extends Context[A] {

  private val added_dependencies = TrieMap[os.RelPath, Future[Result[?]]]()

  def add_dependency(d: TargetBase, fr: Future[Result[?]]): Unit = {
    if (d.is_peek) {
      Context.say(Some(this), s"does not depend on ${d.path}")
    } else {
      Context.say(Some(this), s"depends on ${d.path}")
      added_dependencies.update(d.path, fr)
    }
  }

  // Returns the known dependencies
  private lazy val dependencies: SortedMap[os.RelPath, Signature] = {
    (for {
      (p, result) <- added_dependencies.toSeq
    } yield (p, result.block.signature)).to(SortedMap)
  }

  def run(
      f: Producer[A] ?=> OldState[A] => OldState.Current[A] | (() => Future[A])
  )(using ReadWriter[A]): Future[Result[A]] = {
    val producer: Producer[A] = new Producer[A] {
      override val depth: Int = Tracker.this.depth
      override val producing: Target[A] = Tracker.this.producing_opt.get
      override val state: State = Tracker.this.state

      override def producing_opt: Option[Target[A]] = Tracker.this.producing_opt

      override def execute(runnable: Runnable): Unit = state.execute(runnable)

      override def reportFailure(cause: Throwable): Unit =
        state.reportFailure(cause)
    }

    val old_state: OldState[A] = {
      if (os.exists(producer.dirty_path)) {
        say("removing dirty state")
        os.remove.all(producer.target_path)
      }

      // (1) let's find out if we have a saved value
      if (os.isFile(producer.saved_path)) {
        try {
          say("loading old state")
          val old = read[Saved[A]](os.read(producer.saved_path))
          // (2) we seem to have one, check if the dependencies changed
          // ctx has the newly discovered dependencies
          // old has the dependency at the time the value was saved
          say("checking dependencies")
          if (
            Tracker.this.dependencies
              .forall((p, s) => old.depends_on.get(p).contains(s))
          ) {
            say("keeping old state")
            OldState.Current(old)
          } else {
            say("dependencies changed for old state")
            OldState.Expired(old)
          }
        } catch {
          case NonFatal(e) =>
            say(s"load error $e")
            os.remove.all(producer.saved_path)
            OldState.Missing
        }
      } else {
        say(s"no old state")
        OldState.Missing
      }

    }

    f(using producer)(old_state) match {
      case OldState.Current(ov) =>
        Future.successful(ov.result)
      case ffa: (() => Future[A]) =>
        // remove the old result
        say("removing old result")
        os.remove.all(producer.target_path)

        // mark it as dirty while we compute it. This allows is to recover is the program
        // terminates in the middle of the computation
        say("marking dirty")
        os.write.over(producer.dirty_path, "", createFolders = true)

        // Run the computation (asynchronous)
        ffa().map { new_value =>
          // We have a new result, store it on disk
          val new_result = Result(new_value, Signer.sign(new_value))
          val new_saved = Saved(new_result, Tracker.this.dependencies)
          say("writing new state")
          os.write.over(
            producer.saved_path,
            write(new_saved, indent = 2),
            createFolders = true
          )

          os.remove.all(producer.dirty_path)
          say("done")
          new_result
        }(using producer.state)
    }

  }

  // pre-condition: ctx is populated with dependencies for this target
  // The simplest way to achieve this is to write code that looks like this:
  //  Target
  //       a.track
  //       b.track
  //       run_if_needed { ... } // depends on a and b
  //

  def run_if_needed(
      f: Producer[A] ?=> Future[A]
  )(using ReadWriter[A]): Future[Result[A]] = {
    run {
      case old_state @ OldState.Current(_) =>
        old_state
      case _ =>
        // We either didn't find a result on disk or we found one with changed dependencies.
        // In either case, we forget the old result and evaluate again
        () => f.apply
    }
  }

}
