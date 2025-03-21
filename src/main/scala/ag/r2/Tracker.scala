package ag.r2

import ag.common.{Signature, Signer, block}
import upickle.default.{ReadWriter, read, write}

import scala.annotation.implicitNotFound
import scala.collection.SortedMap
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.reflect.ClassTag
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
      f: Producer[A] ?=> Option[Result[A]] => Some[Result[A]] |
        (() => Future[A])
  )(using ReadWriter[A], ClassTag[A]): Future[Result[A]] = {
    given producer: Producer[A] = new Producer[A] {
      override val depth: Int = Tracker.this.depth
      override val producing: Target[A] = Tracker.this.producing_opt.get
      override val state: State = Tracker.this.state

      override def producing_opt: Option[Target[A]] = Tracker.this.producing_opt

      override def execute(runnable: Runnable): Unit = state.execute(runnable)

      override def reportFailure(cause: Throwable): Unit =
        state.reportFailure(cause)
    }

    // check if we have a failed update
    if (os.exists(producer.backup_path)) {
      say(s"recovering ${producer.target_path} from backup")
      os.remove.all(producer.target_path)
      os.move(producer.backup_path, producer.target_path)
    }

    val old_state: Option[Result[A]] = {

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
            Some(old.result)
          } else {
            say("dependencies changed for old state")
            None
          }
        } catch {
          case NonFatal(e) =>
            say(s"load error $e")
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
        Future.successful(ra)
      case ffa: (() => Future[A]) =>
        // remove the old result
        // say("removing old result")
        // os.remove.all(producer.target_path)

        // Make a backup copy of the old result. This is useful if the computation fails
        // and we want to restore the old result
        if (os.exists(producer.target_path)) {
          say(s"backing up ${producer.producing.path}")
          os.copy(
            from = producer.target_path,
            to = producer.backup_path,
            createFolders = true,
            followLinks = false
          )
        }

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

          os.remove.all(producer.backup_path)
          say(s"${producer.producing.path} done")
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
  )(using ReadWriter[A], ClassTag[A]): Future[Result[A]] = {
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
