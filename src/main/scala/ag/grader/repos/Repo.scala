package ag.grader.repos

import ag.grader.{Create, Notice, Publish}
import scala.annotation.unused
import ag.r3.{Commit, Producer, Target, Tracker, WithData, create_data}

// A repo on the git server, one kind per file, each a case class whose
// fields say which repo (see Instance).
//
// Every repo is inspected the same way, from the local mirror. A repo the
// grader publishes (PublishedRepo) says how its contents follow from what
// it depends on, by the policy of who owns it afterwards:
//
//   OwnedRepo        the grader's: it works out the contents
//   AdditiveRepo     added to, never taken from (keys, aliases)
//   HandedOverRepo   made by the grader, then someone else's
//   ReadOnlyRepo     never published (courses_config, students' work)
trait Repo[S] extends Instance[S] { self: S =>
  // its name on the server
  def repo_name: String

  // its master on the mirror, if it has one
  lazy val inspect: Target[Option[Commit]] = env.source.master(repo_name)
}

trait ReadOnlyRepo[S] extends Repo[S] { self: S => }

// What's common to the repos the grader publishes: a policy's target works
// out what the repo should hold, starting from what it holds now (or, if
// it isn't there, from what it would be forked from); the outbox records
// it for `sync` to push.
sealed trait PublishedRepo[S] extends Repo[S] { self: S =>
  // how the server makes it if it isn't there (None: the grader never does)
  def create: Option[Create]
  def message: String

  // The target: `inputs` tracks what the contents depend on; `update`
  // works them out in `dir`, which holds what's there now (or what it'd be
  // forked from), and says who to tell once it's synced.
  protected final def published[I](inputs: Tracker[?] => I)(
      update: (I, Producer[?], os.Path, Boolean) => Seq[Notice]
  )(using sourcecode.FullName): Target[WithData[Publish]] =
    complex_target[WithData[Publish]] { tracker =>
      val exists = tracker.track(env.source.exists(repo_name))
      val base = tracker.track(create.filterNot(_ => exists) match {
        case Some(c) => env.source.master(c.fork_from)
        case None    => inspect
      })
      val in = inputs(tracker)
      tracker.run_if_needed { producer =>
        create_data(producer) { dir =>
          base.foreach(b => producer.copy_to(producer.commit(b).tree, dir))
          val notices = update(in, producer, dir, exists)
          Publish(repo_name, exists, base.map(_.id), message, create, notices)
        }
      }
    }
}

object PublishedRepo {
  // the files in `dir`, but dot files (e.g. .initialized), relative to it
  def files(dir: os.Path): Seq[os.RelPath] =
    os.walk(dir, skip = _.last.startsWith("."))
      .filter(os.isFile(_))
      .map(_.relativeTo(dir))
}

// The grader's: `write` works out the contents. Unless `replaces_all` is
// off, it starts from an empty repo (but its dot files), so a file it no
// longer writes goes.
trait OwnedRepo[S] extends PublishedRepo[S] { self: S =>
  protected def replaces_all: Boolean = true

  protected final def owned[I](inputs: Tracker[?] => I)(
      write: (I, Producer[?], os.Path) => Seq[Notice]
  )(using sourcecode.FullName): Target[WithData[Publish]] =
    published(inputs) { (in, producer, dir, _) =>
      if (replaces_all)
        for (f <- os.list(dir) if !f.last.startsWith(".")) os.remove.all(f)
      write(in, producer, dir)
    }
}

// Added to, never taken from: `add` adds or changes files; it fails if it
// removes one.
trait AdditiveRepo[S] extends PublishedRepo[S] { self: S =>
  protected final def additive[I](inputs: Tracker[?] => I)(
      add: (I, Producer[?], os.Path) => Seq[Notice]
  )(using sourcecode.FullName): Target[WithData[Publish]] =
    published(inputs) { (in, producer, dir, _) =>
      val before = PublishedRepo.files(dir)
      val notices = add(in, producer, dir)
      val gone = before.filterNot(f => os.isFile(dir / f))
      if (gone.nonEmpty)
        throw IllegalStateException(
          s"$repo_name is only added to, but ${gone.map(_.toString).mkString(", ")} would go"
        )
      notices
    }
}

// Made by the grader (`initial` works out what it starts with, on top of
// what it's forked from), then someone else's: once it's there, it's left
// as it is.
trait HandedOverRepo[S] extends PublishedRepo[S] { self: S =>
  // whether it's been handed over, given what it holds now: by default,
  // once it's there
  protected def is_handed_over(
      @unused dir: os.Path,
      exists: Boolean
  ): Boolean =
    exists

  protected final def handed_over[I](inputs: Tracker[?] => I)(
      initial: (I, Producer[?], os.Path) => Seq[Notice]
  )(using sourcecode.FullName): Target[WithData[Publish]] =
    published(inputs) { (in, producer, dir, exists) =>
      if (is_handed_over(dir, exists)) Seq() else initial(in, producer, dir)
    }
}
