package ag.rules

import ag.common.given_ReadWriter_SortedMap

import scala.collection.{SortedMap, SortedSet}

// A directory of bare git repos (e.g. a local mirror of a git server) that
// no target writes: something outside any State keeps it up to date
// (possibly while a run reads it), and State only hands out its repos to
// read (Store.Reads). What targets publish comes back to it that way. Targets see
// it through snapshots of its refs (repos, refs, ref), which are read again
// on every run, and read its contents only at the commits a snapshot names
// (see Context: commit, history, note, and read/list/checkout of a commit's
// tree), so a run sees one consistent state of each repo even while it's
// being updated.
final class Source(val root: os.Path) {
  private val scope = Scope("sources") / root.relativeTo(os.root)

  // A repo's directory: <name>.git, or <name> itself
  def dir(name: String): os.Path = {
    val dotted = root / os.RelPath(s"$name.git")
    if (os.exists(dotted)) dotted else root / os.RelPath(name)
  }

  // the repos' names (their directories, without .git), nested ones included
  lazy val repos: Target[SortedSet[String]] = scope.target(EmptyTuple) {
    (_, _) =>
      def walk(d: os.Path): Seq[os.Path] =
        if (Source.is_repo(d)) Seq(d)
        else
          os.list(d)
            // .names: not repos (e.g. a clone in progress, by whoever updates it)
            .filter(p =>
              os.isDir(p, followLinks = false) && !p.last.startsWith(".")
            )
            .flatMap(walk)
      if (!os.isDir(root)) SortedSet()
      else
        walk(root)
          .filter(_ != root)
          .map(_.relativeTo(root).toString.stripSuffix(".git"))
          .to(SortedSet)
  }

  // A snapshot of repo `name`'s refs (branches, tags, notes, ...): ref ->
  // id. Empty if there's no such repo.
  lazy val refs: String => Target[SortedMap[String, String]] = scope.fun {
    (name: String) =>
      scope.target(EmptyTuple) { (producer, _) =>
        val d = dir(name)
        if (!Source.is_repo(d)) SortedMap[String, String]()
        else producer.state.store_at(Some(d.toString)).refs()
      }
  }

  // One ref of repo `name`, pinned: what targets that read only that ref
  // should depend on, so that other refs moving doesn't rerun them.
  lazy val ref: (String, String) => Target[Option[Commit]] = scope.fun {
    (name: String, ref: String) =>
      scope.target((all = refs(name))) { (_, deps) =>
        deps.all.get(ref).map(id => Commit(dir(name).toString, id))
      }
  }

  // whether there's a repo `name` (even one with no commits)
  lazy val exists: String => Target[Boolean] = scope.fun { (name: String) =>
    scope.target((all = repos))((_, deps) => deps.all.contains(name))
  }

  def master(name: String): Target[Option[Commit]] =
    ref(name, "refs/heads/master")

  // every repo's master: repo -> id (repos with no master left out), as
  // the server's `history` lists them, as of the last fetch
  lazy val masters: Target[SortedMap[String, String]] =
    scope.complex_target[SortedMap[String, String]] { tracker =>
      val names = tracker.track(repos).toSeq
      val ms = tracker.track_all(names.map(master))
      tracker.run_if_needed(_ =>
        (for {
          (n, m) <- names.zip(ms)
          c <- m
        } yield (n, c.id)).to(SortedMap)
      )
    }
}

object Source {
  // never part of a target's name: it's what targets work from, not which
  // they are (see ag.grader.repos.Instance)
  given ag.common.ToRelPath[Source] = ag.common.ToRelPath.Omitted()

  def is_repo(d: os.Path): Boolean =
    os.isFile(d / "HEAD") && os.isDir(d / "objects") && os.isDir(d / "refs")
}
