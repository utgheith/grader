package ag.r3

import ag.common.given_ReadWriter_SortedMap

import scala.collection.{SortedMap, SortedSet}

// A directory of bare git repos (e.g. a local mirror of a git server) that
// r3 reads and never writes: something else keeps it up to date. Targets see
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

  def master(name: String): Target[Option[Commit]] =
    ref(name, "refs/heads/master")
}

object Source {
  def is_repo(d: os.Path): Boolean =
    os.isFile(d / "HEAD") && os.isDir(d / "objects") && os.isDir(d / "refs")
}
