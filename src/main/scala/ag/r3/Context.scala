package ag.r3

import ag.common.Logging
import org.eclipse.jgit.lib.ObjectId

import java.nio.charset.StandardCharsets.UTF_8

// What a target's computation knows about where it runs (as in ag.r2), and
// how it reads data and sources: only through a Tree or Commit, at the id it
// was given.
trait Context[A] {
  val route: Seq[Target[?]]
  val depth: Int
  val state: State
  def producing_opt: Option[Target[A]]

  private def id(s: String): ObjectId = ObjectId.fromString(s).nn

  def read(tree: Tree, path: os.RelPath): Option[Array[Byte]] =
    state.store_at(tree.at).read(id(tree.id), path)
  def read_text(tree: Tree, path: os.RelPath): Option[String] =
    read(tree, path).map(String(_, UTF_8))
  def list(tree: Tree, path: os.RelPath = os.rel): Option[Seq[String]] =
    state.store_at(tree.at).list(id(tree.id), path)
  // a read-only directory with `tree`'s files, for tools that need a path;
  // shared by everyone who reads that tree
  def checkout(tree: Tree): os.Path = state.checkout_tree(tree)
  // `tree`'s files, writable, into `dir` (e.g. one create_data is filling),
  // over what's there
  def copy_to(tree: Tree, dir: os.Path): Unit =
    state.store_at(tree.at).checkout_to(id(tree.id), dir, writable = true)
  // the file at `path` in `tree`, written to `dest` (streamed); false if
  // there's no file there
  def copy_file(tree: Tree, path: os.RelPath, dest: os.Path): Boolean =
    state.store_at(tree.at).copy_file(id(tree.id), path, dest)
  // where the link at `path` in `tree` points, as written; None if it's not
  // a link
  def link_target(tree: Tree, path: os.RelPath): Option[String] =
    state.store_at(tree.at).link_target(id(tree.id), path)
  // what's at `path` in `tree` (a file, link or directory), written to
  // `dest`; false if there's nothing there
  def copy_path(tree: Tree, path: os.RelPath, dest: os.Path): Boolean =
    if (list(tree, path).isDefined)
      id_at(tree, path) match {
        case Some(sub) =>
          copy_to(Tree(sub, tree.at), dest)
          true
        case None => false
      }
    else copy_file(tree, path, dest)

  def commit(c: Commit): CommitInfo =
    state.store_at(Some(c.at)).commit_info(id(c.id))
  // `c` and the commits before it, newest first (see Store.history)
  def history(
      c: Commit,
      first_parent: Boolean = true,
      limit: Int = Int.MaxValue
  ): Seq[CommitInfo] =
    state.store_at(Some(c.at)).history(id(c.id), first_parent, limit)
  // what's at `path` in `tree`, as an id: equal ids, equal contents
  def id_at(tree: Tree, path: os.RelPath): Option[String] =
    state.store_at(tree.at).id_at(id(tree.id), path)
  // the commit `spec` names in `c`'s repo (as rev-parse --verify spec^{commit})
  def resolve(in: Commit, spec: String): Option[Commit] =
    state.store_at(Some(in.at)).resolve(spec).map(Commit(in.at, _))
  // the commits descending from `from` that `to` has (rev-list --ancestry-path)
  def ancestry_path(from: Commit, to: Commit): Seq[Commit] =
    state
      .store_at(Some(to.at))
      .ancestry_path(id(from.id), id(to.id))
      .map(Commit(to.at, _))
  // the note on commit `of`, in notes commit `notes` (e.g. a notes ref's)
  def note(notes: Commit, of: String): Option[String] =
    state
      .store_at(Some(notes.at))
      .note(id(notes.id), id(of))
      .map(String(_, UTF_8))
}

// the Context of the target being computed
trait Producer[A] extends Context[A] with Logging {
  def producing: Target[A]

  // the data it made with create_data or update_data, if it did
  @volatile private[r3] var data: Option[ObjectId] = None
  // its previous data, for update_data
  private[r3] val previous_data: Option[ObjectId]

  lazy val log_path: os.Path = state.log_path(producing)

  override def log(msg: => Any): Unit =
    if (msg != null)
      os.write.append(log_path, msg.toString, createFolders = true)
}
