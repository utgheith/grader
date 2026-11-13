package ag.r3

import org.eclipse.jgit.internal.storage.file.FileRepository
import org.eclipse.jgit.lib.{
  CommitBuilder,
  Constants,
  FileMode,
  ObjectId,
  ObjectInserter,
  ObjectReader,
  PersonIdent,
  RefUpdate,
  Repository,
  TreeFormatter
}
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.treewalk.TreeWalk

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.time.Instant
import java.util.UUID
import scala.collection.SortedMap
import scala.jdk.CollectionConverters.*
import scala.util.Using

// The bare git repo a State keeps its targets in. Each target has a ref,
// Store.ref_name(path), at a commit whose tree is
//
//   result/        its signature: what dependents see
//     value.json   the value
//     data/        the data it made, if any (see create_data)
//   deps.json      what it depended on: path -> signature
//
// and whose parent is the target's previous commit: its history. A run that
// changes neither the result nor the dependencies makes no commit.
//
// With `create = false` it's a repo r3 only reads: one in a Source.
final class Store(val dir: os.Path, create: Boolean = true) {
  import Store.*

  val repo: Repository = {
    if (!create && !os.isFile(dir / "HEAD"))
      throw IllegalArgumentException(s"no git repo at ${dir.toString}")
    val r = new FileRepository(dir.toIO)
    if (create && !os.exists(dir / "HEAD")) r.create(true)
    r
  }

  // ---- reads, for a Source's repos

  // every ref under refs/ (branches, tags, notes, ...) and its id
  def refs(): SortedMap[String, String] =
    repo.getRefDatabase.nn
      .getRefsByPrefix("refs/")
      .nn
      .asScala
      .toSeq
      .flatMap(r => Option(r.getObjectId).map(id => (r.getName.nn, id.name.nn)))
      .to(SortedMap)

  def commit_info(id: ObjectId): CommitInfo =
    Using.resource(new RevWalk(repo))(w => info(w.parseCommit(id).nn))

  // `id` and the commits before it, newest first: along first parents only,
  // or every ancestor in commit-time order
  def history(
      id: ObjectId,
      first_parent: Boolean,
      limit: Int
  ): Seq[CommitInfo] =
    Using.resource(new RevWalk(repo)) { w =>
      w.setFirstParent(first_parent)
      // first parents only: the chain, in its own order
      if (!first_parent)
        w.sort(org.eclipse.jgit.revwalk.RevSort.COMMIT_TIME_DESC)
      w.markStart(w.parseCommit(id))
      w.iterator().nn.asScala.take(limit).map(info).toSeq
    }

  // the note on `of` in notes commit `notes`, if there is one
  def note(notes: ObjectId, of: ObjectId): Option[Array[Byte]] =
    Using.resource(repo.newObjectReader().nn) { reader =>
      val commit = Using.resource(new RevWalk(reader))(_.parseCommit(notes).nn)
      Option(org.eclipse.jgit.notes.NoteMap.read(reader, commit).nn.get(of))
        .map(id => reader.open(id).nn.getBytes.nn)
    }

  // the commit `spec` names (as `git rev-parse --verify spec^{commit}`), if any
  def resolve(spec: String): Option[String] =
    scala.util
      .Try(Option(repo.resolve(s"$spec^{commit}")))
      .toOption
      .flatten
      .map(_.name.nn)

  // the commits that are descendants of `from` and ancestors of `to` (as
  // `git rev-list --ancestry-path from..to`)
  def ancestry_path(from: ObjectId, to: ObjectId): Seq[String] =
    Using.resource(new RevWalk(repo)) { w =>
      w.markStart(w.parseCommit(to))
      w.markUninteresting(w.parseCommit(from))
      val candidates = w.iterator().nn.asScala.map(_.copy()).toSeq
      candidates
        .filter(c =>
          Using.resource(new RevWalk(repo))(w2 =>
            w2.isMergedInto(w2.parseCommit(from), w2.parseCommit(c))
          )
        )
        .map(_.name.nn)
    }

  private def info(c: org.eclipse.jgit.revwalk.RevCommit): CommitInfo = {
    val who = c.getCommitterIdent.nn
    CommitInfo(
      id = c.name.nn,
      parents = c.getParents.nn.toSeq.map(_.nn.name.nn),
      committed = who.getWhenAsInstant.nn
        .atOffset(who.getZoneOffset)
        .nn
        .format(Store.git_time)
        .nn,
      message = c.getFullMessage.nn,
      tree = Tree(c.getTree.nn.name.nn, Some(dir.toString))
    )
  }

  // what a target's ref holds
  final case class Loaded(
      commit: ObjectId,
      result: ObjectId,
      value: Array[Byte],
      deps: Array[Byte],
      data: Option[ObjectId]
  )

  def load(path: os.RelPath): Option[Loaded] =
    Option(repo.exactRef(ref_name(path)))
      .flatMap(r => Option(r.getObjectId))
      .map { commit =>
        Using.resource(repo.newObjectReader().nn) { reader =>
          val top = Using.resource(new RevWalk(reader))(
            _.parseCommit(commit).nn.getTree.nn.getId.nn
          )
          def entry(tree: ObjectId, name: String): Option[ObjectId] =
            Option(TreeWalk.forPath(reader, name, tree))
              .map(_.getObjectId(0).nn)
          def bytes(id: ObjectId): Array[Byte] = reader.open(id).nn.getBytes.nn
          val result = entry(top, "result").getOrElse(
            throw IllegalStateException(s"${ref_name(path)} has no result")
          )
          Loaded(
            commit = commit,
            result = result,
            value = bytes(
              entry(result, "value.json").getOrElse(
                throw IllegalStateException(s"${ref_name(path)} has no value")
              )
            ),
            deps = entry(top, "deps.json").map(bytes).getOrElse(Array()),
            data = entry(result, "data")
          )
        }
      }

  // Saves a target's new state, on top of `old` (what load found, which must
  // still be there: two writers of one target fail rather than lose a run).
  // Returns the result's id: the target's signature.
  def save(
      path: os.RelPath,
      old: Option[ObjectId],
      value: Array[Byte],
      deps: Array[Byte],
      data: Option[ObjectId],
      message: String
  ): ObjectId = Using.resource(repo.newObjectInserter().nn) { ins =>
    val result = tree(
      ins,
      Seq(("value.json", FileMode.REGULAR_FILE, blob(ins, value))) ++
        data.map(("data", FileMode.TREE, _))
    )
    val top = tree(
      ins,
      Seq(
        ("result", FileMode.TREE, result),
        ("deps.json", FileMode.REGULAR_FILE, blob(ins, deps))
      )
    )
    val unchanged = old.exists { c =>
      Using.resource(new RevWalk(repo))(
        _.parseCommit(c).nn.getTree.nn.getId
      ) == top
    }
    if (!unchanged) {
      val who = PersonIdent(
        "r3",
        "r3@localhost",
        Instant.now(),
        java.time.ZoneOffset.UTC
      )
      val cb = CommitBuilder()
      cb.setTreeId(top)
      old.foreach(cb.setParentId)
      cb.setAuthor(who)
      cb.setCommitter(who)
      cb.setMessage(message)
      val commit = ins.insert(cb).nn
      ins.flush()
      val ru = repo.updateRef(ref_name(path)).nn
      ru.setNewObjectId(commit)
      ru.setExpectedOldObjectId(old.getOrElse(ObjectId.zeroId()))
      ru.setRefLogMessage(message, false)
      Using.resource(new RevWalk(repo))(ru.update(_)) match {
        case RefUpdate.Result.NEW | RefUpdate.Result.FAST_FORWARD => ()
        case other                                                =>
          throw IllegalStateException(
            s"can't save ${path.toString}: ${String.valueOf(other)}"
          )
      }
    } else ins.flush()
    result
  }

  // `dir`'s contents as a tree, empty directories included. A git repo
  // (.git, or a bare one) is refused: a target's data is files, not repos.
  def import_dir(dir: os.Path): ObjectId =
    Using.resource(repo.newObjectInserter().nn) { ins =>
      val t = import_tree(ins, dir, dir)
      ins.flush()
      t
    }

  private def import_tree(
      ins: ObjectInserter,
      base: os.Path,
      dir: os.Path
  ): ObjectId = {
    if (is_bare_repo(dir))
      throw IllegalArgumentException(
        s"${dir.relativeTo(base).toString}: a git repo can't be a target's data"
      )
    val entries = os.list(dir).map { p =>
      val name = p.last
      if (name == ".git")
        throw IllegalArgumentException(
          s"${p.relativeTo(base).toString}: a git repo can't be a target's data"
        )
      if (os.isLink(p))
        (
          name,
          FileMode.SYMLINK,
          blob(ins, Files.readSymbolicLink(p.toNIO).toString.getBytes(UTF_8))
        )
      else if (os.isDir(p)) (name, FileMode.TREE, import_tree(ins, base, p))
      else if (os.isFile(p))
        (
          name,
          if (Files.isExecutable(p.toNIO)) FileMode.EXECUTABLE_FILE
          else FileMode.REGULAR_FILE,
          Using.resource(os.read.inputStream(p))(in =>
            ins.insert(Constants.OBJ_BLOB, os.size(p), in).nn
          )
        )
      else
        throw IllegalArgumentException(
          s"${p.relativeTo(base).toString}: not a file, directory or link"
        )
    }
    tree(ins, entries)
  }

  // `path` in `tree`, if it's a file (or a link: its target)
  // the id of what's at `path` in `tree`, if anything: the same id, the same
  // contents
  def id_at(tree: ObjectId, path: os.RelPath): Option[String] =
    Using.resource(repo.newObjectReader().nn)(reader =>
      walk_to(reader, tree, path).map(_._1.name.nn)
    )

  def read(tree: ObjectId, path: os.RelPath): Option[Array[Byte]] =
    Using.resource(repo.newObjectReader().nn) { reader =>
      walk_to(reader, tree, path).collect {
        case (id, mode) if mode != FileMode.TREE =>
          reader.open(id).nn.getBytes.nn
      }
    }

  // where the link at `path` in `tree` points, as written; None if there's
  // no link there
  def link_target(tree: ObjectId, path: os.RelPath): Option[String] =
    Using.resource(repo.newObjectReader().nn) { reader =>
      walk_to(reader, tree, path).collect {
        case (id, mode) if mode == FileMode.SYMLINK =>
          String(reader.open(id).nn.getBytes.nn, UTF_8)
      }
    }

  // The file (or link) at `path` in `tree`, written to `dest` (streamed: it
  // may be bigger than memory); false if there's no file there
  def copy_file(tree: ObjectId, path: os.RelPath, dest: os.Path): Boolean =
    Using.resource(repo.newObjectReader().nn) { reader =>
      walk_to(reader, tree, path) match {
        case Some((id, mode)) if mode == FileMode.SYMLINK =>
          os.remove.all(dest)
          val target = String(reader.open(id).nn.getBytes.nn, UTF_8)
          ag.common.Links.create(dest, target)
          true
        case Some((id, mode))
            if mode != FileMode.TREE && mode != FileMode.GITLINK =>
          os.remove.all(dest)
          Using.resource(reader.open(id).nn.openStream().nn)(in =>
            os.write(dest, in, createFolders = true)
          )
          if (mode == FileMode.EXECUTABLE_FILE)
            os.perms.set(dest, os.PermSet.fromString("rwxr-xr-x"))
          true
        case _ => false
      }
    }

  // the names in directory `path` of `tree`, if it's a directory
  def list(tree: ObjectId, path: os.RelPath): Option[Seq[String]] =
    Using.resource(repo.newObjectReader().nn) { reader =>
      walk_to(reader, tree, path).collect {
        case (id, mode) if mode == FileMode.TREE =>
          val tw = new TreeWalk(reader)
          tw.addTree(id)
          val names = Seq.newBuilder[String]
          while (tw.next()) names += tw.getNameString.nn
          names.result()
      }
    }

  private def walk_to(
      reader: ObjectReader,
      tree: ObjectId,
      path: os.RelPath
  ): Option[(ObjectId, FileMode)] =
    if (path.segments.isEmpty) Some((tree, FileMode.TREE))
    else
      Option(TreeWalk.forPath(reader, path.toString, tree))
        .map(tw => (tw.getObjectId(0).nn, tw.getFileMode(0).nn))

  // Writes `tree` to `dest`, which mustn't exist; files are read-only unless
  // `writable`.
  // `tree`'s files into `dest`, over what's there (a file or link in the way
  // is replaced; directories are merged). A submodule becomes an empty
  // directory, as git checks it out.
  def checkout_to(tree: ObjectId, dest: os.Path, writable: Boolean): Unit =
    Using.resource(repo.newObjectReader().nn) { reader =>
      os.makeDir.all(dest)
      val tw = new TreeWalk(reader)
      tw.addTree(tree)
      while (tw.next()) {
        val p = dest / os.RelPath(tw.getPathString.nn)
        val mode = tw.getFileMode(0).nn
        val dir = mode == FileMode.TREE || mode == FileMode.GITLINK
        if (
          os.exists(p, followLinks = false) && (!dir || !os.isDir(
            p,
            followLinks = false
          ))
        )
          os.remove.all(p)
        if (dir) {
          os.makeDir.all(p)
          if (mode == FileMode.TREE) tw.enterSubtree()
        } else if (mode == FileMode.SYMLINK) {
          val target =
            String(reader.open(tw.getObjectId(0)).nn.getBytes.nn, UTF_8)
          ag.common.Links.create(p, target)
        } else {
          Using.resource(reader.open(tw.getObjectId(0)).nn.openStream().nn)(
            in => os.write(p, in)
          )
          val exec = mode == FileMode.EXECUTABLE_FILE
          os.perms.set(
            p,
            os.PermSet.fromString((writable, exec) match {
              case (true, true)   => "rwxr-xr-x"
              case (true, false)  => "rw-r--r--"
              case (false, true)  => "r-xr-xr-x"
              case (false, false) => "r--r--r--"
            })
          )
        }
      }
    }

  // A shared, read-only checkout of `tree` under `root`, made once: a tree
  // never changes, so neither does its checkout.
  def checkout(tree: ObjectId, root: os.Path): os.Path = {
    val dest = root / tree.name.nn
    if (!os.exists(dest)) {
      val tmp = root / s".tmp-${UUID.randomUUID().toString}"
      checkout_to(tree, tmp, writable = false)
      try os.move(tmp, dest, atomicMove = true)
      catch {
        // another reader got there first (which the move may report as just
        // a FileSystemException: "Directory not empty")
        case _: java.nio.file.FileSystemException if os.exists(dest) =>
          os.remove.all(tmp)
      }
    }
    dest
  }
}

object Store {
  // as git's %cI
  val git_time: java.time.format.DateTimeFormatter =
    java.time.format.DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ssxxx").nn

  def blob(ins: ObjectInserter, bytes: Array[Byte]): ObjectId =
    ins.insert(Constants.OBJ_BLOB, bytes).nn

  // A tree of `entries`, in git's order: by name, a tree's as if it ended in /
  def tree(
      ins: ObjectInserter,
      entries: Seq[(String, FileMode, ObjectId)]
  ): ObjectId = {
    def key(name: String, mode: FileMode): Array[Byte] =
      (if (mode == FileMode.TREE) s"$name/" else name).getBytes(UTF_8)
    val f = TreeFormatter()
    entries
      .sortWith((a, b) =>
        java.util.Arrays.compareUnsigned(key(a._1, a._2), key(b._1, b._2)) < 0
      )
      .foreach((name, mode, id) => f.append(name, mode, id))
    ins.insert(f).nn
  }

  private def is_bare_repo(dir: os.Path): Boolean =
    os.isFile(dir / "HEAD") && os.isDir(dir / "objects") && os.isDir(
      dir / "refs"
    )

  // characters a ref name component can hold as they are
  private val plain: Set[Char] =
    (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "-_.+=,").toSet

  // One component of a target's path, as a valid ref name component: other
  // bytes (and dots git won't take) become %XX.
  def encode(component: String): String = {
    val bytes = component.getBytes(UTF_8)
    val out = StringBuilder()
    for (i <- bytes.indices) {
      val c = (bytes(i) & 0xff).toChar
      val ok = plain(c) && !(c == '.' && (i == 0 || bytes(i - 1) == '.'))
      if (ok) out += c else out ++= f"%%${bytes(i) & 0xff}%02X"
    }
    val s = out.result()
    if (s.endsWith(".lock")) s.dropRight(5) + "%2Elock" else s
  }

  // A target's ref: its path's components, then "@", which no encoded
  // component is, so that a and a/b can both have refs.
  def ref_name(path: os.RelPath): String = {
    require(path.ups == 0, s"a target's path can't go up: ${path.toString}")
    (Seq("refs", "r3") ++ path.segments.map(encode) :+ "@").mkString("/")
  }

  // the same, as a relative path, for files kept beside the repo
  def rel_path(path: os.RelPath): os.RelPath =
    path.segments.map(encode).foldLeft(os.rel)(_ / _) / "@"
}
