package ag.rules

import scala.collection.SortedSet
import upickle.default.ReadWriter

// Using an absolute path means that we can't copy workspaces around, remotely sync them, etc.
// making path relative is workable but complicates the API.
// TODO: figure out a convenient way of making this relative
case class SignedPath[D](
    path: os.Path,
    signature: Signature,
    ignore_last: SortedSet[String],
    data: D
) derives ReadWriter {}

object SignedPath {

  def rule[In, D: ReadWriter](
      in: Maker[In],
      ignore_last: SortedSet[String],
      rest: os.RelPath | Null
  )(f: (os.Path, In) => D)(using
      fn: sourcecode.FullName
  ): Rule[In, SignedPath[D]] = new Rule[In, SignedPath[D]] {
    override val path: os.RelPath =
      val first: os.RelPath = os.RelPath(fn.value.replace('.', '/').nn)
      if (rest == null) first else first / rest

    override def needs: Maker[In] = in

    override def compute(
        context: Context[SignedPath[D]],
        in: In
    ): SignedPath[D] = {
      val base_dir = context.state.signed_path_dir / context.rule.path
      val dirty_file = base_dir / os.up / s"__${base_dir.last}__is__dirty"

      if (os.exists(dirty_file)) {
        say(s"Signed Path ${context.rule.path} is dirty, removing")
        os.remove.all(base_dir)
        os.remove.all(dirty_file)
      }

      os.write(dirty_file, "", createFolders = true)

      // Failures will leave the dirty file in place. This is good  because:
      //    (1) next attempt to evaluate the target will remove the directory
      //        then construct from scratch
      //    (2) humans can look at the dirty directory for debugging
      val d = f(base_dir, in)

      os.remove.all(dirty_file)
      SignedPath(
        base_dir,
        Signature.of(SortedSet(base_dir), ignore_last),
        ignore_last,
        d
      )
    }
  }
}
