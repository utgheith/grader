package ag.common

import java.nio.file.Files

// Symbolic links with their targets exactly as written. java.nio normalizes
// a target as it makes a link ("../a/" becomes "../a", "a//b" becomes "a/b";
// reading one keeps it as it is), but git keeps it as written, and what a
// link resolves to can depend on it (a trailing slash: only a directory).
object Links {
  // makes `p` a link to `target`, as written: with ln when java.nio would
  // change it
  def create(p: os.Path, target: String): Unit = {
    val nio = java.nio.file.Path.of(target)
    if (nio.toString == target) {
      val _ = Files.createSymbolicLink(p.toNIO, nio)
    } else { val _ = os.proc("ln", "-s", "--", target, p.toString).call() }
  }
}
