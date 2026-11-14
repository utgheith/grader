package ag.r3

import upickle.default.{ReadWriter, write}

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

// A keyed definition (see Tracker.run_keyed): a case class whose fields are
// everything its target reads, so its saved value stays good whatever else
// changes. It's stored as keyed/<store>/<a hash of all its fields>: not by
// where its code is, since what it holds can't be worked out again, so
// where it's kept mustn't move when the code does. `store` must never
// change.
trait Keyed[S](store: String)(using rw: ReadWriter[S]) { self: S =>
  // a hash of all its fields
  lazy val key: String =
    MessageDigest
      .getInstance("SHA-256")
      .nn
      .digest(write(self)(using rw).getBytes(UTF_8))
      .nn
      .map(b => f"${b & 0xff}%02x")
      .mkString

  // its target: `f` always gets the saved value, and decides whether to add
  // to it
  protected final def keyed_target[Out: ReadWriter](
      f: (Producer[Out], Option[Result[Out]]) => Some[Result[Out]] | (() => Out)
  ): Target[Out] =
    Target(os.rel / "keyed" / store / key, os.rel / "keyed" / store)(
      _.run_keyed(f)
    )
}
