package ag.grader.main.misc

import ag.grader.{Outbox, RemoteServer}
import ag.grader.gitolite.Publish
import ag.rules.WithData

import scala.util.Try

// What a command publishes: recorded in the workspace's outbox and, if
// config.json's can_push_repo allows it (`server`), pushed at once, by the
// thread that records it, so students see their results as soon as there
// are some; else it waits in the outbox for `sync`. What must go out in
// order (what a repo's contents depend on, a repo before its other
// branches) is recorded in order, by one thread; the outbox does one thing
// at a time per repo.
//
// The first push that fails ends the round: nothing more is pushed (what's
// recorded from then on stays in the outbox, for the next round), and test
// runs not yet started don't start (see targets.stop).
private[main] final class Publishing(
    val outbox: Outbox,
    server: Option[RemoteServer],
    can_send_mail: Boolean
) {
  // the keys recorded (see Publish.key)
  private val recorded =
    java.util.concurrent.ConcurrentHashMap.newKeySet[String]().nn

  // the first push that failed
  private val failure =
    java.util.concurrent.atomic.AtomicReference[Option[Throwable]](None)
  def failed: Option[Throwable] = failure.get

  // pushes what's pending for `key`, unless a push has failed
  private def push(s: RemoteServer, key: String): Unit =
    if (failure.get.isEmpty)
      try outbox.push_one(s, can_send_mail, println, key)
      catch {
        case e: Throwable =>
          if (failure.compareAndSet(None, Some(e))) {
            println(s"$key: not pushed; ending the round: ${e.toString}")
            ag.grader.targets.stop(e)
          }
      }

  // Records `p` (see Outbox.record), and pushes it: whether there was
  // something to push. Once per key: the mirror isn't updated after a push,
  // so a second record would be on an out-of-date base (and its push would
  // fail); it's the same anyway (a State works out each target once).
  def record(p: WithData[Publish]): Boolean =
    recorded.add(p.value.key) && {
      val todo = outbox.record(p)
      if (todo) server.foreach(push(_, p.value.key))
      todo
    }

  // how many keys have been recorded
  def count: Int = recorded.size

  // What's pending from earlier (a round that ended at a failed push) that
  // this one didn't record: pushed, unless the server's moved on since it
  // was worked out (`refresh` brings a repo's mirror up to date first);
  // then it's dropped, to be worked out again when it's next asked for.
  private[misc] def leftovers(refresh: String => Unit): Unit =
    for (s <- server)
      for (key <- outbox.pending if !recorded.contains(key)) {
        refresh(outbox.publish_of(key).repo)
        if (outbox.is_stale(key)) {
          println(s"$key: dropped from the outbox: the server's moved on")
          outbox.drop(key)
        } else push(s, key)
      }
}

// Runs `f` with what publishes in `workspace`, then pushes what's left in
// the outbox from earlier rounds (see Publishing.leftovers). A failed push
// fails it with the push's error (why the rest of `f` may have failed: see
// Publishing); what's unpushed stays in the outbox.
private[main] def publishing[A](workspace: os.Path)(f: Publishing => A): A = {
  val ob = outbox_of(workspace)
  val server =
    if (ag.grader.Config.pushes_allowed) Some(ag.grader.Config.gitolite_server)
    else {
      println(
        "not pushing (config.json's can_push_repo): what's recorded waits in the outbox"
      )
      None
    }
  val pub = Publishing(ob, server, ag.grader.Config.mail_allowed)
  val out = Try {
    val a = f(pub)
    val mirror = ag.grader.gitolite.LocalMirror
      .of(ag.grader.Config.gitolite_server, mirror_root(workspace), println)
    pub.leftovers(mirror.refresh)
    a
  }
  println(s"${ob.pending.size} pending in the outbox")
  pub.failed.orElse(out.failed.toOption).foreach(e => throw e)
  out.get
}
