package ag.common

import munit.FunSuite

class RunLockTests extends FunSuite {
  test("a second run is refused, and told who holds the lock") {
    val dir = os.temp.dir(deleteOnExit = true)
    val first = RunLock.acquire(dir, "run publish_results -p p5")
    assert(first.isRight)

    RunLock.acquire(dir, "run gen_html") match {
      case Left(holder) =>
        assert(clue(holder).contains("run publish_results -p p5"))
        assert(holder.contains(s"pid ${ProcessHandle.current().nn.pid()}"))
      case Right(_) => fail("acquired a lock that's already held")
    }

    first.foreach(release => release())
    val again = RunLock.acquire(dir, "run gen_html")
    assert(again.isRight, "free again once released")
    again.foreach(release => release())
  }

  test("a lock held by another process is respected") {
    val dir = os.temp.dir(deleteOnExit = true)
    val lock_file = (dir / ".grader.lock").toString
    // a separate process holding the same kind of (fcntl) lock Java uses
    val holder = os
      .proc(
        "python3",
        "-c",
        s"""import fcntl, sys, time
           |f = open("$lock_file", "w")
           |f.write("pid 4242, started earlier: run publish_results"); f.flush()
           |fcntl.lockf(f, fcntl.LOCK_EX)
           |print("locked", flush=True)
           |time.sleep(30)""".stripMargin
      )
      .spawn()
    try {
      assertEquals(holder.stdout.readLine(), "locked")
      RunLock.acquire(dir, "run gen_html") match {
        case Left(who)      => assert(clue(who).contains("pid 4242"))
        case Right(release) =>
          release()
          fail("acquired a lock another process holds")
      }
    } finally holder.destroy()
    val _ = holder.waitFor()
    val after = RunLock.acquire(dir, "run gen_html")
    assert(after.isRight, "free once that process is gone")
    after.foreach(release => release())
  }
}
