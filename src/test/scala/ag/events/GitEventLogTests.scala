package ag.events

import munit.FunSuite

class GitEventLogTests extends FunSuite {
  private val kinds = EventKinds(Said.kind, Joined.kind)

  // a bare repository standing in for the git server, and a way to get
  // "machines" (separate checkouts) using it
  private final class Server {
    val dir: os.Path = os.temp.dir(deleteOnExit = true)
    val bare: os.Path = dir / "course__private.git"
    val _ = os.proc("git", "init", "-q", "--bare", "-b", "main", bare).call()
    private var n = 0
    def machine(): GitEventLog = synchronized {
      n += 1
      GitEventLog(bare.toString, dir / s"machine$n", kinds)
    }
    def commits: Int =
      os.proc("git", "rev-list", "--count", "main")
        .call(cwd = bare, check = false)
        .out
        .text()
        .trim
        .toIntOption
        .getOrElse(0)
  }

  test("what one machine appends, another reads, at consecutive offsets") {
    val server = Server()
    val a = server.machine()
    val b = server.machine()
    assertEquals(b.end, 0L)
    val _ = a.append(Seq(Said("a", "hi"), Said("a", "there")))
    val _ = b.append(Seq(Joined("Ada", "Lovelace")))
    assertEquals(
      a.read().map(l => (l.offset, l.event)).toList,
      List(
        0L -> Said("a", "hi"),
        1L -> Said("a", "there"),
        2L -> Joined("Ada", "Lovelace")
      )
    )
    assertEquals(b.read(from = 1).size, 2)
  }

  test("each append is one commit, so history is the log's") {
    val server = Server()
    val a = server.machine()
    val _ = a.append(Seq(Said("a", "1"), Said("a", "2")))
    val _ = a.append(Seq(Said("a", "3")))
    assertEquals(server.commits, 2)
    // going back: the first commit is the state after the first append
    val old = server.dir / "old"
    val _ = os.proc("git", "clone", "-q", server.bare, old).call()
    val _ = os.proc("git", "checkout", "-q", "HEAD~1").call(cwd = old)
    assertEquals(os.walk(old / "events").count(_.ext == "json"), 2)
  }

  test("a machine that's behind renumbers its append and retries") {
    val server = Server()
    val a = server.machine()
    val b = server.machine()
    assertEquals(b.end, 0L) // b's checkout now knows of nothing
    val _ = a.append(Seq(Said("a", "first")))
    val mine = b.append(Seq(Said("b", "second")))
    assertEquals(mine.map(_.offset), Seq(1L))
    assertEquals(
      a.read().map(_.event).toList,
      List(Said("a", "first"), Said("b", "second"))
    )
  }

  test("append_if: a stale writer is refused") {
    val server = Server()
    val a = server.machine()
    val b = server.machine()
    val seen = b.end
    val _ = a.append(Seq(Said("a", "meanwhile")))
    assertEquals(b.append_if(seen, Seq(Said("b", "stale"))), None)
    assertEquals(
      b.append_if(b.end, Seq(Said("b", "fresh"))).map(_.map(_.offset)),
      Some(Seq(1L))
    )
  }

  test("racing machines: every append lands once, at unique offsets") {
    val server = Server()
    val machines = (1 to 4).map(_ => server.machine())
    val offsets = machines.zipWithIndex
      .map((m, w) =>
        ag.task.Task(
          (1 to 5)
            .flatMap(i => m.append(Seq(Said(s"w$w", s"$i"))).map(_.offset))
        )
      )
      .flatMap(_.block)
    assertEquals(offsets.sorted, (0L until 20L).toSeq)
    assertEquals(server.machine().read().size, 20)
  }

  test("racing conditional appends: exactly one wins") {
    val server = Server()
    val machines = (1 to 4).map(_ => server.machine())
    val wins = machines.zipWithIndex
      .map((m, w) => ag.task.Task(m.append_if(0, Seq(Said(s"w$w", "claim")))))
      .map(_.block)
    assertEquals(wins.count(_.isDefined), 1)
    assertEquals(server.machine().read().size, 1)
  }

  test("an unreachable server is an error, not an empty log") {
    val log = GitEventLog(
      "/nonexistent/repo.git",
      os.temp.dir(deleteOnExit = true) / "c",
      kinds
    )
    val _ = intercept[EventLogException](log.append(Seq(Said("a", "hi"))))
  }
}
