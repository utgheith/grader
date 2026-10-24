package ag.events

import munit.FunSuite
import upickle.default.ReadWriter

// test-only events
case class Said(who: String, words: String) extends Event derives ReadWriter {
  def kind: EventKind[Said] = Said.kind
}
object Said { val kind: EventKind[Said] = EventKind[Said]("said") }

// version 1 stored {"name": ...}; version 2 split it into first and last
case class Joined(first: String, last: String) extends Event
    derives ReadWriter {
  def kind: EventKind[Joined] = Joined.kind
}
object Joined {
  val kind: EventKind[Joined] = EventKind[Joined](
    "joined",
    upgrades = Seq { v1 =>
      val Array(first, last) = v1("name").str.split(" ", 2)
      ujson.Obj("first" -> first, "last" -> last)
    }
  )
}

class EventLogTests extends FunSuite {
  private val kinds = EventKinds(Said.kind, Joined.kind)
  private def fresh(): FileEventLog =
    FileEventLog(os.temp.dir(deleteOnExit = true) / "log.jsonl", kinds)

  test("appended events read back in order, at consecutive offsets") {
    val log = fresh()
    assertEquals(log.end, 0L)
    val first = log.append(Seq(Said("a", "hi"), Said("b", "hello")))
    val second = log.append(Seq(Joined("Ada", "Lovelace")))
    assertEquals((first ++ second).map(_.offset), Seq(0L, 1L, 2L))
    assertEquals(log.end, 3L)
    assertEquals(
      log.read().map(_.event).toList,
      List(Said("a", "hi"), Said("b", "hello"), Joined("Ada", "Lovelace"))
    )
    assertEquals(log.read(from = 2).map(_.offset).toList, List(2L))
  }

  test("the log outlives the object that wrote it") {
    val log = fresh()
    val _ = log.append(Seq(Said("a", "hi")))
    val again = FileEventLog(log.file, kinds)
    assertEquals(again.read().map(_.event).toList, List(Said("a", "hi")))
  }

  test("concurrent appenders, each with its own log object, never collide") {
    val log = fresh()
    val tasks = (1 to 8).map { w =>
      ag.task.Task {
        val mine = FileEventLog(log.file, kinds)
        (1 to 25).flatMap(i =>
          mine.append(Seq(Said(s"w$w", s"$i"))).map(_.offset)
        )
      }
    }
    val offsets = tasks.flatMap(_.block)
    assertEquals(offsets.sorted, (0L until 200L).toSeq)
    assertEquals(log.read().size, 200)
    // each writer's own events stay in its order
    for (w <- 1 to 8)
      assertEquals(
        log
          .read()
          .collect {
            case Logged(_, _, Said(who, n)) if who == s"w$w" => n.toInt
          }
          .toList,
        (1 to 25).toList
      )
  }

  test("a line half-written by a crash was never appended") {
    val log = fresh()
    val _ = log.append(Seq(Said("a", "hi")))
    os.write.append(
      log.file,
      """{"offset":1,"at":"2026-10"""
    ) // crashed mid-write
    assertEquals(log.read().size, 1)
    assertEquals(log.end, 1L)
    val next = log.append(Seq(Said("b", "again")))
    assertEquals(next.map(_.offset), Seq(1L), "the partial line was cut off")
    assertEquals(
      log.read().map(_.event).toList,
      List(Said("a", "hi"), Said("b", "again"))
    )
  }

  test("old versions of an event are upgraded when read") {
    val log = fresh()
    os.write(
      log.file,
      """{"offset":0,"at":"2026-01-01T00:00:00Z","kind":"joined","v":1,"data":{"name":"Ada Lovelace"}}""" + "\n"
    )
    assertEquals(
      log.read().map(_.event).toList,
      List(Joined("Ada", "Lovelace"))
    )
    val _ = log.append(Seq(Joined("Grace", "Hopper")))
    assert(
      os.read(log.file).contains(""""kind":"joined","v":2"""),
      "new ones are written as v2"
    )
  }

  test("an unknown kind, or a version newer than the code, is an error") {
    val log = fresh()
    os.write(
      log.file,
      """{"offset":0,"at":"2026-01-01T00:00:00Z","kind":"mystery","v":1,"data":{}}""" + "\n"
    )
    assert(
      intercept[EventLogException](log.read().toList).getMessage
        .contains("unknown event kind")
    )

    os.write.over(
      log.file,
      """{"offset":0,"at":"2026-01-01T00:00:00Z","kind":"said","v":2,"data":{}}""" + "\n"
    )
    assert(
      intercept[EventLogException](log.read().toList).getMessage
        .contains("knows versions 1 to 1")
    )
  }

  test("two kinds can't share a name") {
    val _ = intercept[EventLogException](
      EventKinds(Said.kind, EventKind[Said]("said"))
    )
  }

  test("an append waits while another process holds the log") {
    val log = fresh()
    val _ = log.append(Seq(Said("a", "first")))
    // another process holding the same kind of (fcntl) lock Java uses
    val holder = os
      .proc(
        "python3",
        "-c",
        s"""import fcntl, time
           |f = open("${log.file.toString}", "r+")
           |fcntl.lockf(f, fcntl.LOCK_EX)
           |print("locked", flush=True)
           |time.sleep(0.5)""".stripMargin
      )
      .spawn()
    try {
      assertEquals(holder.stdout.readLine(), "locked")
      val start = System.nanoTime()
      val appended = log.append(Seq(Said("b", "second")))
      val waited_ms = (System.nanoTime() - start) / 1000000
      assert(clue(waited_ms) >= 300, "waited for the other process")
      assertEquals(appended.map(_.offset), Seq(1L))
    } finally holder.destroy()
    assertEquals(
      log.read().map(_.event).toList,
      List(Said("a", "first"), Said("b", "second"))
    )
  }

  test(
    "append_if appends only if nothing was appended since the caller looked"
  ) {
    val log = fresh()
    val seen = log.end // a watcher reads the log...
    val _ =
      log.append(Seq(Said("other", "meanwhile"))) // ...someone else appends...
    assertEquals(
      log.append_if(seen, Seq(Said("me", "stale"))),
      None
    ) // ...so it's refused
    assertEquals(log.read().size, 1)

    val now = log.end // re-read, recompute, retry
    assertEquals(
      log.append_if(now, Seq(Said("me", "fresh"))).map(_.map(_.offset)),
      Some(Seq(1L))
    )
  }

  test("of racing conditional appenders, exactly one wins each round") {
    val log = fresh()
    val start = log.end
    val wins = (1 to 8)
      .map(w =>
        ag.task.Task(
          FileEventLog(log.file, kinds)
            .append_if(start, Seq(Said(s"w$w", "claim")))
        )
      )
      .map(_.block)
    assertEquals(wins.count(_.isDefined), 1)
    assertEquals(log.read().size, 1)
  }
}
