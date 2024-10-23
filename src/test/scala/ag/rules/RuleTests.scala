package ag.rules

import munit.FunSuite

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.{mutable, SortedSet}

import upickle.default.write

class RuleTests extends munit.FunSuite {

  val dir = FunFixture[os.Path](
    setup = { test =>
      val keep = sys.env.contains("KEEP_STATE")
      val d = os.temp.dir(deleteOnExit = !keep)
      if (keep) {
        println(s"keeping state for ${test.name} at $d")
      }
      d
    },
    teardown = { _ => }
  )

  dir.test("t1") { base_dir =>
    val hello: Rule[Unit, Long] = Rule(os.RelPath("x")) {
      42
    }

    val goodbye: Rule[Unit, String] = Rule() {
      "goodbye"
    }

    val nice: Rule[(String, Long), (String, Long)] =
      Rule(goodbye *: hello, null) { (h: String, g: Long) =>
        (h, g)
      }

    {
      val counts = CountingStateMonitor()

      given State = State.of(base_dir, counts)

      assert(clue(nice.value) == ("goodbye", 42))
      assert(clue(hello.value) == 42)
      assert(clue(goodbye.value) == "goodbye")
      assert(clue(counts.lookups.get()) >= 3)
      assert(clue(counts.misses.get()) == 3)
      assert(clue(counts.computes.get()) == 3)

      assert(clue(nice.value) == ("goodbye", 42))
      assert(clue(hello.value) == 42)
      assert(clue(goodbye.value) == "goodbye")
      assert(clue(counts.lookups.get()) >= 3)
      assert(clue(counts.misses.get()) == 3)
      assert(clue(counts.computes.get()) == 3)
    }

    {
      val counts = CountingStateMonitor()

      given State = State.of(base_dir, counts)

      assert(clue(nice.value) == ("goodbye", 42))
      assert(clue(hello.value) == 42)
      assert(clue(goodbye.value) == "goodbye")
      assert(clue(counts.lookups.get()) >= 3)
      assert(clue(counts.misses.get()) == 3)
      assert(clue(counts.computes.get()) == 2)

    }
  }

  dir.test("exceptions") { base_dir =>
    val ex = new Exception("it")
    val a: Rule[Unit, Int] = Rule() {
      throw ex
    }
    val b: Maker[Long] = Rule(a, null) { _ => throw new Exception("b") }
    {
      val counts = CountingStateMonitor()

      given State = State.of(base_dir, counts)

      try {
        val _ = b.value
        assert(false)
      } catch {
        case RuleException(_, chain, cause) =>
          assert(clue(ex) == clue(cause))
          assert(clue(chain) == clue(Seq(b, a)))
      }

      assert(clue(counts.lookups.get) >= 2)
      assert(clue(counts.misses.get) == 2)
      assert(clue(counts.computes.get) == 1)
      assert(clue(counts.saves.get) == 0)
      assert(clue(counts.failures.get) == 2)
      assert(clue(counts.successes.get) == 0)
    }
  }

  dir.test("parallelism") { base_dir =>
    val barrier = AtomicInteger(3)
    val a = Rule() {
      barrier.getAndDecrement()
    }
    val b = Rule() {
      barrier.getAndDecrement()
    }
    val c = Rule() {
      barrier.getAndDecrement()
    }

    val d: Maker[Int] = Rule(a *: b *: c, null)(_ + _ + _)
    {
      val thread_names: mutable.Set[String] = mutable.Set[String]()
      val counts: CountingStateMonitor = new CountingStateMonitor() {
        override def onStart(s: State, rule: RuleBase): Unit = {
          super.onStart(s, rule)
          val tn = Thread.currentThread().nn.getName.nn
          thread_names.synchronized {
            val _ = thread_names.add(tn)
          }
        }
      }

      given State = State.of(base_dir, counts)

      assert(clue(d.value) == 6)
      assert(clue(thread_names.size) == 4)
      assert(clue(counts.lookups.get) >= 4)
      assert(clue(counts.successes.get) == 4)
      assert(clue(counts.starts.get) == 4)
      assert(clue(counts.computes.get) == 4)
      assert(clue(counts.saves.get) == 4)
      assert(clue(counts.starts.get) == 4)
    }
  }

  dir.test("tuples") { base_dir =>
    val a = Rule() {
      10
    }

    val b = Rule() {
      "hello"
    }

    given State = State.of(base_dir)

    val v: Task[(Int, String)] = Task.sequence((a.make, b.make))
    assert(clue(v.block) == (10, "hello"))
  }

  dir.test("paths") { base_dir =>
    val a = Rule() {
      10
    }
    def b(x: String): Rule[Int, (Int, String)] = Rule(a, os.RelPath(x)) { a =>
      (a, x)
    }

    given State = State.of(base_dir)

    assert(clue(a.value) == 10)
    val bb = b("things")
    assert(clue(bb.value) == (10, "things"))

    val targets = base_dir / "targets"

    val found = (for {
      p <- os.walk(targets)
      if os.isFile(p)
    } yield p.relativeTo(targets)).sorted

    val expected = (for {
      p <- Seq("a", "b/things")
    } yield os.RelPath("ag/rules/RuleTests") / os.RelPath(p)).sorted

    assert(clue(found) == clue(expected))
  }

  dir.test("parts") { base_dir =>
    given State = State.of(base_dir)
    val a: Maker[String] = Rule() { "this is a" }
    val b: Maker[Int] = Rule() { 0xb }
    val c: Maker[Char] = Rule(a *: b, null) { _ => 'c' }

    val s = a *: b *: c

    assert(clue(s.parts.block).toSeq == Seq(a, b, c))
  }

  dir.test("signed_path") { base_dir =>
    val a = Rule() { 10 }
    val b = Rule() { "10" }
    val sp = SignedPath.rule(a *: b, SortedSet(), null) { (path, in) =>
      os.write(path / "f", write(in), createFolders = true)
      in
    }

    given State = State.of(base_dir)

    assert(clue(sp.value.data) == (10, "10"))
  }

  dir.test("Make.*:") { base_dir =>
    val a = Rule() { 1 }
    val b = Rule() { '2' }
    val c = Rule() { "3" }
    val x: Maker[(Int, Char, String)] = a *: b *: c

    given State = State.of(base_dir)

    println(x.value)
  }

}
