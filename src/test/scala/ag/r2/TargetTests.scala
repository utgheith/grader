package ag.r2

import ag.common.Fork
import org.scalatest.*
import flatspec.*
import matchers.*

object Things extends Scope(".") {
  val a: Target[WithData[Int]] = target() {
    run_if_needed {
      create_data(_ => false) { path =>
        os.write(path / "xyz", "hello\n")
        10
      }
    }
  }

  val b: (String, Int) => Target[Int] = fun { (s, i) =>
    target(a) { a =>
      assert (s != "")
      //println(s"running $s $i, a:${a.toString}")
      a.value + i
    }
  }
}

class TargetTests extends AnyFlatSpec with should.Matchers {

    "basic tracking" should "work" in {
      def doit(d: Int): Unit = {
        given State = State(config.get_test_dir)

        Noise.noise = true

        val a = Things.a.track
        val b = Things.b("thing", d).track
        val c = Fork {
          val _ = a.join.value should be(10)
          val _ = b.join should be(10 + d)
          a.join.value + b.join
        }
        val _ = c.join should be(20 + d)

      }

      doit(6)
      doit(4)
      doit(6)
    }

}
