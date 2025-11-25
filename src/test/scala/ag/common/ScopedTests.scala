package ag.common

import munit.FunSuite

class ScopedTests extends FunSuite {

  test("Scoped returns default value") {
    val scoped = Scoped("default")
    assertEquals(scoped.get, "default")
  }

  test("Scoped sets value within scope") {
    val scoped = Scoped("default")
    scoped.set("new") {
      assertEquals(scoped.get, "new")
    }
    assertEquals(scoped.get, "default")
  }

  test("Scoped handles nested scopes") {
    val scoped = Scoped(0)
    scoped.set(1) {
      assertEquals(scoped.get, 1)
      scoped.set(2) {
        assertEquals(scoped.get, 2)
      }
      assertEquals(scoped.get, 1)
    }
    assertEquals(scoped.get, 0)
  }

  test("Multiple Scoped instances are independent") {
    val s1 = Scoped("s1")
    val s2 = Scoped("s2")

    s1.set("s1-mod") {
      assertEquals(s1.get, "s1-mod")
      assertEquals(s2.get, "s2")

      s2.set("s2-mod") {
        assertEquals(s1.get, "s1-mod")
        assertEquals(s2.get, "s2-mod")
      }
    }
  }

  test("Scoped restores value after exception") {
    val scoped = Scoped("default")
    try {
      scoped.set("temp") {
        assertEquals(scoped.get, "temp")
        throw new RuntimeException("boom")
      }
    } catch {
      case _: RuntimeException => // expected
    }
    assertEquals(scoped.get, "default")
  }
}
