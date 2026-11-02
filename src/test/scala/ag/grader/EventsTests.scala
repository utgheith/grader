package ag.grader

import munit.FunSuite

import scala.collection.mutable
import scala.quoted.*
import scala.tasty.inspector.*

// The concrete classes that extend ag.k2.Event, found in compiled code (its
// TASTy files), so that none can be missed out of a registry.
object EventClasses {
  // in `where` (a jar, or a directory of classes), with `classpath` for what
  // that code uses
  def in(where: os.Path, classpath: Seq[String]): Set[String] = {
    val found = mutable.Set[String]()
    val inspector = new Inspector {
      def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
        import quotes.reflect.*
        val event = TypeRepr.of[ag.k2.Event]
        object find extends TreeTraverser {
          override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
            tree match {
              case c: ClassDef =>
                val s = c.symbol
                if (
                  !s.flags.is(Flags.Trait) && !s.flags.is(Flags.Abstract) &&
                  s.typeRef <:< event
                ) found += s.fullName
              case _ => ()
            }
            super.traverseTree(tree)(owner)
          }
        }
        for (t <- tastys) find.traverseTree(t.ast)(Symbol.noSymbol)
      }
    }
    val ok =
      if (os.isDir(where))
        TastyInspector.inspectAllTastyFiles(
          os.walk(where).filter(_.ext == "tasty").map(_.toString).toList,
          Nil,
          classpath.toList
        )(inspector)
      else {
        // (sbt's cache keeps jars under names without .jar, which the
        // inspector won't read)
        val jar = os.temp(suffix = ".jar")
        os.copy.over(where, jar)
        TastyInspector.inspectAllTastyFiles(
          Nil,
          List(jar.toString),
          classpath.toList
        )(inspector)
      }
    assert(ok, s"couldn't read the TASTy in ${where.toString}")
    found.toSet
  }

  // the test classpath (see build.sbt)
  lazy val classpath: Seq[String] =
    String(
      getClass.getResourceAsStream("/snippet-classpath.txt").nn.readAllBytes().nn,
      java.nio.charset.StandardCharsets.UTF_8
    ).split(java.io.File.pathSeparator).toSeq

  def location(c: Class[?]): os.Path =
    os.Path(java.nio.file.Paths.get(c.getProtectionDomain.nn.getCodeSource.nn.getLocation.nn.toURI).nn)
}

class EventsTests extends FunSuite {
  test("every Event class in grader is in Events.kinds") {
    val registered = Events.kinds.all.map(_.schema.root).toSet
    val found =
      EventClasses.in(EventClasses.location(Events.getClass), EventClasses.classpath)
    assertEquals(found -- registered, Set.empty[String])
  }

  // (so that the check above is known to find what there is)
  test("the search finds the test events") {
    val found = EventClasses.in(
      EventClasses.location(classOf[EventsTests]),
      EventClasses.classpath
    )
    assertEquals(found, Set("ag.k2.Enrolled", "ag.k2.Ping", "ag.k2.Graded"))
  }
}
