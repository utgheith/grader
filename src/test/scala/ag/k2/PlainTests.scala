package ag.k2

import munit.FunSuite

// What capture checking stops: plaintext (a Plain) going anywhere its
// KeyStore can't. Each snippet compiles, as ag.k2's code does, or doesn't.
class PlainTests extends FunSuite {
  private val prelude = """
    final case class Note(course: String) extends Event { def kind = Note.kind }
    object Note {
      given upickle.default.ReadWriter[Note] =
        upickle.default.readwriter[String].bimap(_.course, Note(_))
      val kind: EventKind[Note] = EventKind[Note]("note")
    }
    def opened(using keys: KeyStore): Plain[String]^{keys} = ???
  """

  private def compiles(body: String): Unit = {
    val errors = Snippets.errors(prelude + body)
    assert(errors.isEmpty, errors.mkString("\n"))
  }

  private def rejected(body: String, because: String): Unit = {
    val errors = Snippets.errors(prelude + body)
    assert(errors.exists(_.contains(because)), errors.mkString("\n"))
  }

  test("pure functions compute with plaintext; the result stays Plain") {
    compiles("""
      def f(using keys: KeyStore): Plain[Int]^{keys} =
        for (csid <- opened) yield csid.length
    """)
  }

  test("plaintext can be sealed again, or revealed with a Reveal") {
    compiles("""
      def again(using keys: KeyStore): Sealed[String] throws SealedException =
        opened.seal(KeyId("k"))
      def shown(using keys: KeyStore, r: Reveal): String = opened.reveal
    """)
  }

  test("plaintext doesn't go in an event") {
    rejected(
      """
      final case class Leak(csid: Plain[String]) extends Event { def kind = Note.kind }
      def leak(using keys: KeyStore): Leak = Leak(opened)
      """,
      "cannot flow into capture set {}"
    )
  }

  test("an event made from plaintext doesn't reach an outbox") {
    rejected(
      """
      def leak(out: Outbox)(using keys: KeyStore, c: CanThrow[EventException]): Unit =
        val _ = opened.map(csid => out.append(Seq(Note(csid))))
      """,
      "capability `out` cannot flow into capture set {}"
    )
  }

  test("plaintext doesn't escape into a global") {
    rejected(
      """
      object Globals { var csid: Option[Plain[String]] = None }
      def leak(using keys: KeyStore): Unit = Globals.csid = Some(opened)
      """,
      "cannot flow into capture set {}"
    )
  }

  test("a pure function can't reveal") {
    rejected(
      """
      def leak(p: Plain[String])(using r: Reveal): Plain[String] =
        p.map(_ => p.reveal)
      """,
      "capability `r` cannot flow into capture set {}"
    )
  }

  // The gap, kept here so a change in it is noticed: ambient authority
  // isn't a capability, so capture checking doesn't stop it.
  test("not covered: ambient authority, such as println") {
    compiles("""
      def leak(using keys: KeyStore): Plain[Unit]^{keys} =
        opened.map(csid => println(csid))
    """)
  }
}
