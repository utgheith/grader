package ag.rules

import upickle.default.ReadWriter

import scala.collection.SortedSet

// A Maker with:
//    - a name (path) so it can be cached across runs
//    - an input Maker so we can track its dependnecies
//
trait Rule[In, Out: ReadWriter] extends Maker[Out] { self =>

  // What are my named parts
  override def parts(using State): Task[SortedSet[RuleBase]] =
    Task.success(SortedSet(this))

  // What is my input
  def needs: Maker[In]

  // compute my value in a given context
  // called if any of the following holds:
  //   - we don't a cached value
  //   - we have a cached value put our dependency changed
  def compute(context: Context[Out], in: In): Out

  // what is my path
  def path: os.RelPath

  override def make(using State): Task[Out] = get_result.map(_.value)

  def get_result(using s: State): Task[Result[Out]] = s.get(this)

  def signature(using s: State): Signature = {
    s.get(this).block.signature
  }

  override lazy val toString: String = s"Rule(path=$path, needs=$needs)"
}

type RuleBase = Rule[?, ?]

object Rule {

  given Ordering[Rule[?, ?]] = Ordering.by(_.path)

  // Rule() { ... }
  def apply[Out: ReadWriter](rest: os.RelPath | Null = null)(
      f: => Out
  )(using fn: sourcecode.FullName): Rule[Unit, Out] = new Rule[Unit, Out] {
    override val path: os.RelPath = {
      val start = os.RelPath(fn.value.replace('.', '/').nn)
      if (rest != null) start / rest else start
    }

    override def needs: Maker[Unit] = Maker.unit(())

    override def compute(context: Context[Out], in: Unit): Out = f
  }

  // Rule(ma) { a => ... }
  def apply[In, Out: ReadWriter](r1: Maker[In], rest: os.RelPath | Null)(
      f: In => Out
  )(using fn: sourcecode.FullName): Rule[In, Out] = new Rule[In, Out] {
    override val path: os.RelPath = {
      val start = os.RelPath(fn.value.replace('.', '/').nn)
      if (rest != null) start / rest else start
    }

    override def needs: Maker[In] = r1

    override def compute(context: Context[Out], in: In): Out = f(in)
  }

  // Rule((ma, mb, ...), ...) { (a,b, ...) => ... }
  def apply[Out: ReadWriter, T <: Tuple](tu: T, rest: os.RelPath | Null)(
      f: Maker.StripMaker[T] => (Context[Out] ?=> Out)
  )(using fn: sourcecode.FullName): Rule[Maker.StripMaker[T], Out] =
    new Rule[Maker.StripMaker[T], Out] {
      override val path: os.RelPath = {
        val start = os.RelPath(fn.value.replace('.', '/').nn)
        if (rest != null) start / rest else start
      }

      override def needs: Maker[Maker.StripMaker[T]] = Maker.sequence(tu)

      override def compute(
          context: Context[Out],
          in: Maker.StripMaker[T]
      ): Out = {
        f(in)(using context)
      }
    }
}
