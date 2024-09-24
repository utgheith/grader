package ag.rules

import scala.collection.SortedSet

// Something that could produce a value (e.g. a rule)
// useful for 2 reasons:
//    - simplifies modelling of leaf rules
//    - captures the behaviors that don't depend on the input type
trait Maker[+Out] { self =>
  // knows how to make its value
  def make(using State): Task[Out]

  def value(using State): Out = make.block

  // could be made up of different parts (e.g. Seq[Maker[?])
  def parts(using State): Task[SortedSet[RuleBase]]

  def flatMap[B](f: Out => Maker[B]): Maker[B] = new Maker[B] {
    override def make(using State): Task[B] = for {
      out <- self.make
      b <- f(out).make
    } yield b

    override def parts(using State): Task[SortedSet[RuleBase]] = for {
      a_parts <- self.parts
      out <- self.make
      b_parts <- f(out).parts
    } yield a_parts ++ b_parts

    override lazy val toString: String = s"Maker.flatMap(${self}, ...)"
  }

  def flatMapSeq[U, B](
      f: U => Maker[B]
  )(using ev: Out <:< Iterable[U]): Maker[Seq[B]] = for {
    out <- this
    seq_u = ev(out).toSeq
    seq_b <- Maker.sequence(seq_u.map(f))
  } yield seq_b

  def map[B](f: Out => B): Maker[B] = new Maker[B] {
    override def make(using State): Task[B] = for {
      out <- self.make
    } yield f(out)

    override def parts(using State): Task[SortedSet[RuleBase]] = self.parts

    override lazy val toString: String = s"Maker.map(${self}, ...)"
  }

  def zip[B](rhs: Maker[B]): Maker[(Out, B)] = new Maker[(Out, B)] {
    override def make(using State): Task[(Out, B)] = for {
      out <- self.make
      b <- rhs.make
    } yield (out, b)

    override def parts(using State): Task[SortedSet[RuleBase]] = for {
      out <- self.parts
      b <- rhs.parts
    } yield out ++ b

    override lazy val toString: String = s"Maker.zip(${self}, ${rhs})"
  }

  type Merge[B, X] <: Tuple = X match {
    case Tuple => B *: X
    case _     => (B, X)
  }

  // Apologies for the type voodoo
  // Maker[A] *: Maker[B] *: Maker[C] => Maker[(A,B,C)]
  // Shorthand for Maker.sequence(ma.zip(Maker.sequence(mb.zip(mc)))
  // Why use `*:`? hints at tuples
  def *:[B, X >: Out](lhs: Maker[B]): Maker[Merge[B, X]] = for {
    right <- this
    left <- lhs
  } yield (right match {
    case t: Tuple => left *: t
    case _        => (left, right)
  }).asInstanceOf[Merge[B, X]]

  // allow another Rule to check my value without creating a dependency
  def peek: Maker[Out] = new Maker[Out] {
    override def make(using State): Task[Out] = self.make
    override def parts(using State): Task[SortedSet[RuleBase]] =
      Task.success(SortedSet[RuleBase]())
  }
}

type MakerBase = Maker[?]

object Maker {

  def apply[Out](f: State => Task[Out]): Maker[Out] = new Maker[Out] {
    override def make(using s: State): Task[Out] = f(s)
    override def parts(using State): Task[SortedSet[RuleBase]] =
      Task.success(SortedSet())
  }

  def unit[Out](o: Out): Maker[Out] = new Maker[Out] {
    override def make(using State): Task[Out] = Task.success(o)

    override def parts(using State): Task[SortedSet[RuleBase]] =
      Task.success(SortedSet[RuleBase]())
  }

  def sequence[Out](m: Seq[Maker[Out]]): Maker[Seq[Out]] = new Maker[Seq[Out]] {
    override def make(using State): Task[Seq[Out]] = Task {
      // force parallel evaluation
      val tasks = m.map(_.make)
      // now we can block
      tasks.map(_.block)
    }

    override def parts(using State): Task[SortedSet[RuleBase]] = {
      Task.sequence(m.map(_.parts)).map { iter =>
        iter.foldLeft(SortedSet[RuleBase]())(_ ++ _)
      }
    }
  }

  type StripMaker[T] <: Tuple = T match {
    case EmptyTuple       => EmptyTuple
    case Maker[h] *: rest => h *: StripMaker[rest]
  }

  type MakeTuple[T] <: Tuple = T match {
    case EmptyTuple       => EmptyTuple
    case Maker[h] *: rest => Task[h] *: MakeTuple[rest]
  }

  // (Maker[A], Maker[B], ....) => (Task[A], Task[B], ....)
  def makeTuple[T <: Tuple](tu: Tuple)(using State): MakeTuple[T] = (tu match {
    case _: EmptyTuple => EmptyTuple
    case h *: rest     => h.asInstanceOf[Maker[?]].make *: makeTuple(rest)
  }).asInstanceOf[MakeTuple[T]]

  // (Maker[A], Maker[B], ...) => Maker[(A, B, ...)]
  def sequence[T <: Tuple](tu: T): Maker[StripMaker[T]] =
    new Maker[StripMaker[T]] {
      override def make(using State): Task[StripMaker[T]] =
        Task.sequence(makeTuple(tu)).asInstanceOf[Task[StripMaker[T]]]

      override def parts(using State): Task[SortedSet[RuleBase]] = {
        Task
          .sequence(
            tu.productIterator
              .map(_.asInstanceOf[Maker[Any]])
              .map(_.parts)
              .toSeq
          )
          .map { iter =>
            iter.foldLeft(SortedSet[RuleBase]())(_ ++ _)
          }
      }
    }

  def pick[A](sma: Maker[Seq[A]])(f: A => Boolean): Maker[Seq[A]] = for {
    sa <- sma
  } yield for {
    a <- sa
    if f(a)
  } yield a

  def select[A](sma: Maker[Seq[A]])(f: A => Maker[Boolean]): Maker[Seq[A]] =
    for {
      sa <- sma
      flags <- Maker.sequence(for {
        a <- sa
      } yield f(a))
    } yield for {
      (a, flag) <- sa.zip(flags)
      if flag
    } yield a

  def select[A](msa: Seq[Maker[A]])(f: A => Maker[Boolean]): Maker[Seq[A]] =
    select(Maker.sequence(msa))(f)

}
