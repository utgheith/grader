package ag.rules

trait Task[+Out] { self =>
  def block: Out

  def ready: Boolean

  def map[A](f: Out => A): Task[A] = flatMap { out =>
    try {
      Task.success(f(out))
    } catch {
      case e: RuleException => Task.failure(e)
    }
  }

  def flatMap[A](f: Out => Task[A]): Task[A] = if (ready) {
    f(block)
  } else {
    Task {
      f(block).block
    }
  }

}

object Task {
  type BlockForTuple[T] <: Tuple = T match {
    case EmptyTuple      => EmptyTuple
    case Task[a] *: rest => a *: BlockForTuple[rest]
  }
  private val builder = Thread.ofVirtual().nn.name("t", 0).nn

  def failure(e: RuleException): Task[Nothing] = new Task[Nothing] {
    override def block: Nothing = throw e

    override def ready: Boolean = true
  }

  def success[A](a: A): Task[A] = new Task[A] {
    override def block: A = a

    override def ready: Boolean = true
  }

  def apply[A](f: => A): Task[A] = new Task[A] {
    import language.unsafeNulls

    @volatile
    private var state: Option[Either[RuleException, A]] = None

    private val t = builder.start { () =>
      try {
        state = Some(Right(f))
      } catch {
        case e: RuleException =>
          state = Some(Left(e))
        case e: Throwable =>
          e.printStackTrace()
          sys.exit(-1)
      }
    }

    override def block: A = {
      t.join()
      state match {
        case Some(Left(e))  => throw e
        case Some(Right(a)) => a
        case None =>
          println("************ internal error *************")
          sys.exit(-1)
      }
    }

    override def ready: Boolean = state.nonEmpty
  }

  def blockForTuple[T <: Tuple](t: T): BlockForTuple[T] = (t match {
    case x: EmptyTuple        => x
    case (h: Task[_]) *: tail => h.block *: blockForTuple(tail)
    case _                    => ???
  }).asInstanceOf[BlockForTuple[T]]

  def sequence[T <: Tuple](t: T): Task[BlockForTuple[T]] =
    new Task[BlockForTuple[T]] {
      private val it = Lazy(blockForTuple(t))
      override def ready: Boolean =
        if (it.ready) true
        else t.productIterator.forall(_.asInstanceOf[Task[?]].ready)
      override def block: BlockForTuple[T] = it.value
    }

  def sequence[A](s: Iterable[Task[A]]): Task[Iterable[A]] =
    new Task[Iterable[A]] {
      private val it = Lazy(s.map(_.block))
      override def ready: Boolean = if (it.ready) true else s.forall(_.ready)
      override def block: Iterable[A] = it.value
    }
}
