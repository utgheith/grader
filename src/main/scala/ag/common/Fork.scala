package ag.common

import language.experimental.saferExceptions

trait Fork[+E <: Exception, +A] { outer =>
  def result: Either[E, A]
  def join(using CanThrow[E]): A = result match {
    case Left(e)  => throw e
    case Right(a) => a
  }
  def map[B](f: A => B): Fork[E, B] = new Fork[E, B] {
    override lazy val result: Either[E, B] = outer.result match {
      case Left(e)  => Left(e)
      case Right(a) => Right(f(a))
    }
  }
}

object Fork {
  val builder: Thread.Builder = Thread.ofVirtual().name("fork", 0)

  class Forker[E <: Exception, A] extends Fork[E, A] {

    var state: Option[Either[E, A]] = None

    override def result: Either[E, A] = synchronized {
      var t = state
      while (t.isEmpty) {
        wait()
        t = state
      }
      t.get
    }
  }

  inline def apply[E <: Exception, A](
      body: => A throws E
  ): Fork[E, A] = {
    val f = new Forker[E, A]

    builder.start { () =>
      try {
        val out =
          try {
            Right(body(using CanThrow[E]))
          } catch {
            case e: E => Left(e)
          }

        f.synchronized {
          f.state = Some(out)
          f.notifyAll()
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          sys.exit(1)
      }
    }
    f
  }

  def success[A](a: A): Fork[Nothing, A] = {
    new Fork[Nothing, A]() {
      override def result: Either[Nothing, A] = Right(a)
      override def join(using CanThrow[Nothing]): A = a
    }
  }

  def failure[E <: Exception](e: E): Fork[E, Nothing] = {
    new Fork[E, Nothing] {
      override def result: Either[E, Nothing] = Left(e)
      override def join(using CanThrow[E]): Nothing = throw e
    }
  }
}
