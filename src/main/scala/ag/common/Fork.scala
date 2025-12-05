package ag.common

import java.util.concurrent.Semaphore
import scala.annotation.threadUnsafe
import scala.util.{Failure, Success, Try}

trait Fork[+A] { outer =>
  def result: Try[A]
  def join: A = result.get
  def map[B](f: A => B): Fork[B] = new Fork[B] {
    override lazy val result: Try[B] = outer.result.map(f)
  }
}

object Fork {
  val builder: Thread.Builder = Thread.ofVirtual().name("fork", 0)

  class Forker[A](f: () => A) extends Fork[A] {
    @threadUnsafe
    private lazy val the_result: Try[A] = Try(f())

    private val s = new Semaphore(1)

    override def result: Try[A] = s.down(1) {
      the_result
    }
  }

  inline def apply[A](
      body: => A
  ): Fork[A] = {
    val f = Forker[A](() => body)
    builder.start { () =>
      val _ = f.result
    }
    f
  }

  def success[A](a: A): Fork[A] = {
    new Fork[A]() {
      override def result: Try[A] = Success(a)
      override def join: A = a
    }
  }

  def failure(e: Throwable): Fork[Nothing] = {
    new Fork[Nothing] {
      override def result: Try[Nothing] = Failure(e)
      override def join: Nothing = throw e
    }
  }
}
