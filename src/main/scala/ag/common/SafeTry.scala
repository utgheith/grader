package ag.common

import language.experimental.saferExceptions

trait SafeTry[+E <: Exception, +T] {
  def get(using CanThrow[E]): T
}

object SafeTry {
  def success[T](v: T): SafeTry[Nothing, T] = new SafeTry[Nothing, T] {
    override def get(using CanThrow[Nothing]): T = v
  }
  def failure[E <: Exception](e: E): SafeTry[E, Nothing] =
    new SafeTry[E, Nothing] {
      override def get(using CanThrow[E]): Nothing = throw e
    }
  inline def apply[E <: Exception, T](f: => T throws E): SafeTry[E, T] = try {
    success(f)
  } catch {
    case e: E => failure(e)
  }
}
