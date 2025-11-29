package ag.common

import language.experimental.saferExceptions

trait SafeTry[-E <: Exception, T] {
  def get(using CanThrow[E]): T
}

object SafeTry {
  def success[E, T](v: T): SafeTry[E,T] = new SafeTry[E, T] {
    def get(using CanThrow[E]): T = v
  }
  def failure[E <: Exception, T](e: E): SafeTry[E, T] = new SafeTry[E,T] {
    override def get(using CanThrow[E]): Nothing = throw e
  }
  inline def apply[E <: Exception, T](f: => T throws E): SafeTry[E, T] = try {
    success(f)
  } catch {
    case e: E => failure(e)
  }
}
