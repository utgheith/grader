package ag.common

class Lazy[+A](f: () => A) {
  private lazy val v = f()

  def get(): A = v

  def map[B](f: A => B): Lazy[B] = new Lazy(() => f(v))
  def flatMap[B](f: A => Lazy[B]): Lazy[B] = new Lazy(() => f(v).get())
}

object Lazy {
  def apply[A](f: => A): Lazy[A] = new Lazy(() => f)
}
