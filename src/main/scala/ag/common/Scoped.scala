//noinspection Since15
package ag.common

class Scoped[A](factory: () => A) {
  private lazy val default_value = factory()
  private lazy val jsv = ScopedValue.newInstance[A]()

  def get: A = jsv.orElse(default_value)

  def set[B](a: A)(f: => B): B =
    ScopedValue.where(jsv, a).call(() => f)
}

object Scoped {
  def apply[A](f: => A): Scoped[A] = new Scoped(() => f)
}

