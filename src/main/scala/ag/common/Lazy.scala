package ag.common

class Lazy[+A](f: () => A) {
  private lazy val v = f()

  def get(): A = v
}
