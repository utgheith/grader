package ag.rules

import java.util.concurrent.Semaphore

class Lazy[+A](f: () => A) {

  @volatile
  private var it: Option[A] = None
  private val sem = Semaphore(1)

  def value: A = {
    if (it.isEmpty) {
      sem.acquire(1)
      try {
        if (it.isEmpty) {
          it = Some(f())
        }
      } finally {
        sem.release(1)
      }
    }
    it.get
  }

  def ready: Boolean = it.nonEmpty
}

object Lazy {
  def apply[A](f: => A): Lazy[A] = new Lazy(() => f)
}
