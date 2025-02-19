package ag.common

import scala.concurrent.ExecutionContext

class VirtualExecutionContext(prefix: String) extends ExecutionContext {
  val builder: Thread.Builder = Thread.ofVirtual().name(prefix, 0)

  override def execute(runnable: Runnable): Unit = {
    val _  = builder.start(runnable)
  }

  override def reportFailure(cause: Throwable): Unit = {
    cause.printStackTrace();
    System.exit(-1)
  }
}

given VirtualExecutionContext = VirtualExecutionContext("vt")
