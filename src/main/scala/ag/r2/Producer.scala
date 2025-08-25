package ag.r2

import ag.common.Logging

import scala.annotation.implicitNotFound

// A producer is a context that produces a value of type A

@implicitNotFound("no given Producer")
trait Producer[A] extends Context[A] with Logging {

  // What does it produce?
  def producing: Target[A]

  // Working directory for the producer
  lazy val target_path: os.Path = state.target_path(producing)

  // Where to save the state (e.g. a json file)
  lazy val saved_path: os.Path = state.saved_path(producing)

  // Where to save associated data (e.g. a subdirectory with a bunch of files)
  lazy val data_path: os.Path = state.data_path(producing)

  // Where to save the backup of the state. Used in order to recover from failed attempts
  // to update the state.
  lazy val backup_path: os.Path = state.backup_path(producing)

  // log messages related to this producer here
  lazy val log_path: os.Path = state.log_path(producing)

  override def log(msg: => Any): Unit = {
    if (msg != null)
      os.write.append(log_path, msg.toString, createFolders = true)
  }
}
