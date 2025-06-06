package ag.grader

import java.security.MessageDigest
import scala.collection.concurrent.TrieMap

object Docker {

  private val cache = TrieMap[os.Path, String]()

  def of(docker_file: os.Path): String =
    cache.getOrElseUpdate(
      docker_file, {
        if (!os.exists(docker_file)) {
          println(s"${docker_file.toString} does not exist")
        }

        val sha = MessageDigest
          .getInstance("MD5")
          .nn
          .digest(os.read(docker_file).getBytes)
          .nn
        val sha_string = sha.map(b => f"$b%02x").mkString
        val image_name = s"grader:$sha_string"

        val existing =
          os.proc("docker", "images", "-q", image_name).call().out.lines()
        if (existing.isEmpty) {
          println(s"$image_name not found, creating")
          val _ = os
            .proc("docker", "build", "-t", image_name, "-f", docker_file, ".")
            .call(
              cwd = os.temp.dir(deleteOnExit = true),
              check = true,
              stdout = os.Inherit,
              stderr = os.Inherit
            )
        } else if (existing.size > 1) {
          println(s"    found ${existing.size} of them")
          existing.sorted.foreach { n =>
            println(s"        $n")
          }
          println("too many images")
        }
        image_name
      }
    )

  def dockerCommand(
      workingDir: os.Path,
      dockerImage: Option[String]
  ): os.Shellable = {
    dockerImage.toSeq.flatMap { dockerImage =>
      Seq(
        "docker",
        "run",
        "--rm",
        "-v",
        s"${workingDir.toString}:/work",
        "-w",
        "/work",
        "-u",
        s"${Project.uid}:${Project.gid}",
        "-t",
        dockerImage
      )
    }
  }

}
