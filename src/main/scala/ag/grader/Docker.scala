package ag.grader

import ag.rules.recorded

import java.security.MessageDigest
import scala.collection.concurrent.TrieMap

object Docker {

  private val cache = TrieMap[os.Path, String]()

  // the image's name: from the file's contents
  def image_name(docker_file: os.Path): String = {
    val sha = MessageDigest
      .getInstance("MD5")
      .nn
      .digest(os.read(docker_file).getBytes)
      .nn
    s"grader:${sha.map(b => f"$b%02x").mkString}"
  }

  // the image for `docker_file`, built if it isn't there
  def of(docker_file: os.Path): String =
    cache.getOrElseUpdate(
      docker_file, {
        if (!os.exists(docker_file)) {
          println(s"${docker_file.toString} does not exist")
        }

        val image_name = Docker.image_name(docker_file)

        val images = os.proc("docker", "images", "-q", image_name)
        val existing = recorded(images, os.pwd)(images.call()).out.lines()
        if (existing.isEmpty) {
          ag.rules.refuse_if_read_only(s"build docker image $image_name")
          println(s"$image_name not found, creating")
          val build =
            os.proc("docker", "build", "-t", image_name, "-f", docker_file, ".")
          val build_dir = os.temp.dir(deleteOnExit = true)
          val _ = recorded(build, build_dir) {
            build.call(
              cwd = build_dir,
              check = true,
              stdout = os.Inherit,
              stderr = os.Inherit
            )
          }
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
