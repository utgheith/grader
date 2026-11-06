package ag.github

import ag.common.sh
import ag.r2.{periodic, Scope, Target, WithData, update_data}

object Github extends Scope(".") {

  lazy val mirror: os.RelPath => Target[WithData[String]] = fun {
    (repo: os.RelPath) =>
      target(
        (
          periodic = periodic(15 * 60 * 1000)
        )
      ) { (producer, _) =>
        println(s"updating mirror for ${repo.toString}")
        update_data(producer)(_ => true) { d =>
          val dir = d / "repo"
          if (os.exists(dir)) {
            sh"git pull" (dir)(using producer)
          } else {
            os.makeDir.all(dir)
            sh"git clone --mirror https://github.com/${repo.toString} $dir" ()(
              using producer
            )
          }
          sh"git rev-parse HEAD" (cwd = dir, out = _.out.text().trim)(using
            producer
          )
        }
      }
  }

  lazy val checkout: (os.RelPath, String) => Target[WithData[String]] = fun {
    (repo: os.RelPath, commit: String) =>
      target((the_repo = Github.mirror(repo))) { (producer, deps) =>
        update_data(producer)(skip = _ => true) { d =>
          val dir = d / "repo"
          os.remove.all(dir)
          sh"git clone --local --depth=1 --branch ${commit} ${deps.the_repo.get_data_path(producer) / "repo"} $dir" (
            os.pwd
          )(using producer)
          os.proc("git", "rev-parse", "HEAD").call(cwd = dir).out.text().trim
        }
      }

  }

}
