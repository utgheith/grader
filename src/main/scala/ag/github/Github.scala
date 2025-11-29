package ag.github

import ag.common.sh
import ag.r2.{
  periodic,
  Scope,
  Target,
  Tracker,
  WithData,
  run_if_needed,
  update_data
}
import ag.r2.Producer

object Github extends Scope(".") {

  lazy val mirror: os.RelPath => Target[Nothing, WithData[String]] = fun {
    (repo: os.RelPath) =>
      target(periodic(15 * 60 * 1000)) { _ =>
        println(s"updating mirror for ${repo.toString}")
        update_data[Nothing, String](_ => true) { d =>
          val dir = d / "repo"
          if (os.exists(dir)) {
            sh"git pull" (dir)
          } else {
            os.makeDir.all(dir)
            sh"git clone --mirror https://github.com/${repo.toString} $dir" ()
          }
          sh"git rev-parse HEAD" (cwd = dir, out = _.out.text().trim)
        }
      }
  }

  lazy val checkout: (os.RelPath, String) => Target[Nothing, WithData[String]] =
    fun { (repo: os.RelPath, commit: String) =>
      target(Github.mirror(repo)) { the_repo =>
        update_data[Nothing, String](skip = _ => true) { d =>
          val dir = d / "repo"
          os.remove.all(dir)
          sh"git clone --local --depth=1 --branch $commit ${the_repo.get_data_path / "repo"} $dir" (
            os.pwd
          )
          os.proc("git", "rev-parse", "HEAD").call(cwd = dir).out.text().trim
        }

      }

    }

}
