import ag.r2.{periodic, Scope, Target, ToRelPath, update_data, WithData}

class ProcessHandler(p: os.proc) {
  def apply[Out](
      cwd: os.Path = os.pwd,
      out: os.CommandResult => Out = _ => (),
      check: Boolean = true,
      show: Boolean = true
  ): Out = {
    pprint.pprintln((p.command, cwd))
    val stdout = if (show) os.Inherit else os.Pipe
    val stderr = if (show) os.Inherit else os.Pipe
    out(p.call(cwd = cwd, check = check, stdout = stdout, stderr = stderr))
  }

}

extension (sc: StringContext) {

  def sh(args: Any*): ProcessHandler = {
    val clean_parts = sc.parts.map(_.trim.split("  *").toIndexedSeq)

    val s = clean_parts.head ++ clean_parts.tail.zip(args).flatMap {
      case (part, arg) => arg.toString +: part
    }

    ProcessHandler(os.proc(s))
  }
}

object Github extends Scope(".") {

  lazy val mirror: os.RelPath => Target[WithData[String]] = fun {
    (repo: os.RelPath) =>
      target(
        periodic(15 * 60 * 1000)
      ) { _ =>
        println(s"updating mirror for ${repo.toString}")
        update_data(_ => true) { d =>
          val dir = d / "repo"
          val _ = if (os.exists(dir)) {
            sh"git pull" (dir)
          } else {
            os.makeDir.all(dir)
            sh"git clone --local --depth=1 ${repo} ." (dir)
          }
          sh"git rev-parse HEAD" (cwd = dir, out = _.out.text().trim)
        }
      }
  }

  lazy val checkout: (os.RelPath, String) => Target[WithData[String]] = fun {
    (repo: os.RelPath, commit: String) =>
      target(Github.mirror(repo)) { the_repo =>
        update_data(skip = _ => true) { d =>
          val dir = d / "repo"
          os.remove.all(dir)
          sh"git clone --local --depth=1 --branch ${commit} ${the_repo.get_data_path / "repo"} $dir" (
            os.pwd
          )
          os.proc("git", "rev-parse", "HEAD").call(cwd = dir).out.text().trim
        }
      }

  }

}

class Z3(release: String) extends Scope(release) {
  lazy val checkout: Target[WithData[String]] =
    Github.checkout(os.RelPath("Z3Prover/Z3"), s"z3-$release")
  lazy val build = target(checkout) { co =>
    update_data(skip = _ => false) { d =>
      val src = co.get_data_path / "repo"
      val build = src / "build"
      sh"python scripts/mk_make.py --staticbin --prefix $d" (src)
      sh"make" (build)
      sh"make install" (build)

      release
    }
  }

}

object PlTools extends Scope("pl_tools") {

  lazy val z3_old = new Z3("4.8.7")
  lazy val z3_new = new Z3("4.13.3")

  lazy val z3 = target(z3_old.build, z3_new.build) { (o, n) =>
    (o, n)
  }

}
