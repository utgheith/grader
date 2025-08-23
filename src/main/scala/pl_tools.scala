import ag.r2.{periodic, Scope, Target, ToRelPath, update_data, WithData}
import java.util.concurrent.Semaphore

class ProcessHandler(p: os.proc) {
  def apply[Out](
      cwd: os.Path = os.pwd,
      out: os.CommandResult => Out = _ => (),
      check: Boolean = true,
      show: Boolean = true
  ): Out = {

    ProcessHandler.lock {
      pprint.pprintln((p.command, cwd))
    }
    val stdout = if (show) os.Inherit else os.Pipe
    val stderr = if (show) os.Inherit else os.Pipe
    out(p.call(cwd = cwd, check = check, stdout = stdout, stderr = stderr))
  }

}

object ProcessHandler {
  val lock = new Semaphore(1)
}

extension (s: Semaphore) {
  def apply[T](f: => T): T = {
    s.acquire()
    try f
    finally s.release()
  }
}

extension (sc: StringContext) {

  def sh(args: Any*): ProcessHandler = {
    assert(args.length >= 0)
    assert(sc.parts.length == args.length + 1)

    val strings = if (args.length == 0) {
      sc.parts.head.split("  *").to(Seq).filterNot(_.isEmpty)
    } else {
      // Normal Scala:
      //    split(" x ") ==> ["", "x"]
      // What we want:
      //    split(" x ") ==> ["", "x", ""]

      def consistent_split(s: String): Seq[String] = {
        val out =
          (" " + s).split("  *").to(Seq) ++ (if (s.endsWith(" ")) Seq("")
                                             else Seq())
        println(s"consistent_split('$s') = ${out.toString}")
        if (out.isEmpty) Seq("") else out
      }

      println("------------------")

      pprint.pprintln((sc.parts, args))

      (for {
        (Seq(part, next_part), arg) <- sc.parts
          .map(consistent_split)
          .sliding(2)
          .zip(args)
        x <- part.tail.dropRight(
          1
        ) :+ (part.last + arg.toString + next_part.head)
        _ = println(s"x = ___${x}___")
      } yield x).toSeq
    }

    println(s"********************** output has ${strings.length} elements")

    pprint.pprintln(strings.toSeq)

    ProcessHandler(os.proc(strings.toSeq))
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
