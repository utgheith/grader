import ag.github.Github
import ag.r2.{Scope, Target, update_data, WithData}

import ag.common.sh

class Z3(release: String) extends Scope(release) {

  lazy val checkout: Target[WithData[String]] =
    Github.checkout(os.RelPath("Z3Prover/Z3"), s"z3-$release")

  def install(prefix: os.Path) = target((co = checkout)) { (producer, deps) =>
    update_data(producer)(skip = _ => false) { d =>
      val src = deps.co.get_data_path(producer) / "repo"
      val build = src / "build"
      sh"python scripts/mk_make.py --staticbin --prefix $d" (src)(using
        producer
      )
      sh"make" (build)(using producer)
      sh"make install" (build)(using producer)
      os.copy(
        d / "bin" / "z3",
        prefix / "bin" / s"z3-$release",
        replaceExisting = true,
        createFolders = true
      )
      release
    }
  }

}

class FStar(release: String) extends Scope(release) {
  lazy val checkout: Target[WithData[String]] =
    Github.checkout(os.RelPath("FStarLang/FStar"), s"v$release")

  lazy val opam_deps = target((co = checkout)) { (producer, deps) =>
    sh"opam install -y --deps-only ${deps.co.get_data_path(producer) / "repo"}" ()(
      using producer
    )
    release
  }

  def build(prefix: os.Path) = target(
    (
      opam_deps = opam_deps,
      co = checkout,
      z3 = Z3("4.8.7").install(prefix),
      z32 = Z3("4.13.3").install(prefix)
    )
  ) { (producer, deps) =>
    sh"make" (deps.co.get_data_path(producer) / "repo")(using producer)
    release
  }

  def install(prefix: os.Path) =
    target((checkout = checkout, build = build(prefix))) { (producer, deps) =>
      val src = deps.checkout.get_data_path(producer) / "repo"
      sh"env PREFIX=$prefix make install" (src)(using producer)
      deps.build
    }
}

class PlTools(prefix: os.Path) extends Scope("pl_tools") {

  lazy val fstar = FStar("2025.08.07").install(prefix)
}
