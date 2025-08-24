import ag.github.Github
import ag.r2.{Scope, Target, update_data, WithData}

import ag.common.sh

class Z3(release: String) extends Scope(release) {

  lazy val checkout: Target[WithData[String]] =
    Github.checkout(os.RelPath("Z3Prover/Z3"), s"z3-$release")

  def install(prefix: os.Path) = target(checkout) { co =>
    update_data(skip = _ => false) { d =>
      val src = co.get_data_path / "repo"
      val build = src / "build"
      sh"python scripts/mk_make.py --staticbin --prefix $d" (src)
      sh"make" (build)
      sh"make install" (build)
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

  lazy val opam_deps = target(checkout) { co =>
    sh"opam install -y --deps-only ${co.get_data_path / "repo"}" ()
    release
  }

  def build(prefix: os.Path) = target(
    opam_deps,
    checkout,
    Z3("4.8.7").install(prefix),
    Z3("4.13.3").install(prefix)
  ) { (_, co, _, _) =>
    sh"make" (co.get_data_path / "repo")
    release
  }

  def install(prefix: os.Path) = target(checkout, build(prefix)) {
    (checkout, build) =>
      val src = checkout.get_data_path / "repo"
      sh"env PREFIX=$prefix make install" (src)
      build
  }
}

class PlTools(prefix: os.Path) extends Scope("pl_tools") {

  lazy val fstar = FStar("2025.08.07").install(prefix)
}
