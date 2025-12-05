package ag.common

import munit.FunSuite
import upickle.default.ReadWriter

import ag.common.Signer.{given_Signer_Path, given_Signer_Path_Function}

case class SimpleData(i: Int, s: String) derives ReadWriter

class SignerTests extends FunSuite {
  test("signer for simple data") {

    val d1 = SimpleData(1, "foo")
    val d2 = SimpleData(1, "foo")
    val d3 = SimpleData(2, "bar")

    val s1 = Signer.sign(d1)
    val s2 = Signer.sign(d2)
    val s3 = Signer.sign(d3)

    assert(clue(s1) == clue(s2))
    assert(clue(s1) != clue(s3))
  }

  test("signer for file") {
    val tmp = os.temp(
      dir = os.root / "tmp",
      prefix = "SignerTests",
      deleteOnExit = true
    )

    os.write.over(tmp, "content1")
    val s1 = Signer.sign(tmp)

    os.write.over(tmp, "content1")
    val s2 = Signer.sign(tmp)

    os.write.over(tmp, "content2")
    val s3 = Signer.sign(tmp)

    assert(clue(s1) == clue(s2))
    assert(clue(s1) != clue(s3))

  }

  test("signer with exclusion") {
    val tmpDir = os.temp.dir(
      dir = os.root / "tmp",
      prefix = "SignerTests",
      deleteOnExit = true
    )

    os.write(tmpDir / "a.txt", "a")
    os.write(tmpDir / "b.txt", "b")

    val s1 = Signer.sign(tmpDir) // include all

    // modify b.txt
    os.write.over(tmpDir / "b.txt", "b2")
    val s2 = Signer.sign(tmpDir)
    assert(clue(s1) != clue(s2))

    // exclude b.txt
    val s3 = Signer.sign(
      (tmpDir, (p: os.RelPath) => p.segments.nonEmpty && p.last == "b.txt")
    )

    // modify b.txt again
    os.write.over(tmpDir / "b.txt", "b3")
    val s4 = Signer.sign(
      (
        tmpDir,
        (p: os.RelPath) => p.segments.nonEmpty && p.last == "b.txt"
      )
    )

    assert(clue(s3) == clue(s4))

  }
}
