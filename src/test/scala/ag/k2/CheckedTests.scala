package ag.k2

import munit.FunSuite

// ag.k2's exceptions are checked: code that calls a method that `throws`
// has to handle what it throws. (No saferExceptions import here: this is
// how ag.k2 looks from ordinary code.)
class CheckedTests extends FunSuite {
  test("an unhandled checked exception doesn't compile") {
    val errors = compileErrors(
      """Sealed("x", KeyId.fresh())(using summon, KeyStore(os.temp.dir() / "keys"))"""
    )
    assert(
      errors.contains(
        "The capability to throw exception ag.k2.SealedException is missing"
      ),
      errors
    )
  }

  test("code that throws nothing needs nothing") {
    assert(KeyId.fresh().value.nonEmpty)
    assertEquals(K2.max_record_bytes, 1_000_000)
  }
}
