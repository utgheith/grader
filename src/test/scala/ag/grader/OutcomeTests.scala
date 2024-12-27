package ag.grader

import upickle.default.{read, write}

class OutcomeTests extends munit.FunSuite {
  test("results") {
    // write produces a json encoded string
    assert(clue(write(OutcomeStatus.pass)) == "\"pass\"")
    // toString produces a string
    assert(clue(OutcomeStatus.pass.toString) == "pass")
    
    // reads looks for a json encoded string
    assert(clue(read[OutcomeStatus]("\"fail\"")) == OutcomeStatus.fail)
    // valueOf looks for a string
    assert(clue(OutcomeStatus.valueOf("fail")) == OutcomeStatus.fail)
  }
}
