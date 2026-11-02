package ag.grader

import ag.k2.EventKinds

// Every kind of event grader reads or writes. Bindings for other languages
// (./run binding) are generated from these, and a test checks that every
// Event class is here.
object Events {
  val kinds: EventKinds = EventKinds()
}
