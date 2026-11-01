package ag.k2

import language.experimental.captureChecking
import language.experimental.saferExceptions

import upickle.default.ReadWriter

// ---- Plaintext, kept on a leash
//
// An opened Sealed value. Its type captures the KeyStore that opened it, and
// capture checking keeps it wherever that store may go: never in an event
// (events are pure), never in a global, never past the store's scope. Pure
// functions (`->`, which capture no capability, so can't reach an Outbox, a
// K2Client, or a Reveal) can compute with it; their results stay Plain.
// Plaintext leaves in two ways only: sealed again, or revealed through a
// Reveal.
//
// What this doesn't cover: ambient authority. Code (pure functions
// included) can still println, write files, or call global objects, as
// those aren't capabilities. The guarantee is about the sinks that are
// capabilities, in code compiled with capture checking.
final class Plain[+T] private[k2] (value: T, val keys: KeyStore) {
  def map[U](f: T -> U): Plain[U]^{this} = Plain(f(value), keys)

  // sealed again, with `key`, by the same store
  def seal[U >: T: ReadWriter](key: KeyId): Sealed[U] throws SealedException =
    Sealed[U](value, key)(using summon, keys)

  // the plaintext itself: a declassification, authorized by `by`
  def reveal(using by: Reveal): T = value

  // (so that logging one shows nothing)
  override def toString: String = "Plain(...)"
}

// Permission to take plaintext out of a Plain. Each one names its reason,
// and is made where an auditor can find it: the places that make them are
// the code that has to be trusted with plaintext. It's a capability, so a
// pure function can't use one.
final class Reveal(val reason: String) extends caps.SharedCapability
