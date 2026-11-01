# From a monolith to a trusted hosted service

This log records, step by step, how *grader* (a course autograder) moves
from a monolithic application on in-house machines to a hosted service
(Cloudflare Workers, with K2 as the event stream) that can be trusted with
student data. Local machines are left as pure test runners.

Each step says what changed and what it guarantees. It also says how that is
checked, and what it does **not** cover. The gaps are as important as the
guarantees: together they show what the next steps must address.

## The goal, in properties

These are drafts; later steps make them precise and verify them.

- **Confidentiality:** plaintext student data is held only by trusted
  components (to be defined; runners, the stream and logs are not among them).
- **Erasure:** once a student's key is shredded at time *t*, nobody can
  recover their data after *t* plus the stores' retention window.
- **Integrity:** a tampered or substituted sealed value is detected, never
  accepted.

## Constraints while this is in progress

Until hosting student data at Cloudflare is approved, student data stays on
university machines, even when encrypted. Anything hosted runs on synthetic
data only.

## Steps

0. [Starting point](00-starting-point.md): the monolith, and an event library
   (`ag.k2`) with sealed values and key shredding.
1. [Plaintext in the types](01-plaintext-in-the-types.md): checked exceptions
   and capture checking. Opened values carry their key store in their type,
   so the compiler keeps them out of events and other sinks.

## Planned

- A model of the system (TLA+ or Alloy) and a threat model, with the
  properties above stated precisely.
- Closing the ambient-authority gap left by step 1.
- Pseudonymous runners: runners see opaque submission ids, never student
  identities, and hold no keys.
- A prototype of the hosted side, on synthetic data.
- The trusted core (keys, sealing, validation, what may leave) in a
  verified language (e.g. Rust with Verus).
