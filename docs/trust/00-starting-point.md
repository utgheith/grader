# Step 0: Starting point

## The application

*grader* is a monolithic Scala 3 application. It runs on in-house machines
and does all of the work:
- fetches student submissions from git;
- runs tests on them;
- tracks dependencies between results, with its own build-system-like
  engine;
- generates reports and web pages;
- sends notifications.

Student data (ids, code, results) lives on those machines, and in git on an
in-house server.

## The event library (`ag.k2`)

`ag.k2` is the first piece built for the hosted future: a library for an
event stream kept in Cloudflare K2. It has four parts.

- **Events** are Scala objects, and JSON in the stream. Each names its own
  type and version. Each carries two times: `as_of`, when its source made it,
  and `as_at`, when K2 took it in. K2 stamps `as_at` itself, so producers
  can't forge it.
- **Outbox:** sources are often offline. They append events to a local file
  (flushed to disk) and sync it to K2 when they can. K2 doesn't deduplicate,
  so a retried batch may be stored twice.
- **Consumer:** reads a subscription, drops duplicates by event id, and acks
  a batch only after it has been handled.
- **Sealed values:** events aren't encrypted, but fields with private data
  are `Sealed[T]`:
  - AES-256-GCM, with the key's id bound in as associated data;
  - one key per subject (e.g. per student), kept in a local key file on
    each producer and consumer, never in the stream;
  - shredding a key makes everything sealed with it unreadable on that
    machine;
  - a missing key isn't an error: the value is simply unavailable.

## What holds at this point, and what doesn't

| | |
|---|---|
| Holds, by design | Sealed fields are ciphertext everywhere outside a machine that holds the key. |
| Not enforced | Nothing stops code from opening a sealed value and putting the plaintext into an event, a log or a network call. The protection is only as good as every line of code that opens a value. |
| Not enforced | Failures (no network, K2 errors, a missing key, a tampered value) are runtime exceptions that callers may ignore. |

The next step closes both "not enforced" rows, as far as the compiler can
check them.
