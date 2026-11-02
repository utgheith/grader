# Step 2: Events described for other languages

The hosted side will not all be Scala: the trusted core is planned in Rust,
Workers may be TypeScript, and tools may be Python. Every one of them has
to agree on what an event is. This step makes that agreement something the
compiler derives, rather than something each language re-implements.

## What changed

- **A schema per event kind, derived at compile time.** A macro reads each
  event's Scala type, the same type upickle encodes, and records:
  - each field's name and type;
  - the records and enums it uses;
  - which fields have defaults. upickle leaves out a field that equals its
    default, so readers must supply it.

  Scala stays the source of truth: event authors write case classes as
  before, and the schema comes with each `EventKind`.
- **A portable subset, enforced.** Events may only use String, Boolean,
  Int, Double, Seq, Option, string-keyed Map, non-generic case classes,
  enums with only simple cases, and `Sealed[A]`. Anything else is a compile
  error where the kind is made. `Long` is excluded because upickle writes
  big values as strings, which other languages would read as a different
  type.
- **A registry of grader's event kinds** (`ag.grader.Events`), and a test
  that reads the compiled code (TASTy) and fails if any `Event` class is
  missing from it. The macro describes what is registered; the TASTy check
  ensures nothing escapes registration.
- **Generated bindings:** `./run binding --language python` writes a Python
  module that reads and writes events, envelopes included, as `ag.k2` does.
  It uses only Python's standard library, plus `cryptography` for sealing
  and opening.

## What it guarantees

| Claim | How it is checked |
|---|---|
| A schema matches the encoding | The schema is derived from the same type upickle encodes. Generated Python decodes events written by Scala and encodes them back; Scala then reads them as equal values (test). |
| No event uses a type other languages can't read | Compile error (tests compile counterexamples). |
| No event class is left out of the bindings | The TASTy scan of compiled code (test). The test also shows the scan finds the test suite's own events. |
| Unknown versions aren't misread | A version a binding doesn't know comes back as `Unrecognized` (test). |

## What it does not cover

- **Upgrades.** These are Scala functions, so other languages read only each
  kind's current version. Older versions arrive as `Unrecognized`.
- **Step 1's guarantees.** Python has no equivalent of `Plain` or capture
  checking: an opened sealed value is an ordinary value there.
- **Python writing keys.** The Python `KeyStore` only reads key files;
  creating and shredding keys stays in Scala.
- **Sealing and opening in Python** are tested only where that Python has
  `cryptography`, e.g. a venv named by `K2_PYTHON`; elsewhere the test is
  skipped. With it, values sealed in Scala open in Python, values sealed in
  Python open in Scala, and Python refuses tampered values and returns
  nothing for missing keys.
- **Time precision.** Python keeps times to the microsecond, so a
  nanosecond-precision `as_of` written by Scala is truncated there.
- **Annotations that change encoding** (e.g. upickle's `@key`) aren't
  recognized yet, and should be rejected.

## Next

- A Rust binding, the language of the planned trusted core, with an
  equivalent of `Plain`: a type that can't be serialized or copied.
- Emitting JSON Schema, for validation in Workers.
