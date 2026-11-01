# Step 1: Plaintext in the types

Two experimental Scala 3 features, used together:

- **Checked exceptions** (`saferExceptions`): a method declares
  `throws E`, and callers must handle `E` or declare it in turn.
- **Capture checking** (`captureChecking`): types record which
  *capabilities* a value can reach, and the compiler enforces it.
  A function `A -> B` is pure, reaching no capability; `A => B` may reach
  any.

## What changed

**Failures are checked.** Each one that a caller must decide about has a
checked exception:
- `K2Exception`: K2 errors. Being offline is one too, marked retryable.
- `SealedException`: a missing key when sealing, or a value that doesn't
  open with the key it names.
- `EventException`: an event too big for K2, or a record that isn't an
  event.

**Opened values are tracked.**
- `KeyStore` is a capability.
- `Sealed[T].open` returns a `Plain[T]^{keys}`: a value whose type
  captures the key store that opened it.
- A `Plain` can be:
  - computed with, by pure functions only (`map(f: T -> U)`), and the
    result stays `Plain`;
  - sealed again (`seal`);
  - revealed (`reveal`), which needs a `Reveal`. A `Reveal` is a capability,
    made with a stated reason. The places that make one are, by
    construction, the code that must be trusted with plaintext.
- `Event` is pure (`caps.Pure`): an event can't hold a capability, and so
  can't hold a `Plain`.
- `Outbox` and `K2Client`, the places data leaves through, are
  capabilities. A pure function can't reach them.

## What it guarantees

In code compiled with capture checking, without `Reveal`:

| Attempt | Result |
|---|---|
| Put plaintext in an event | rejected: `Plain[T]^{keys}` doesn't fit a pure field |
| Compute an event from plaintext, then append it to an outbox | rejected: the function passed to `map` would capture the outbox |
| Store plaintext in a global | rejected: its capture set must be empty |
| Reveal plaintext inside a pure function | rejected: the function would capture the `Reveal` |
| Ignore a K2, key or event failure | rejected: unhandled checked exception |

The checks run at compile time and cost nothing at run time.

## How it is checked

`src/test/scala/ag/k2/PlainTests.scala` compiles small programs with the
real compiler, and asserts which compile and which are rejected, and why.
munit's `compileErrors` isn't enough: it stops after the typer, and capture
checking runs later.

## What it does not cover

- **Ambient authority.** `println`, file writes (`os.write`), calls to
  global objects, and the network through anything that isn't a
  capability are all reachable from a "pure" function. Capture checking
  constrains only declared capabilities. A test (`not covered: ambient
  authority`) records this gap, so that closing it shows up as a change.
- **Values derived through ordinary methods.** Within a pure function,
  `csid.length` is an ordinary `Int`. What keeps it contained is that `map`
  wraps the result back into a `Plain`. Code that can `reveal` gets
  unconstrained values.
- **Code without capture checking.** The guarantees hold only in files
  compiled with it. Elsewhere `Plain` and `Event` are ordinary types.
- **Implicit flows and side channels**, such as branching on a secret, or
  timing. This is capability safety, not noninterference.
- **Escape hatches:** casts, reflection and `caps.unsafe`.
- **Whether the guarantee means anything at the system level.** That
  depends on which components hold keys and `Reveal`s. A system model
  (planned) has to say this.

## Costs and pitfalls (Scala 3.10)

- **A global `-experimental` flag.** Importing `saferExceptions` at the top
  of a file marks every definition in it experimental, which breaks all
  callers. Importing it inside a class is rejected.
- **Mixing files.** A capture-checked file can't call a `throws` method in a
  file that isn't capture-checked. In practice, files with `throws` all use
  capture checking, or none do.
- **upickle.** Its derivation macros (`derives ReadWriter`, `macroRW`) crash
  under capture checking. Readers and writers are written by hand.
- **Small restrictions:** a catch with a guard can't provide a capability,
  and catching a Java exception produces a spurious "unused" warning.

## Next

- A threat model and a system model, so that the trusted components (key
  holders, `Reveal` makers) are named and the properties stated precisely.
- Closing the ambient-authority gap: forbid ambient effects in trusted
  code, or make I/O a capability.
