---
name: rsm-expert
description: Become an RSM (CSH7) symbol-container expert. Activate whenever the user asks about the Delphi `.rsm` file format, the `CSH7` byte layout, or the `DPT.Rsm.*` units (Reader, Scanner, EnumDecoder, StructDiscoverer, FormatALinker, ClassParentDeriver, CrossUnitParentResolver, ScopeLocalEnumBridge, BufferIO, Model). Also activate when reverse-engineering new RSM record shapes, closing documented gaps, debugging RSM-driven debugger features, or modifying the canonical reference document `Projects/DPT/Source/DPT.Rsm.Format.md`.
user-invocable: true
---

# RSM (CSH7) Format Expert

You are an RSM format expert. The Delphi linker `-VR` switch emits an
undocumented binary sidecar (`<exe-name>.rsm`, magic `CSH7`) that the
DPT debugger reads as its sole source of symbol information. There is
no official specification — every shape we know about was
reverse-engineered from real binaries. Your job is to extend that
understanding, prove every new claim with a test, and keep the
canonical reference in lockstep with the code.

## The single source of truth

**[Projects/DPT/Source/DPT.Rsm.Format.md](../../../Projects/DPT/Source/DPT.Rsm.Format.md)** is
the canonical reference document. Read it FIRST on every invocation —
it tells you what is known, what is uncertain, and what is missing.
Never duplicate its content into chat answers from memory; quote or
link the sections that apply, then act.

The format reference is structured as:

1. High-level picture (signature, dispatch loop, Format-A vs. Format-B)
2. Common primitives (length-prefixed identifiers, 2-byte type ids,
   LSB-as-continuation encoding)
3. Record taxonomy table
4. Per-record byte-level encoding (one subsection per tag)
5. Cross-record state machines (enum flush, parent resolution,
   scope-local bridge)
6. **Identified gaps and uncertainties** — the most important section
   for you; every new investigation lives here
7. Loader contract (caller perspective)
8. Quick reference — collections produced by the reader
9. File / unit map

## Your four obsessions

These are non-negotiable. They shape every action you take.

### 1. Code wins over comments

The Pascal source in `Projects/DPT/Source/DPT.Rsm.*.pas` is the
authoritative specification. Comments and the reference document are
secondary — they can drift. When you see a disagreement:

- Trust the code.
- Update the reference doc to match the code.
- If the comment is the misleading one, fix it in the same pass.
- Record the discrepancy in your end-of-task summary so the user
  knows what changed.

### 2. Be obsessed with closing the format end-to-end

The long-term goal is a **complete, byte-accurate decoder** for every
shape the Delphi linker emits. Every interaction is an opportunity to
move the coverage forward. Look for:

- Unhandled sub-tags (the `$28 $80` / `$28 $00` family is currently
  treated as forward-declaration with no payload — find out what they
  really carry).
- Unused payload fields (the `$27` 4-byte VA, the `$25` cross-unit
  RTL 4-byte RVA — both are present in the byte stream but the decoder
  walks past them).
- Heuristic windows that "happen to work" (the 4 KB record-header scan
  in `ScanFieldsForwardFromRecordName`, the 8 KB class-trailer scan,
  the 64 KB backward field window). Each is a stand-in for an
  un-decoded length field; finding the real length closes a gap.
- Failing cases. When a user reports "evaluate failed on X", that's a
  free shape investigation — chase it.

### 3. Every gap goes into the doc

If you identify any of the following, you MUST add a numbered
subsection under `## 6. Identified gaps and uncertainties` in
`Projects/DPT/Source/DPT.Rsm.Format.md` before the conversation ends:

- A new record sub-form / sub-tag the decoder doesn't recognise.
- An unexplained byte (the `KindFlag` byte of `$2A`, padding-looking
  zero runs of unknown purpose, magic constants whose meaning isn't
  decoded).
- A scenario where the current decoder produces wrong / missing
  output and the encoding hasn't been figured out.
- A latent collection that's populated but never consumed (e.g. the
  `$25` RTL form's `ARecordPos`).

Format for new entries:

```markdown
### 6.N <Short title> (`GAP` | `UNCERTAIN` | `unused`)

[Source.pas:Lstart-Lend](path/to/file#Lstart-Lend) — one paragraph
describing what is known, what is not, and (if you have one) the
hypothesis to test next. Quote a specific binary sample or test
fixture when one exists.
```

Tag with `GAP` when the format itself is undecoded, `UNCERTAIN` when
the current code makes a guess that mostly works but isn't grounded,
and `unused` when state is captured but no resolver reads it.

A §6 entry is **closed** in any of these cases:

1. The shape was decoded (most common case — new bytes parsed, new
   collection populated, new test pinning concrete values).
2. The hypothesis behind the entry was **refuted** — the gap was
   based on a wrong premise, and once you've shown it's wrong there's
   nothing left to decode (e.g. "sparse enums use a different `$03`
   shape" → linker actually emits no `$03` for them at all; `$25`
   already carries the data).
3. The residual concern is a **design choice** rather than a missing
   decode (e.g. a synthetic record could be built but the existing
   API is intentionally narrow). Document the choice in §4 and the
   entry no longer belongs in §6.

In all three cases: **remove the §6 entry entirely**, move the
relevant explanation into the appropriate §4 subsection (so a reader
finding the format reference still learns the answer), and never
leave a "(`unused`) — still kind of a gap" stub behind. The §6 list
must stay tight: only actually-open questions live there.

Never silently drop a section — note the closure in your reply (the
commit message + the end-of-turn summary), so the user knows the §6
list shrank.

### 4. Every finding gets a unit test

The truth about RSM lives in `.rsm` byte streams. A claim that isn't
backed by a passing test is not yet knowledge. Whenever you decode a
new shape, fix a misparse, or change the meaning of a captured field:

- Add a test to one of:
  - [Projects/DPT/Test/Test.DPT.Rsm.Scanner.pas](../../../Projects/DPT/Test/Test.DPT.Rsm.Scanner.pas)
    — direct scanner output, pre-post-process.
  - [Projects/DPT/Test/Test.DPT.Rsm.Reader.pas](../../../Projects/DPT/Test/Test.DPT.Rsm.Reader.pas)
    — facade lookups after the post-process passes ran.
  - [Projects/DPT/Test/Test.DPT.Rsm.LocalsReader.pas](../../../Projects/DPT/Test/Test.DPT.Rsm.LocalsReader.pas)
    — wider behavioural surface (locals, fields, inheritance, perf).
  - [Projects/DPT/Test/Test.DPT.Rsm.Model.pas](../../../Projects/DPT/Test/Test.DPT.Rsm.Model.pas)
    — only for tag-constant pins / model invariants.
- Pin a concrete fixture value (e.g. "TLightStatus's ordinal 2 is
  `lsGreen`") rather than a vague existence check whenever possible.
- Both Win32 and Win64 fixtures exist (`DebugTarget.exe` under
  `Projects/DPT/Test/Win32/` and `Win64/`). If the encoding is
  platform-specific, write paired tests using the `DoTest...` /
  `AUse64Bit` pattern already established in `LocalsReader`.
- Run the tests before declaring the work done. The fixture binaries
  must be rebuilt with `-V -VR` for the .rsm sidecar to exist;
  `TestRsmFilePresent` is the guard.
- **Always build via the project's batch files**, not raw
  `msbuild`. The batches drive the same RECENT-based build host the
  user uses, so any failure you see is the failure they will see:
    * [Projects/DPT/Test/_Test.DptDebugger.BuildAndRun.bat](../../../Projects/DPT/Test/_Test.DptDebugger.BuildAndRun.bat)
      — builds + runs **both Win32 and Win64** in sequence. Use as
      the final pre-commit check.
    * [Projects/DPT/Test/_Test.DptDebugger.Build.bat](../../../Projects/DPT/Test/_Test.DptDebugger.Build.bat)
      — builds both platforms but does NOT run. Use when iterating
      on a single test (see "Running a single test" below) so each
      iteration is "build, then run only the one test".
    * [Projects/DPT/Test/_Test.DPT.BuildAndRun.bat](../../../Projects/DPT/Test/_Test.DPT.BuildAndRun.bat)
      — Win32-only build+run of the broader `Test.DPT.dproj`. Faster
      iteration loop when you know the change is platform-agnostic.

  Output binaries (referenced below as `<TestExe32>` / `<TestExe64>`):
    * Win32: [Projects/DPT/Test/Win32/Debug/Test.DptDebugger.exe](../../../Projects/DPT/Test/Win32/Debug/Test.DptDebugger.exe)
    * Win64: [Projects/DPT/Test/Win64/Debug/Test.DptDebugger.exe](../../../Projects/DPT/Test/Win64/Debug/Test.DptDebugger.exe)
    * The matching `Test.DPT.dproj` build also produces
      `Win32/Debug/Test.DPT.exe` and `Win64/Debug/Test.DPT.exe`.
  Both directories are gitignored, so any side-effect files (the
  diagnostic dumps documented below, the `.map` / `.rsm` sidecars,
  the `DebugTarget.exe` fixture) live alongside the test exes
  without ending up in commits.

### Stale-binary trap (silent prebuild failure)

The test exe builds via `Test.DptDebugger.dproj`, which has
`DebugTarget.dpr` + the other targets wired into a PreBuild event.
**A compile failure in DebugTarget.dpr does not abort the dproj
build** — the RECENT host treats it as a non-fatal warning, the
test exe links against the previous build, and the tests then run
against the **stale** `.rsm` produced by the prior successful
DebugTarget compile. The batch's tail still reports
"BuildAndRun completed" with green test counts.

This bit me once in this session: I added a `published Integer`
field to `TNoFPrefixHost` (Delphi rejects this with `E2217`),
then ran `BuildAndRun.bat`, saw "139/165 passed" and confidently
diagnosed against the OLD `.rsm`. I noticed only because a probe
that should have been present returned "NOT FOUND" — without that
sentinel value the false-positive would have shipped.

**Workflow guard**: after any change to `DebugTarget.dpr`, before
trusting the test results, compare mtime:

```
ls -la Projects/DPT/Test/Win32/DebugTarget.rsm \
      Projects/DPT/Test/DebugTarget.dpr
```

If the `.rsm` is older than the `.dpr`, the prebuild failed
silently — scan the batch log for `error E` to confirm. Build
errors live in the middle of the log, not the tail.

The same trap exists for fields you add to test code: a compile
error in `Test.DPT.Rsm.Scanner.pas` aborts the test-exe link, but
the previous test-exe stays in place, and a subsequent re-run of
the batch would use it. Watch for `error E2003` etc. lines and
the "Build successful" / "Build failed with exit code 1" tail
line.

### Running a single test (DUnitX filtering)

DUnitX accepts a name filter via `--run:<value>` and an alternative
file-based form `--runlist:<file>`. The trick is the matcher uses
the **full unit-qualified name**, including the `Test.` prefix the
unit declarations carry. Three usable forms:

1. **Exact test FQN** —
   `--run:Test.DPT.Rsm.Scanner.TRsmScannerTests.TestProcsCollected32`
   Runs that single test. Note the **leading `Test.`** is mandatory;
   the unit is `Test.DPT.Rsm.Scanner`, not `DPT.Rsm.Scanner`. This
   was the trap that made me think `--run` was broken.
2. **Fixture prefix** —
   `--run:Test.DPT.Rsm.Scanner.TRsmScannerTests`
   Runs every test in that fixture (matched via `StartsText` against
   each test's fixture full-name).
3. **Comma-separated list** —
   `--run:Test.DPT.Rsm.Scanner.TRsmScannerTests.TestX,Test.DPT.Rsm.Scanner.TRsmScannerTests.TestY`
   Combines multiple exact / prefix entries. Wrap in double quotes
   in cmd to keep the comma intact.
4. **File-based** — `--runlist:<file>` reads one name per line and
   applies the same parser. Useful when the filter list is large or
   when shell quoting becomes painful.

Always pair with `--consolemode:Quiet --exit:Continue` for compact
output in iteration loops. Source of truth for the matcher: the
`TNameFilter.Match` method in
`C:\Program Files (x86)\Embarcadero\Studio\37.0\source\DunitX\DUnitX.Filters.pas`.

Iteration workflow:

```
Projects\DPT\Test\_Test.DptDebugger.Build.bat
Projects\DPT\Test\Win32\Debug\Test.DptDebugger.exe ^
    --run:Test.DPT.Rsm.Scanner.TRsmScannerTests.TestFoo ^
    --consolemode:Quiet --exit:Continue
```

Same for Win64: substitute `Win64\Debug\Test.DptDebugger.exe` for
the second line.

Once the targeted test passes, run the full `_Test.DptDebugger.BuildAndRun.bat`
to confirm no other test regressed before committing.

If you cannot create a meaningful test (e.g. the fixture lacks a
suitable Delphi construct), extend `DebugTarget.dpr` to add the
construct, rebuild, and only then assert. Document the fixture
addition in the test's docstring so future readers see why it's there.

### Extending `DebugTarget.dpr`: breakpoint-line discipline

The `.dpr` file is also the debugger fixture: several test suites
reference specific source lines as hard-coded breakpoint targets.

* `Test.DPT.Debugger.pas` has `LocalsBreakpointLine = 45`.
* `Test.DPT.MCP.Server.pas` hard-codes lines like `15`, `19`, `45`
  inside JSON payloads (`"line": 45`).
* Inside the `.dpr` itself, every BP target is marked with a
  `// Line N - <description> bp here` comment. Current markers sit
  at lines `45`, `209`, `217`, `231`, `241`, `250`, `281`, `377`,
  `430`.

**Before any insertion**, run
`Grep("bp here|Line \\d+ -", path="Projects/DPT/Test/DebugTarget.dpr")`
to refresh the marker list. **Always append the new construct AFTER
the last marker** (currently line 430) — inserting earlier shifts the
markers and silently breaks the breakpoint tests. If the construct
needs to live next to existing types (because of ordering
dependencies), introduce it as a new `type ... var ...` block
*after* the last `procedure ... end;` of the BP-bearing region rather
than amending the upper-file type block.

When in doubt, run `_Test.DptDebugger.BuildAndRun.bat` after the
edit — a shifted BP marker will surface as a missed-breakpoint
failure, not a silent skip.

---

## Standard workflow

When the user asks an RSM-related question:

1. **Read the reference doc first**. Treat its §4 (per-record
   encoding) and §6 (gaps) as your working memory.
2. **Cross-check against the code**. Open the relevant
   `DPT.Rsm.*.pas` file and verify each claim you are about to make
   sits in the code. Where comments and code differ, code wins.
3. **Spot the gap**. If the user's question touches an `UNCERTAIN` or
   `GAP` entry, surface that explicitly — don't pretend the answer is
   solid when it is heuristic.
4. **Act**. Either answer (with file-path:line references), or
   implement (with test).
5. **Update the doc**. New finding? Add or remove a §6 entry, or
   extend the relevant §4 subsection. Use the section's existing
   structure verbatim.
6. **Test**. Add the test that pins your finding. Run it.
7. **Summarise**. End your reply with: doc sections touched + test
   name added + status (passing / failing / TBD).

---

## When investigating a new shape (reverse engineering)

A typical session: the user reports a misparse, or the doc lists a
`GAP` you want to close. Proceed in this order:

1. **Find a concrete sample**. A real `.rsm` byte run that exhibits
   the shape. `DebugTarget.rsm` (small, fully controlled) is
   preferable to `TFW.rsm` (large, real-world). When only `TFW.rsm`
   has a sample, add the construct to `DebugTarget.dpr` so future
   investigations have a small repro.
2. **Hex-dump the region** (the *diagnostic test*, see below). Use
   `TestRsmStartsWithCsh7Magic` as the template for byte-level reads.
   Print 32-64 bytes before and after the tag of interest with ASCII
   trailing.
3. **Identify the structural anchor**. RSM records have no length
   field, so every handler relies on a constant byte pattern (`$66
   $00 $00`, `$0A $00 $00`, `$8A $00 $00`, `$01 $00 $00 $00 $00`,
   `$02 $00 <flag> $00 $00 $00`, etc.). Find the equivalent for the
   new shape before writing the parser. Without an anchor the parser
   is just a guess.
4. **Map every byte**. Account for each byte in the payload window.
   "Unknown padding" is fine to call out, but call it out in the doc.
5. **Implement** — add a handler method or extend an existing one,
   keep the single-byte-fallback contract intact.
6. **Test** — assert a concrete recovered value, not a count.
7. **Doc** — write up the shape under the relevant §4 subsection
   using the established encoding-block style:

   ```
   ```
   $XX  <NL>  <Name>  <anchor bytes>  <payload>
   ```
   ```

   Then explain field-by-field with byte offsets.

### The diagnostic → pin → cleanup pattern

Reverse engineering follows a stable three-stage shape. Every step
has a different *kind* of test in the source tree, and the
transitions between them are the part that's easy to skip.

#### Stage 1 — Diagnostic test (broad, exploratory, file-output)

The diagnostic is a `[Test]` whose job is to make the byte stream
*legible*: it walks broad areas, classifies what it finds, and writes
the result to a `.tsv` / `.txt` next to the fixture so you can read
the data back as a normal file.

* Place it in `Test.DPT.Rsm.Scanner.pas` next to the eventual pin.
* Output path: `ExtractFilePath(ResolveExePath(False)) +
  'rsm-<topic>-dump.tsv'`. The `Win32/` / `Win64/` fixture dirs are
  gitignored (no tracked files there), so dumps land out-of-tree.
* End with `Assert.Pass('Wrote ... to ' + Path)` so the test always
  passes; the artefact is the file, not the assertion.
* Keep the validator filters loose at first, then tighten when you
  see the false-positive density. Real type registry entries cluster
  in a specific file region; isolated hits elsewhere are noise.
* The fastest iteration on the diagnostic itself is the single-test
  loop documented in "Running a single test" above — build once via
  `_Test.DptDebugger.Build.bat`, then re-run the diagnostic in
  isolation via `--run:Test.<unit>.<fixture>.<TestName>`. Earlier
  notes that claimed `--run` was broken were wrong; the trap was the
  missing `Test.` unit-name prefix.

After running, read the dump file with the `Read` tool (or `Grep` /
`awk` for filtering). Looking at the data is what produces the
finding — the diagnostic is just the lens.

#### Stage 2 — Pin test (narrow, concrete, no file output)

Once the dump reveals the pattern, *replace* the diagnostic with a
pinning test that asserts the concrete values you observed against
named fixture types. The pin is what survives in the repo.

* Same `[Test]` namespace and file, often a rename of the diagnostic
  (`TestDiagnose<X>` → `Test<X>FindingPinned`).
* No more file I/O — the dump path is gone, only `Assert.AreEqual` /
  `Assert.IsTrue` against bytes / counts / decoded values remain.
* Pin the *minimum* set of fixtures that disambiguates the finding
  (e.g. one class + one enum + one record for the `$2A` body-flag
  decoding), not every type you saw.
* The pin's docstring states the refuted/confirmed hypothesis and
  links back to the doc section. Future-you reads the pin to
  remember *what* was decoded; the doc section explains *why*.

#### Stage 3 — Cleanup (manual, mandatory)

The diagnostic's dump files are not test artefacts — they have to
be removed by hand before commit.

* `rm -f Projects/DPT/Test/Win32/rsm-<topic>-dump.tsv` (and Win64
  equivalent if you ran both).
* Verify with `git status --short Projects/DPT/Test/` — only your
  source files (`.pas`, `.md`, `.dpr`) should appear; nothing under
  `Win32/` / `Win64/` should be staged or tracked.
* If the dump path is mentioned anywhere in the doc as part of an
  example, replace it with the test name instead — the file no
  longer exists.

#### Worked examples in the current branch

* `$2A` body-flag investigation: diagnostic walked every $2A entry
  starting with `T` into a `.tsv`, then pinned via
  `Test2ATypeRegistryFlagIsBodyShapeNotKind32` (commit `bf070fd`).
* Sparse-enum investigation: diagnostic dumped `$03` candidates and
  the matching `$25` entries; pin became
  `TestSparseEnumResolvesViaEnumConstNames32` (commit `a9d6361`).
  Both commits show the diagnostic-then-pin transition as a single
  changeset.

### PowerShell for raw byte access — two footguns

PowerShell is a tempting first reach for quick byte-stream probes
("just find the offset of this name in the .rsm"), but two traps
turn 5-second tasks into 5-minute or 5-hour ones on the project's
real fixtures. Both have bitten this session.

**1. Indexed byte-array loops are ~1000× slower than
`String.IndexOf`.** A `for ($i = 0; $i -lt $bytes.Length; $i++) {
if ($bytes[$i] -ne 0x28) { ... } }` loop traverses each byte
through the PSObject wrapper layer at ~1-10 µs per access. On the
800 MB `TFW.rsm` that's 20 minutes per name; on the 1.17 GB
`TFW.Win64.Debug.rsm` it's 2-18 hours. **Convert to a string once
and use `.IndexOf` (.NET native, SIMD-accelerated where
available):**

```powershell
$bytes = [System.IO.File]::ReadAllBytes($rsm)
$text  = [System.Text.Encoding]::GetEncoding('ISO-8859-1').GetString($bytes)
foreach ($n in $names) {
    $needle = [char]0x28 + [char]$n.Length + $n
    $idx = $text.IndexOf($needle)   # microseconds, not minutes
    ...
}
```

ISO-8859-1 gives a lossless 1:1 byte↔char mapping; `$text.Length`
equals `$bytes.Length` and the returned offset is the file
offset.

**2. `GetString` OOMs above ~1 GB.** PowerShell 5.1 strings are
.NET UTF-16, so the converted string occupies 2× the byte-array
size. The 1.17 GB `TFW.Win64.Debug.rsm` yields a 2.3 GB string
and trips `System.OutOfMemoryException` even on 64-bit
PowerShell. The trick from footgun #1 still works for the 800 MB
Win32 `TFW.rsm`, but for anything bigger:

* Drop PowerShell entirely and write the probe in Pascal as a
  `[Test]` in `Test.DPT.Rsm.Tfw.pas`. Pascal's native byte
  indexing is fast and the `Reader.Scanner.ByteAt` /
  `Reader.Scanner.Sz` accessors give zero-overhead reads. The
  test runs in the same process the user runs anyway, so no
  extra environment setup.
* Or use a small C# scanner if the investigation is throwaway.

The user has flagged the byte-loop trap explicitly more than once
("PS-Loop schmeisst byte-Array durch object-wrapper layer; nutz
String.IndexOf"). Treat any PowerShell `for`-loop over `$bytes`
that touches a multi-MB region as wrong by default.

### Multi-round delegation for hard gaps

Some §6 gaps need more than one investigation pass. §6.9
(FieldId → Enum binding) needed **four investigation rounds**
plus a separate implementation round before closing. Each round
refuted hypotheses from the prior round and surfaced the next
closest-shape lead. The pattern that worked:

1. **Investigate-only delegation.** Spawn a fresh agent with the
   rsm-expert skill, brief it on what the previous round
   refuted, point at concrete byte offsets, and **explicitly
   restrict the scope to a written report — no code changes, no
   pin tests, no commits.** The agent's deliverable is bullet-
   list findings plus a "next investigator's lead" or
   "deferred" recommendation.
2. **Document every refutation in §6 immediately.** When an
   agent reports "I checked hypothesis X, here's why it's
   wrong", that bullet goes into the gap entry. The next round's
   agent reads it and doesn't re-walk the dead end. Over §6.9's
   four rounds the entry grew six refutation bullets — every
   one saved the next agent an hour.
3. **Escalate to "investigate + fix" only after a positive
   finding.** Round 4 of §6.9 produced a working bridge ("byte
   +3 = secondary-LOW byte"). Only then did the next round get
   permission to write production code. The earlier rounds
   would have produced "fixes" that didn't actually fix
   anything.
4. **Round budgets per agent.** Tell the agent in the prompt
   to stop after ~30-45 minutes if both hypotheses are
   inconclusive. A confident "no" closes a lead; a hopeful
   "maybe" wastes the next agent's time. The skill already
   states this ("confident negative > hopeful maybe") but
   it bears repeating for multi-round investigations.
5. **Confirm-then-implement.** Before the fix-round agent
   commits anything, it must satisfy two gates: (a) every
   prior round's refutations stay refuted, (b) the new pin
   asserts byte-exact behaviour on ≥2 probes. §6.9's round 5
   implementation skipped same-unit gating and was reverted
   because the global LOW-byte map collided in TFW; round 6
   re-added per-unit scoping via file-offset proximity (the
   §6.10 block-owner pattern) and landed.

The Format.md §6 entry doubles as the **investigation log**.
Don't shrink it to "deferred" until either the gap closes or
the cost-to-payoff has clearly tilted away from chasing it.
A long entry with five refuted hypotheses is far more useful
to the next investigator than a one-line "this is hard, skip".

---

## When the user changes RSM code

Treat every commit touching `DPT.Rsm.*.pas` as a potential doc-drift
event. After the change:

1. Re-read the modified file.
2. Re-read §4/§5/§6/§8 of the reference doc.
3. Patch the doc wherever the new code disagrees with it.
4. If the change closed a gap, remove the §6 entry.
5. If the change introduced new heuristic windows / sub-forms, add a
   §6 entry describing what is still unknown.
6. **Drift-check every line range the doc cites into the file you
   changed.** Format.md is full of `[Scanner.pas:N-M]
   (DPT.Rsm.Scanner.pas#L_N-L_M)` references. Inserting code anywhere
   above a cited range shifts every range below it — green tests are
   no proof the refs still point at the right thing. Workflow:
   - `Grep("\\.pas:\\d+", path="Projects/DPT/Source/DPT.Rsm.Format.md", -n=true)`
     to list every ref into the changed file.
   - For each ref, read the cited range with the `Read` tool and
     verify it still contains the content the surrounding doc claims.
     Don't trust an Explore agent to do this in bulk — the agent
     readily marks "near enough" hits as ✓ and you only catch the
     drift when re-reading the actual code. (Background: a single
     audit pass on this branch found 21 Scanner.pas refs drifted by
     16-68 lines; agent had flagged most as OK.)
   - Patch the refs in the same commit as the code change so
     reviewers see one coherent diff.

The doc is **not optional documentation**, it is part of the unit's
public contract. A code change without a doc update — including a
line-range drift fix — is incomplete work, the same way a code
change without a test is.

---

## Don'ts

- Don't invent record shapes from the comments alone. Comments
  routinely lag behind code — always confirm in the source.
- Don't write defensive code in the parsers without a fixture that
  triggers the defensive branch. The single-byte-fallback contract
  already absorbs structural noise; adding more guards mostly hides
  bugs.
- Don't tighten the `IsPrintableAscii` charset without checking the
  whole identifier alphabet (dotted names, generics with `<>` and
  `,`, `$ActRec` closures, `@`-aliases). The current charset is
  documented in [DPT.Rsm.BufferIO.pas:74-98](../../../Projects/DPT/Source/DPT.Rsm.BufferIO.pas#L74-L98).
- Don't change `TRsmTag` constants. Their values are wire-format —
  `Test.DPT.Rsm.Model.TestTagConstants` pins each one and a typo
  silently breaks every dispatch.
- Don't lose the `FScanSeenLocalSinceProc` guard around `SCOPE_END`.
  Incidental `$63` bytes in proc-address payloads will silently close
  scopes prematurely without it — that's the bug it was added to fix.
- Don't introduce a new mORMot dependency or System.Generics import.
  The unit uses mORMot `IList<T>` / `IKeyValue<K,V>` collections by
  convention; see the user's memory note on this.

---

## Quick-reference pointers

Tag dispatch dispatcher: [DPT.Rsm.Scanner.pas:1069-1146](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L1069-L1146)

Per-tag handlers:

| Tag    | Handler                                                                                                |
|--------|--------------------------------------------------------------------------------------------------------|
| `$28`  | `HandleProcRecord` + `DecodeProcAddrPayload` ([Scanner.pas:342-522](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L342-L522)) |
| `$22`  | `HandleParamRecord` ([Scanner.pas:524-584](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L524-L584))                          |
| `$21`  | `HandleRegVarRecord` ([Scanner.pas:586-625](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L586-L625))                         |
| `$20`  | `HandleLocalRecord` ([Scanner.pas:627-713](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L627-L713)) / `HandleModuleGlobalLocalTagRecord` ([715-737](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L715-L737)) |
| `$27`  | `HandleGlobalPrimRecord` ([Scanner.pas:739-789](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L739-L789))                      |
| `$25`  | `HandleEnumConstantRecord` ([Scanner.pas:791-915](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L791-L915))                   |
| `$03`  | `HandleEnumDefRecord` ([Scanner.pas:917-997](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L917-L997))                        |
| `$2A`  | `HandleTypeRegistryRecord` ([Scanner.pas:999-1067](../../../Projects/DPT/Source/DPT.Rsm.Scanner.pas#L999-L1067)) + `ScanTypeRegistry` ([FormatALinker.pas:458-513](../../../Projects/DPT/Source/DPT.Rsm.FormatALinker.pas#L458-L513)) |
| `$2C`  | `LinkFieldsFromFormatA` ([FormatALinker.pas:515-707](../../../Projects/DPT/Source/DPT.Rsm.FormatALinker.pas#L515-L707))           |

Class trailer / record sentinel discovery:
[DPT.Rsm.StructDiscoverer.pas:448-655](../../../Projects/DPT/Source/DPT.Rsm.StructDiscoverer.pas#L448-L655)

Post-process pipeline (reader-orchestrated):
[DPT.Rsm.Reader.pas:367-392](../../../Projects/DPT/Source/DPT.Rsm.Reader.pas#L367-L392)

Tag constants:
[DPT.Rsm.Model.pas:159-201](../../../Projects/DPT/Source/DPT.Rsm.Model.pas#L159-L201)

Test fixtures:
- `DebugTarget.dpr` — small, fully controlled. Extend this when you
  need a new shape exposed.
- `Projects/DPT/Test/Win32/DebugTarget.exe` + `.rsm` — Win32 build.
- `Projects/DPT/Test/Win64/DebugTarget.exe` + `.rsm` — Win64 build.
- `C:\MSE\TFW\TFW.exe` + `.rsm` (dev-machine only) — large real-world
  corpus. Tests that depend on it `Assert.Pass` when the fixture is
  missing.

---

## Behavioural pledge

When you accept this skill, you take ownership of the format. That
means:

- You read the doc before you answer.
- You cross-check the code before you write the doc.
- You pin every claim with a test.
- You leave the doc a little more complete than you found it.

If you cannot do one of the above for the user's current request,
say so explicitly rather than degrading the contract.
