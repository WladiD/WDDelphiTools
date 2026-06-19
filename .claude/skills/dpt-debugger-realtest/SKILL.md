---
name: dpt-debugger-realtest
description: Run an extensive real-world test of the DPT MCP debugger by debugging the large TFW VCL application while a FitNesse acceptance suite drives it through real code paths. The skill carries a catalog of named test cases (default `suite-smoke` for the end-to-end debugger smoke test; `enum-evaluate` for the dotted-walk evaluator chain via `Self.FAd.Land` on TFormAd; `fixture-result-eval` for evaluating a Slim fixture's `Result` and the property-vs-field evaluator gap via `Result.Caption`). Activate when the user wants to "test the DPT debugger", run a "Debugger-Echttest"/"real test"/"umfangreicher Test" of the MCP debugger, validate the mcp__DPT__* tools end-to-end against TFW, reproduce the debugger workflow (start session → run a FitNesse suite as workload → set breakpoints → inspect → teardown), or run a specific named test case from the catalog. Also activate when a FitNesse suite "hangs" while TFW runs under the debugger, or when the user asks "which tests are available" / "list tests".
user-invocable: true
---

# DPT MCP Debugger — Real-World Test

This skill drives an **end-to-end real test of the DPT MCP debugger**. The
system under test (SUT) is **TFW.exe**, a large (~120 MB) Delphi/VCL
application with `.map` + `.rsm` sidecars. TFW embeds a Slim server
(FitNesse fixture host) on TCP **port 9000**. We let the **debugger launch
TFW** (so the debugger controls it from the entry point), then run a
**FitNesse acceptance suite** as the workload that exercises TFW's UI
through huge amounts of real code — the ideal stress test for breakpoints,
named locals, stack traces, exception handling and the
`wait_until_paused` loop.

The whole flow has been verified live. Follow it exactly; the **expected
results** columns let a future run detect real regressions, not just
"it ran".

## Test cases (catalog)

The skill carries multiple named test scenarios. They all share the
same `Fixed coordinates`, `Pre-flight`, `Build`, and teardown
discipline — only the breakpoint(s), the at-pause inspection, and
the reporting differ.

| Name | What it validates | Workload | Run by default? |
|---|---|---|---|
| `suite-smoke` | End-to-end debugger contract: session launch from entry point, pending → active breakpoint resolution at `Tfw.Ad.Form:1120` (`TFormAd.Create`), `ignore_exception` (Delphi `EPrinter`) + `set_break_on_native_first_chance(false)` for native printer/Wow64 noise, `wait_until_paused` orchestration loop, suite finishes 7/0, teardown frees port 9000. The big regression net for any change touching `DPT.Debugger.pas` / `DPT.MCP.Server.pas`. | Adressen FitNesse suite (`ATDD.Master.Stammdaten.Adressen`) | **Yes** (default) |
| `enum-evaluate` | The dotted-walk evaluator chain `class → pointer-to-record → enum-field`. Canonical TFW probe: `evaluate Self.FAd.Land` inside `TFormAd.TblAdDataChange`. Reports per-link verdict (which segments resolve through the evaluator vs. which still need the manual `read_memory` walk). Use after closing any §6 entry that should unblock another link in the chain. | Adressen FitNesse suite (triggers `TblAdDataChange` during `AdresseAnlegen`/`AdresseEditieren`) | No (opt-in only) |
| `fixture-result-eval` | Evaluating a function's `Result` on a Slim fixture, and **published-property resolution via live RTTI**. BP at `Tfw.Playground.SlimFixtures:97` (the `end;` of `TSlimFormCreateFixture.CreateAdForm`, after `Result:=Screen.FocusedForm as TFormAd`). Probes: `evaluate Result` (object → `TFormAd @ <hex>`, GREEN), `evaluate Result.Caption` (published property → GREEN, the form title e.g. `001- Adresse suchen`, resolved the way the IDE does — VMT runtime RTTI → `GetProc` → getter **call injection**; §4.16/§6.37), `evaluate Result.cmLst.Caption` (intermediate object hop + published property → GREEN, expected `Leistungen`), and `evaluate Result.IniName` (a **`public`** (non-published) property → currently **RED gap**: `Could not navigate`; ground-truth `TFormAd` — the published-RTTI walk can't reach public properties). Regression net for the property-evaluation path (RTTI walk + call injection). | Playground FitNesse test (`Playground.CodeCafe.SampleA`, mode `test`) — drives `CreateAdForm` | No (opt-in only) |

### Invocation rules

- **No arg / bare `/dpt-debugger-realtest`** → run `suite-smoke`.
- **`/dpt-debugger-realtest list`** (or the user asks "which tests
  are available", "list tests", "welche Tests gibt es") → reply
  with the catalog above. Do **not** run anything.
- **`/dpt-debugger-realtest <name>`** (e.g.
  `/dpt-debugger-realtest enum-evaluate`) → run that specific
  test only.
- **Adding new test cases**: each new scenario goes into the
  catalog table AND gets its own `## Test case: <name>` section.
  Cases default to opt-in (`Run by default? = No`) unless they're
  cheap regression-net additions to `suite-smoke`.

## Why the debugger must launch TFW (not BuildAndRun)

The fitnesse-test skill normally starts TFW *detached* via
`DPT.exe ... BuildAndRun ... --NoWait`. That is wrong here: the MCP
debugger has to start the process itself (`start_debug_session`) so it is
attached from the entry point. So we only **Build** (never BuildAndRun),
then launch through the debugger.

## Fixed coordinates

| Item | Value |
|---|---|
| Source root | `C:\Source\` |
| Project file (.dproj) | `C:\Source\TFW\PAS\TFW.dproj` |
| LOGIN params file | `C:\Source\LIB\PAS\TAIFUN.params` (node `<Param Name="LOGIN">`) |
| DPT.exe (the MCP-server / CLI) | `c:\WDC\WDDelphiTools\Projects\DPT\DPT.exe` |
| FitNesse runner | `C:\Source\TEST\FitNesse\RunFitnesse.bat` (run with cwd = `C:\Source\TEST\FitNesse`) |
| Slim server port | `9000` |
| Default workload suite | `ATDD.Master.Stammdaten.Adressen` |

**Do not hardcode the SUT path or the LOGIN token** — both are resolved at
runtime (steps 1–2). The LOGIN value is a credential; never write it into
files, the skill, or commits. The DPT subcommand prefix `D12` selects the
Delphi 12 toolchain.

## Test case: `suite-smoke` (default) — procedure (verified end-to-end)

### 1. Resolve the SUT path (build output of the .dproj)

```
DPT.exe D12 DProjPrintOutputFile C:\Source\TFW\PAS\TFW.dproj
```
The **last output line** is the SUT path (e.g. `C:\MSE-26.04-Mongo\TFW\TFW.exe`).
An earlier "Workflow file NOT found…" line is harmless noise. Confirm that
`TFW.map` and `TFW.rsm` sit next to the resolved `TFW.exe`.

### 2. Read the LOGIN argument

Read `C:\Source\LIB\PAS\TAIFUN.params`, take the text of
`<Param Name="LOGIN">` (form `PASS:<hex>`). This is the single command-line
argument passed to TFW.

### 3. Pre-flight

- Port 9000 must be **FREE** (no stray TFW). PowerShell:
  `if (Get-NetTCPConnection -LocalPort 9000 -ErrorAction SilentlyContinue) { "LISTENING" } else { "FREE" }`
  If LISTENING, a previous TFW is still up — stop it first.
- `mcp__DPT__get_state` must be `no_session`. If not, terminate the old session.

### 4. Build (only if changed)

```
DPT.exe D12 Build C:\Source\TFW\PAS\TFW.dproj --OnlyIfChanged
```
Expect `Executable is up to date. Skipping build.` or a successful build.

### 5. (Optional) Queue breakpoints before the session

`mcp__DPT__set_breakpoint(unit, line)` — max **4** hardware breakpoints, may
be set while `no_session` (status `pending`, becomes `active` on start).
A verified, reliably-hit location is
`Tfw.Ad.Form` line **1120** (`KonsMisSearchLoad;`, first executable
statement of `TFormAd.TFormAd.Create` after `inherited`) — the Adressen
suite creates this form. Source lives at `C:\Source\TFW\PAS\Tfw.Ad.Form.pas`.

### 6. Start the debug session

```
mcp__DPT__start_debug_session(executable_path = <SUT from step 1>,
                              arguments       = <LOGIN from step 2>)
```
Process launches **paused at entry point**. `list_breakpoints` should now
show queued breakpoints as `active` with a resolved address.

### 7. Neutralize operational exceptions, then continue

TFW raises first-chance exceptions during normal operation. The big one is
**`EPrinter`** (no default printer) raised while enumerating fonts when a
RichText control is streamed from a DFM — e.g. when `TFormAd` is created.
If the debugger halts there, the embedded Slim server stops responding and
the FitNesse run *looks* hung.

- `mcp__DPT__ignore_exception("EPrinter")` (`EAbort` is ignored by default)
  suppresses the **Delphi** `EPrinter` raised by `TPrinter`.
- The printer/font enumeration also surfaces **native (non-Delphi)
  first-chance exceptions** (winspool / printer-driver code) that
  `ignore_exception` cannot touch — it only matches Delphi exception
  classes. Two ways to get past those:
  - `mcp__DPT__set_break_on_native_first_chance(enabled=false)` — the
    robust choice for a workload run: the debugger stops pausing on
    routine native first-chance exceptions (second-chance/unhandled ones
    still pause). **Set this before `continue` for a suite run.**
  - or `mcp__DPT__ignore_exception_code("C0000005")` to suppress one
    specific native code.
- `mcp__DPT__continue`.

> **Why this matters (root-caused in `DPT.Debugger.pas`):** the
> `ignore_exception` class list is only consulted for Delphi language
> exceptions (`ExceptionCode = $0EEDFADE`). A native/OS exception has a
> different code (e.g. `C0000005` access violation, `E06D7363` C++ EH),
> carries no Delphi class name, and therefore can **never** be suppressed
> by class — it always pauses unless `break_on_native_first_chance` is off
> or its code is ignored. (Earlier this looked like a "pending vs. live
> ignore" timing issue; that was a red herring — the real axis is
> Delphi-class vs. native-code.)

### 8. Wait for the Slim server

Poll port 9000 until `LISTENING` (came up within ~1 s, give it up to ~40 s).
Then `get_state` should be `running`.

### 9. Start the workload suite **in the background**

cwd must be `C:\Source\TEST\FitNesse`. Two-argument (shell-safe) form:

```
RunFitnesse.bat <Suite-in-dot-notation> suite
```
e.g. `RunFitnesse.bat ATDD.Master.Stammdaten.Adressen suite`.

Run it **in the background** (it can take minutes). When launching via the
Bash tool / cmd.exe, first `unset NoDefaultCurrentDirectoryInExePath`.
Capture the output file path for polling.

### 10. The `wait_until_paused` orchestration loop ← the heart of the test

While the suite runs, the debugger may halt at any time for **two distinct
reasons**. Loop: call `mcp__DPT__wait_until_paused` repeatedly and branch on
the result.

The paused state now tells you the reason directly. `get_state` /
`wait_until_paused` include **`paused_reason`** (`"breakpoint"` or
`"exception"`), and for exceptions also **`exception_code`** (hex),
**`first_chance`** (bool) and **`exception_class`** (for Delphi exceptions
only; empty for native ones). Branch on that — no need to infer from
`unit=""`.

- **Still `running`** (timeout): the suite is progressing. Peek at the
  background output file; if the suite finished and the debugger is no
  longer paused, go to teardown.
- **`paused_reason = "exception"`** → decide by `exception_code`:
  - code `0EEDFADE` (Delphi) with an `exception_class` → if it's
    operational noise, `ignore_exception(<class>)`, then `continue`.
  - any other code (native, e.g. `C0000005`) → `ignore_exception_code(<code>)`
    or, better for a whole suite run, `set_break_on_native_first_chance(false)`,
    then `continue`. `get_stack_trace` still shows where it came from
    (e.g. `Vcl.Printers.TPrinter.SetToDefaultPrinter`).
- **`paused_reason = "breakpoint"`** with a real `unit`/`line` (e.g.
  `Tfw.Ad.Form` / `1120`, on a real worker thread id) → inspect, then
  `continue`:
  - `get_stack_trace` — named TFW + RTL frames.
  - `get_locals` — named locals from the `.rsm`. Entries with real names
    (`Self`, `AOwner`, and the procedure's declared locals) are the signal;
    the many `"."` entries are unnamed/heuristic slots — ignore them.
    Interpret `hex` per the Delphi type (first 4 bytes for Integer, all 8
    for Int64/Pointer/Double).
  - Optionally `get_registers`, `get_stack_slots`, `read_memory`,
    `step_over`/`step_into`.

Keep shepherding until the background suite task reports completion **and**
the debugger is not paused. If you stop calling `wait_until_paused` while
the process is paused, the Slim server stalls and the suite never finishes.

### 11. Evaluate the suite result

The background output ends with a summary line:
`N Tests, M Failures   <seconds>.` Exit code `0` = all passed. A clean run
of the Adressen suite is **7 Tests, 0 Failures** (SuiteSetUp +
AdresseAnlegen/Editieren/Loeschen/Suchen + AnsprechpartnerAnlegen +
KontakteintragAnlegen). Per-page counts are `R:right W:wrong I:ignored
E:exceptions`. For detail, FitNesse also writes XML under
`C:\Source\TEST\FitNesse\FitNesseRoot\files\testResults\<page.dot.path>\`
(filename encodes `_right_wrong_ignored_exceptions.xml`).

> Wall-clock time inflates by exactly the time the process spent **paused**
> at breakpoints/exceptions (e.g. SuiteSetUp showed 177 s / 237 s in runs
> where we sat on a pause). That is expected, not a TFW slowdown.

### 12. Teardown

After the suite completes the debugger typically halts again on a
post-suite exception — that's fine. End the session:
`mcp__DPT__terminate_debug_session` (kills the process; state → `no_session`).
Confirm port 9000 returns to FREE.

## Test case: `enum-evaluate` (opt-in)

Validates the dotted-walk evaluator chain on a real
**class → pointer-to-record → enum-field** traversal. The canonical
TFW probe is `evaluate Self.FAd.Land` inside
`TFormAd.TblAdDataChange`. This case is the user-visible payoff of
several §6 closures (the `Self`-spill fix, the §6.16 strict-private
field capture, the §6.18 pointer-to-record fallback, the §6.19
`PAd → TAd` pointer-alias bind, and the §6.24 `Land → TLandTyp`
enum name-convention bind that makes the bare auto-detect resolve
the enum name); run it after any of them lands and the per-link
verdict shows which segment(s) flipped green.

### Anchors

| Element | Detail |
|---|---|
| Class | `TFormAd` (`C:\Source\TFW\PAS\Tfw.Ad.Form.pas`) |
| Field | `FAd: PAd` (strict-private pointer-to-record) |
| Record | `TAd` |
| Leaf field | `Land: TLandTyp` |
| Enum declaration | `TLandTyp = (ltInland, ltAusland, ltEUNetto, ltEUBrutto, ltBLNetto)` — declared at `C:\Source\Base\Base.Types.pas:966` |
| Breakpoint | **`Tfw.Ad.Form:2274`** — the `case FAd.Land of` line; first executable statement of `TFormAd.TblAdDataChange` that reads `FAd.Land` |
| Workload to reach BP | Adressen FitNesse suite — `TblAdDataChange` fires during `AdresseAnlegen`/`AdresseEditieren` (well within ~1 min of suite start) |
| Memory offsets (verified live, build-stable) | `FAd = [Self + 0x0C5C]` (`PAd`, 4 bytes Win32 / 8 bytes Win64); `Land = byte[FAd + 0x169D]` (1 byte). On the standard Adressen test data the byte is `0x00` = `ltInland`. |

### Procedure

Steps 1–8 of `suite-smoke` apply verbatim (resolve SUT, read LOGIN,
pre-flight port + state, build TFW, ignore `EPrinter` +
`set_break_on_native_first_chance(false)`, continue, wait for Slim
on port 9000). The differences are step 5 (which BP to queue) and
step 10 (what to do at the pause).

#### 5'. Queue the BP at the enum-evaluation site

```
mcp__DPT__set_breakpoint(unit="Tfw.Ad.Form", line=2274)
```

Don't queue `Tfw.Ad.Form:1120` for this test — that BP would fire
earlier on form creation and just slow the path to the real probe.

#### 9'. Start the Adressen suite in the background

Same command as `suite-smoke`. The suite RUNS but the suite result
(7/0) is not the deliverable here — the suite is just the workload
that drives TFW into `TblAdDataChange`.

#### 10'. At the pause: probe the chain, then walk memory

When `wait_until_paused` reports `paused_reason=breakpoint`,
`unit=Tfw.Ad.Form`, `line=2274`,
`procedure=Tfw.Ad.Form.TFormAd.TblAdDataChange`, on a real worker
thread id (the FMX/Slim worker), run the evaluate probes in order
(the leaf both bare and via `type=int`) and report each link's
verdict:

1. **`evaluate Self`** as `"object"` — must return
   `TFormAd @ <hex-addr>`. Validates the §4.2 spill-home recovery
   + the §4.14 dotted-walk priming (these closures land here as
   the foundation; everything downstream depends on them).

2. **`evaluate Self.FAd`** as `"int"` — must return a non-zero
   pointer value (the live `PAd`). Validates the §4.14 strict-
   private field capture (§6.16) and the §4.14 record-field
   dotted-walk plumbing. If this fails on a build where §6.16 was
   closed, suspect a TypeIdx regression.

   > **Heads-up on the signed/unsigned display.** The `int` formatter
   > prints the 4-byte pointer slot as a **signed** Int32, so when the
   > Win32 heap lands above `0x80000000` (high bit set, e.g. modern
   > ASLR placing TFW objects in the `0xE8xxxxxx` band) the decimal is
   > NEGATIVE (`-373215744` is `0xE9C12E00`, a perfectly valid heap
   > pointer) — not a decoder bug. **The `int`/`int64` formatter now
   > appends the raw hex pattern automatically**, so the result already
   > reads `-373215744 (0xE9C12E00)` — no hand conversion. (The hex
   > width follows the field's read size: a clamped Word/Byte shows
   > `0x0001`/`0xA5`.) Just cross-check that the `(0x…)` matches the
   > live `read_memory(<Self>+0x0C5C, 4)` bytes (LE) — they MUST
   > byte-match.

3. **`evaluate Self.FAd.Land`** — expected GREEN, **both** with an
   explicit `type=int` AND bare (auto-detect). The Format-A linker
   binds the `PAd → TAd` alias at scan time (canonical `P<X> = ^T<X>`
   convention), the `BindPointerAliasMembersByNameConvention`
   post-process pass populates `Member.PointerTargetTypeIdx` on `FAd`
   (strict-private fields in TFW have no `$2C` record, so the
   convention bridge is required), the dotted-walk's inter-segment
   priming dereferences in place between segments (gated to
   non-terminal segments only — the terminal-segment guard keeps
   `evaluate Self.FAd (int)` returning the pointer value rather than
   the first DWORD of the dereferenced record), and the §6.18
   name-based fallback (with its `FbMatchCount > 1` bail on TFW's
   Land-on-TAd-plus-Anschrift-siblings ambiguity) is no longer reached
   for this chain.
   - `type=int` → `0 (0x00)` (the raw ordinal, hex appended by the
     formatter; `Land` is a 1-byte enum so the width is `0x00`).
   - **bare (auto-detect) → `ltInland` with type `enum`.** This was the
     §6.24 closure: `BindZeroIdFieldsByEnumNameConvention` (Pass 3 of
     the field-alias bridge) binds `TAd.Land.PrimitiveTypeId` to
     `TLandTyp`'s `$2A` id by the `Land → TLandTyp` name convention,
     because no `$2C`/`$67` record ties the field to its type
     structurally. Before §6.24 the bare call returned the misleading
     `"Failed to evaluate: holds 0 (nil pointer or zero-valued
     primitive)"`; a regression back to that string means the §6.24
     pass stopped firing (verify `TestTfwRecordFieldEnumNameConventionBindsLand`).

4. **Manual `read_memory` walk** to recover `Land` regardless of
   evaluator state — this part of the test always succeeds and
   gives the ground-truth value:
   - `get_registers` to confirm the live `Self` value (matches
     probe #1's hex).
   - `read_memory(<Self> + 0x0C5C, 4)` — bytes are little-endian
     `PAd` pointer value.
   - `read_memory(<FAd> + 0x169D, 1)` — single byte, the `Land`
     enum's ordinal.
   - Map the byte to the enum identifier using the declaration at
     `C:\Source\Base\Base.Types.pas:966`:
     `0 → ltInland, 1 → ltAusland, 2 → ltEUNetto,
      3 → ltEUBrutto, 4 → ltBLNetto`.
   - On the standard Adressen suite test data, expected byte =
     `0x00` = `ltInland`. Pin that in the report.

#### 11'. Reporting format

Produce a five-line verdict block for the user. The `Self.FAd` line
carries the formatter's native `<signed-int> (0x<hex>)` output (the
formatter appends the hex itself — no manual conversion), and the
bare-vs-`type=int` distinction on the leaf is reported on its own
line since §6.24 made the bare call resolve the enum name:

```
enum-evaluate (Tfw.Ad.Form:2274 / TFormAd.TblAdDataChange):
  Self                 : <object-result>              -- <verdict>
  Self.FAd             : <signed-int> (0x<hex>)        -- <verdict>
  Self.FAd.Land (int)  : <ordinal> (0x<hex>)           -- <verdict>
  Self.FAd.Land (auto) : <enum identifier>  [type=enum]-- <verdict>
  manual memory walk   : 0x<byte> = <enum identifier> (expected ltInland)
```

Plus a one-line cross-check note immediately under the block:
`[Self + 0x0C5C]` live bytes (LE) MUST equal the `(0x…)` hex in the
`Self.FAd` line, byte-for-byte. Since the formatter now prints that
hex directly, the check is a literal byte comparison — no
arithmetic. If they don't match, that's a real walker defect, not a
display artefact.

When a probe that was previously red flips green, call it out
explicitly — that's the gap-closure payoff for the user.

#### 12'. Teardown

Same as `suite-smoke` step 12, but remove the BP and `continue`
**before** the suite finishes so the suite doesn't stall on a
re-fired BP. The post-suite Indy `EIdConnClosedGracefully` is
expected; either ignore-by-class or let `terminate_debug_session`
end the run.

## Test case: `fixture-result-eval` (opt-in)

Validates three things at the `Result` of a Slim fixture function:
the **object resolution** of a function return value,
**published-property resolution via the live instance's runtime RTTI**,
and that the dotted walk survives an **intermediate object hop before the
property** (`Result.cmLst.Caption`). Born from a live IDE screenshot probe
(`Result.Caption` → `'001- Adresse suchen'` in the Delphi IDE); the MCP
evaluator now reproduces it the same way the IDE does (VMT RTTI →
`GetProc` → getter call injection; §4.16/§6.37). The deliverable is the
per-probe verdict — now a regression net for the property-evaluation path.

### Anchors

| Element | Detail |
|---|---|
| Unit / fixture | `TSlimFormCreateFixture.CreateAdForm: TFormAd` (`C:\Source\TFW\PAS\Tfw.Playground.SlimFixtures.pas`) |
| Breakpoint | **`Tfw.Playground.SlimFixtures:97`** — the `end;` of `CreateAdForm`, i.e. *after* `Result:=Screen.FocusedForm as TFormAd;` (line 96), so `Result` is assigned. (Lines are build-stable but re-confirm by reading the source; the `begin` form-create lines 94–96 are: `AdShowForm([accAdd,accPut,accDel]); Result:=Screen.FocusedForm as TFormAd;`.) |
| Workload to reach BP | FitNesse **test** page `Playground.CodeCafe.SampleA` (mode `test`, *not* `suite`) — calls `CreateAdForm`. The fixture is invoked more than once, so the BP fires repeatedly. |
| Result type | `TFormAd` (object) |
| Probed property | `Caption` — a VCL **property** (TControl.Caption → GetText), *not* an RSM field |
| Nested-object probe | `Result.cmLst.Caption` — hop through the `cmLst` child component (an RSM field on `TFormAd`), then its published `Caption` property. Expected value: `Leistungen`. |
| String-property probe (gap) | `Result.IniName` — `CBaseForm`'s storage/section name property (`read GetIniName`, returns `ClassName`). Ground-truth value `TFormAd`, but declared **`public`** (not `published`), so it is currently a **RED gap**: the evaluator's published-RTTI walk can't reach it → `Could not navigate`. |

### Procedure

Steps 1–8 of `suite-smoke` apply verbatim (resolve SUT, read LOGIN,
pre-flight port + state, build TFW, ignore `EPrinter` +
`EIdSocketError` + `EIdCouldNotBindSocket` +
`set_break_on_native_first_chance(false)`, start, continue, wait for
Slim on port 9000, and **confirm the port-9000 owner is the build you
launched** — see the pitfall below). Differences are step 5 (BP) and
step 9/10 (workload + at-pause probes).

#### 5'. Queue the BP at the fixture's Result line

```
mcp__DPT__set_breakpoint(unit="Tfw.Playground.SlimFixtures", line=97)
```

#### 9'. Start the Playground test in the background

```
RunFitnesse.bat Playground.CodeCafe.SampleA test
```
cwd `C:\Source\TEST\FitNesse`, background, `unset NoDefaultCurrentDirectoryInExePath`
first. Note the second arg is **`test`** (single page), not `suite`,
and the page is **not** prefixed with `ATDD.`. A clean run is
**1 Tests, 0 Failures** — but the suite result is not the deliverable
here; the at-pause probes are.

#### 10'. At the pause: probe Result, then the property

When `wait_until_paused` reports `paused_reason=breakpoint`,
`unit=Tfw.Playground.SlimFixtures`, `line=97`,
`procedure=…TSlimFormCreateFixture.CreateAdForm`, on a real worker
thread id, run:

1. `get_stack_trace` — expect the Slim-fixture chain:
   `CreateAdForm:97 ← System.Rtti.Invoke/DispatchInvoke ←
   Slim.Exec.TSlimStmtCallBase.ExecuteMember:459 ← CheckSynchronize ←
   FMX.Platform.Win.TPlatformWin.ThreadSync ← TApplication.ProcessMessage`.
2. **`evaluate Result` as `"object"`** → expected GREEN:
   `TFormAd @ <hex>` (e.g. `TFormAd @ E9A657E0`). This proves the
   evaluator resolves a function's `Result` slot to the live object.
3. **`evaluate Result.Caption` as `"string"`** → expected **GREEN**:
   the form title, e.g. `001- Adresse suchen` (matches the Delphi IDE).
   `Caption` is a VCL **published property** (`TControl.Caption` →
   `GetText`); the evaluator resolves it via the live instance's runtime
   RTTI (VMT → `vmtTypeInfo` → published-property table → `GetProc`),
   then **call-injects** the getter on the paused thread — the same path
   the IDE uses (§4.16 / §6.37, closed). This was the gap this case was
   created for; if it ever regresses to `Could not navigate` or an empty
   value, the RTTI-property path or call injection broke — call that out.
4. **`evaluate Result.cmLst.Caption` as `"string"`** → expected **GREEN**:
   `Leistungen`. This chains an **intermediate object hop**
   (`Result.cmLst` dereferences the `cmLst` child component on the form —
   an RSM field on `TFormAd`) into the **published-property + call-injection**
   resolution on the nested instance (`cmLst.Caption`, via the same
   VMT-RTTI → `GetProc` → getter-injection path as probe 3). It is the
   deeper cousin of `Result.Caption`: where that proves one property hop,
   this proves the dotted walk survives an object-field hop *before* the
   property resolution. If it regresses to `Could not navigate` the
   object-field capture broke; if it returns empty the call-injection path
   on the nested instance broke — call out which.
5. **`evaluate Result.IniName` as `"string"`** → currently **RED**
   (documented gap), `Could not navigate "Result.IniName"`. Ground-truth
   value would be `TFormAd`: `IniName` is `CBaseForm`'s storage/section
   name property (`Base.UI.Forms.Base.pas:198`, `read GetIniName`), and
   `GetIniName` returns `ClassName` (`:736`), which for a `TFormAd`
   instance is `TFormAd`. **Why it fails:** `IniName` is declared in a
   **`public`** section, *not* `published`. The MCP evaluator resolves
   properties only through the **published**-property RTTI table
   (VMT → `vmtTypeInfo` → published-property table → `GetProc`), which is
   exactly why `Result.Caption` (published on `TControl`) succeeds while a
   `public`-only property has no RTTI entry to walk and navigation fails.
   The Delphi IDE reads it via full debug info (TD32/line symbols), a path
   the MCP does not parse. Closing this gap would mean resolving public
   properties — either parsing the getter from `.rsm`/debug info and
   call-injecting it, or walking the public-property symbols. Until then
   this probe is the regression net for that gap: a **flip to GREEN
   returning `TFormAd`** is the payoff to call out; staying `Could not
   navigate` is the expected current state.
6. **`evaluate Result.FText` as `"string"`** (backing-field attempt) →
   navigates without error but is typically **empty**: for a VCL form
   the live caption is the native window text, not cached in
   `TControl.FText` at this point. Documents that the naive
   backing-field route does not recover the caption either. (Recovering
   the real string would need a `GetWindowText(Handle)` call, which the
   MCP cannot invoke — so there is no pure read_memory ground-truth for
   this one; that is the point of the gap.)

#### 11'. Reporting format

```
fixture-result-eval (Tfw.Playground.SlimFixtures:97 / TSlimFormCreateFixture.CreateAdForm):
  Result               : <object-result>       -- <verdict>   (expect TFormAd @ <hex>)
  Result.Caption       : <value>               -- <verdict>   (published property via RTTI; expect the form title)
  Result.cmLst.Caption : <value>               -- <verdict>   (object hop + property; expect "Leistungen")
  Result.IniName       : <value>               -- <verdict>   (PUBLIC property gap; currently "Could not navigate", would be "TFormAd")
  Result.FText         : <string or empty>     -- <verdict>   (backing field; expect empty)
```
`Result.Caption` returning the form title (e.g. `001- Adresse suchen`)
while `Result.FText` is empty is the success signal: it proves the
RTTI-property + call-injection path ran the getter (the value lives in
native window state, not the field). A regression to `Could not navigate`
or an empty Caption means the property-evaluation path broke.
`Result.cmLst.Caption` returning `Leistungen` additionally proves the
dotted walk survives an intermediate object hop before resolving the
published property on the nested instance.

#### 12'. Teardown

Same as `enum-evaluate` 12': `remove_breakpoint(Tfw.Playground.SlimFixtures, 97)`
then `continue` so the test wraps up gracefully, let the post-test
`EIdConnClosedGracefully` (Delphi, ignorable) appear, then
`terminate_debug_session`. Confirm port 9000 → FREE.

## Quick command reference

| Phase | Call |
|---|---|
| SUT path | `DPT.exe D12 DProjPrintOutputFile C:\Source\TFW\PAS\TFW.dproj` |
| Build | `DPT.exe D12 Build C:\Source\TFW\PAS\TFW.dproj --OnlyIfChanged` |
| Start | `mcp__DPT__start_debug_session(SUT, LOGIN)` |
| Ignore Delphi exc. | `mcp__DPT__ignore_exception("EPrinter")` |
| Quiet native exc. | `mcp__DPT__set_break_on_native_first_chance(enabled=false)` (or `ignore_exception_code("C0000005")`) |
| Resume | `mcp__DPT__continue` then loop `mcp__DPT__wait_until_paused` |
| Inspect | `mcp__DPT__get_stack_trace`, `mcp__DPT__get_locals` |
| Workload | `RunFitnesse.bat ATDD.Master.Stammdaten.Adressen suite` (cwd `C:\Source\TEST\FitNesse`, background) |
| Stop | `mcp__DPT__terminate_debug_session` |

## Pitfalls (all observed live)

1. **"Suite hangs at 'Starting Test System'"** is almost always the
   debugger sitting on a first-chance exception (TFW paused → Slim server
   silent). It is **not** a FitNesse problem. Check `get_state`; if
   `paused`, run the exception branch of the loop.
2. **Use `paused_reason`** to tell an exception stop from a breakpoint
   hit. For exceptions, `exception_code` is the key: `0EEDFADE` = a Delphi
   exception (ignorable by class via `ignore_exception`); anything else =
   native (suppress via `set_break_on_native_first_chance(false)` or
   `ignore_exception_code`). A native stop also shows `unit=""`, `line=0`
   because its top frame is in a system DLL.
3. **`EPrinter` + native printer exceptions** are the first wall on any
   form with a RichText control and no default printer. Plan to ignore the
   Delphi `EPrinter` *and* quiet native first-chance exceptions for a
   suite run.
4. **`ignore_exception` only matches Delphi exceptions.** It can be set
   before or during a session (pending ignores are applied on start), but
   it has no effect on native/OS exceptions — those need
   `set_break_on_native_first_chance` or `ignore_exception_code`.
5. **`.rsm` is the symbol source.** A `.map` is present too, but named
   locals/frames come from the `.rsm` sidecar next to the EXE.
6. **Never leave a session open** — always `terminate_debug_session` so
   port 9000 frees and the next run's pre-flight passes.
7. **A rogue TFW from a *different* build dir can own port 9000** —
   observed live: a separate `C:\MSE-26.04-MCP-Mongo\TFW\TFW.exe` was
   listening on 9000, so the debugged `C:\MSE-26.04-Mongo\TFW\TFW.exe`
   hit `EIdCouldNotBindSocket` (its Slim server never bound) and the
   FitNesse workload connected to the *rogue* instance instead. The
   suite still passed (7/0) but the **breakpoints never fired** because
   the workload never touched the debugged process. Symptoms: an
   `EIdCouldNotBindSocket` during startup that you must NOT dismiss as
   noise, plus a BP that resolves to `active` yet never hits during a
   green run. Guard: after the Slim server is up, **confirm the
   port-9000 owner is the exact path you launched** —
   `Get-NetTCPConnection -LocalPort 9000 -State Listen` →
   `Get-CimInstance Win32_Process -Filter "ProcessId=<pid>"` and check
   `CommandLine`. Re-run the port pre-flight between back-to-back runs;
   `EIdCouldNotBindSocket` early in a run almost always means a
   competing instance — kill it and restart.

## Parameters when invoked

- **Test-case name** (positional, optional). Match against the
  `Test cases (catalog)` table above. `list` is reserved and means
  "show the catalog, run nothing". Anything else is treated as a
  case name. Default when omitted: `suite-smoke`.
- A different **workload** path may be given. `suite-smoke` /
  `enum-evaluate` default to the `ATDD.Master.Stammdaten.Adressen`
  *suite*; `fixture-result-eval` defaults to the
  `Playground.CodeCafe.SampleA` *test* page (mode `test`, no `ATDD.`
  prefix). Convert slash-notation to dot-notation; only prefix `ATDD.`
  for the Stammdaten suites.
- Optional **breakpoint(s)** as `Unit:Line` — overrides the chosen
  test case's default BP. Without an override, each test case picks
  its own: `suite-smoke` queues `Tfw.Ad.Form:1120`, `enum-evaluate`
  queues `Tfw.Ad.Form:2274`, `fixture-result-eval` queues
  `Tfw.Playground.SlimFixtures:97`. If no test case is named AND no
  breakpoint is given, the bare `suite-smoke` default applies.
