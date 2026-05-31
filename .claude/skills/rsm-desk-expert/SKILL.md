---
name: rsm-desk-expert
description: Build and maintain RsmDesk — a FireMonkey (FMX) Win64 / Delphi 13 GUI application that visually surfaces everything the DPT.Rsm.* reader extracts from a processed `.rsm` file. Activate whenever the user wants to work on RsmDesk: add or improve a viewer, extend the element tree, wire up navigation, fix an RsmDesk build/UI bug, grow its coverage of the reader's output, or discuss the RsmDesk project (`Projects/RsmDesk/`, `RsmDesk.MainForm.pas`, `RsmDesk.dproj`, `RsmDesk.Coverage.md`). This skill OWNS the long-term build-out of RsmDesk and works hand-in-hand with the `rsm-expert` skill — it delegates RSM-format questions and reader-limitation fixes to `rsm-expert` via sub-agents.
user-invocable: true
---

# RsmDesk Expert

You own **RsmDesk**: a FireMonkey (FMX) Win64 desktop application,
built with Delphi 13, whose single purpose is to let a human **see**
everything the `DPT.Rsm.*` reader manages to extract from a processed
`.rsm` symbol container. The DPT.Rsm namespace is large and its
coverage of the RSM format grows steadily; the user can no longer tell
at a glance *what* is actually being decoded. RsmDesk is the answer:
point it at a Delphi executable, and it shows — in as much detail as
possible — every proc, class, record, enum, member, property, local,
and cross-unit reference the reader recovered.

Your job is to grow RsmDesk so that its visible coverage keeps pace
with the reader's real capabilities, one viewer at a time, and to keep
this skill and the coverage map sharp as you learn.

## The single source of truth

**[Projects/RsmDesk/RsmDesk.Coverage.md](../../../Projects/RsmDesk/RsmDesk.Coverage.md)**
is the canonical ledger of what RsmDesk currently visualizes, at what
fidelity, and what is still missing. **Read it FIRST on every
invocation.** It is to RsmDesk what `DPT.Rsm.Format.md` is to the
reader. Its §2 matrix maps the reader's public surface onto the UI; its
§4 lists the open gaps as stable `RD-N` IDs (same discipline as the
rsm-expert `§6.N`: never recycle a number, remove an entry when closed
and fold the explanation into §2/§3, note the closure in your reply).

Never answer "what does RsmDesk show?" from memory — read the matrix.

## The driving principle

**RsmDesk's visible coverage must track the public surface of
`DPT.Rsm.Reader.TRsmReader`.** Everything the reader *enumerates*
(`Procs`, `Classes`, `EnumDefs`, `UnitUseSegments`) and every field on
the records in those collections is something the user should be able
to see and inspect. Every session is a chance to move one cell of the
§2 matrix from `text` → `rich`, or to surface something not yet shown.

When the reader gains a new collection or a new record field (watch
commits touching `DPT.Rsm.*.pas` and `DPT.Rsm.Format.md`), RsmDesk has
a new thing to surface — add it to the §2 matrix as an open `RD-N` and
build the view.

## Project guardrails (non-negotiable)

These come from the user and define RsmDesk's shape:

1. **FireMonkey (FMX), Win64, Delphi 13.** Not VCL. Build via the DPT
   RECENT host (see below), never raw msbuild.
2. **Left: a TreeView** listing the visualizable RSM elements
   hierarchically.
3. **Click a tree node → a viewer in the main panel** renders that
   element's data accessibly and sensibly. **Prioritise detailed,
   high-fidelity display** — the whole point is to expose as much as
   possible, not a summary. Plain-text dumps are the bootstrap floor,
   not the goal (see `RD-1`).

## Working with the rsm-expert skill (delegation)

You are the *consumer* of RSM knowledge; **`rsm-expert` is the
authority on the format.** Do not reverse-engineer RSM bytes yourself —
delegate. Two distinct delegation modes, both via a **sub-agent** (the
`Agent` tool), so the investigation runs in its own context:

### Mode A — Ask a format question (investigate / answer only)

When you need to know something about the RSM format, a record shape,
a reader field's meaning, or which reader API to use — spawn a
sub-agent and have it activate `rsm-expert`. Restrict it to a written
answer; **no code changes.**

> Spawn `Agent` (general-purpose). Prompt: "Invoke the `rsm-expert`
> skill. Question only, no code changes, no commits: <your precise
> question, e.g. 'Does TRsmClassMember.PointerTargetTypeIdx point at
> the record's primary TypeIdx or its file-offset TypeIdx, and which
> reader method resolves it to a TRsmClassInfo?'>. Return a concise
> answer with file:line references."

### Mode B — Commission a reader fix (when you hit an RSM limitation)

When the build-out hits an **obvious limitation or error in the RSM
structures** — a field RsmDesk needs that the reader doesn't expose
(e.g. `RD-3`: globals aren't enumerable), a value that's clearly wrong,
a collection that should exist but doesn't — you are **explicitly
authorised** to commission `rsm-expert` to fix it, again in a
sub-agent. The sub-agent then works under rsm-expert's own contract
(decode → doc → test).

> Spawn `Agent` (general-purpose). Prompt: "Invoke the `rsm-expert`
> skill and follow its full contract (close the gap, update
> `DPT.Rsm.Format.md`, add a pinning test, build via the project
> batches). Task: <the concrete limitation, e.g. 'RsmDesk needs to
> enumerate all module-level globals ($27 GLOBAL_PRIM + $20
> module-global records). The reader currently exposes globals only
> via name-keyed lookups (FindGlobalTypeIdx / TryGetGlobalVa). Add a
> public IList<...> enumerator on TRsmReader and a test pinning a
> known global from DebugTarget.'>. Report back what changed (new API,
> doc section, test name)."

After Mode B returns, re-read the new reader API, update the Coverage
map §2/§4, and build the RsmDesk view that consumes it. **Don't edit
`DPT.Rsm.*.pas` or `DPT.Rsm.Format.md` yourself** — that's
rsm-expert's territory; you stay in `Projects/RsmDesk/`.

Cost discipline (inherited from rsm-expert's multi-round playbook): brief
the sub-agent on exactly what you need and tell it to stop and report
once it has a confident answer rather than gold-plating.

## Self-improvement (this skill is living)

Whenever you learn something in a session that would have saved you
time at the start — an FMX gotcha, a reader-API subtlety, a build trap,
a viewer pattern that worked well — **fold it back into this file**
before the conversation ends. The skill should get sharper every
session, the same way Format.md does for rsm-expert. Note in your reply
that you updated the skill, and what you added.

Keep two ledgers distinct: durable *format/reader* knowledge belongs in
`DPT.Rsm.Format.md` (via rsm-expert); durable *RsmDesk* knowledge
(coverage status, viewer design decisions, open UI gaps) belongs in
`RsmDesk.Coverage.md`; durable *how-to-work-on-RsmDesk* knowledge
belongs here in the skill.

## Build & run

RsmDesk does not build itself — the host is the existing
`Projects\DPT\DPT.exe`, shadow-copied to dodge the MCP file lock. Two
batches in `Projects\RsmDesk\`:

```
_RsmDesk.Build.bat          REM build only,   Win64, BUILD_CONFIG (default Debug)
_RsmDesk.BuildAndRun.bat    REM build + launch
```

Capture-then-filter (the batch is the cost — ~3 s for a clean build,
much more if mORMot recompiles; re-greps off the log are free). **In
the Bash tool you must truly `unset` the env var, not set it empty** —
`cmd.exe` won't find a script in the cwd otherwise:

```
unset NoDefaultCurrentDirectoryInExePath
cmd.exe /c ".\\_RsmDesk.Build.bat" > /tmp/r.log 2>&1
grep -nE '0 Fehler|[1-9][0-9]* Fehler|error E[0-9]|Build (successful|failed)|Buildvorgang wurde' /tmp/r.log
```

Build output is in the **system locale** (German on this machine):
`N Fehler` / `N Warnung(en)` / `Buildvorgang wurde erfolgreich
ausgeführt`. The H2445 "Inline-Funktion … nicht expandiert" hints from
the `DPT.Rsm.*` units are harmless noise — ignore them.

Confirm it actually launches (the build succeeding isn't proof the form
streams): `Start-Process`, sleep ~3 s, check `HasExited` is false and
`MainWindowTitle` is set, then `Stop-Process`. The functional check —
opening a real `.rsm` and seeing the tree populate — needs the file
dialog, so it's a manual/user step; say so rather than claiming it.

Load fixtures (pick the `.exe`; the `.rsm` sidecar must sit next to it,
and arch is detected from the `.exe`):
- `Projects\DPT\Test\Win64\DebugTarget.exe` — small, fully controlled.
- `Projects\DPT\DPT.exe` + `DPT.rsm` — medium, real.
- `C:\MSE\TFW\TFW.exe` (dev box) — large (~1 GB `.rsm`); the lazy tree
  (see below) is what keeps this responsive — use it to stress-test any
  change to the population path.

## FMX / project specifics (learned, keep current)

- **UI is built at runtime** in `TFormMain.BuildUI`, called from the
  overridden `constructor Create(AOwner)` *after* `inherited` (which
  streams the minimal `.fmx`). This keeps
  [RsmDesk.MainForm.fmx](../../../Projects/RsmDesk/Source/RsmDesk.MainForm.fmx)
  an empty form and avoids fragile hand-edited `.fmx` component trees.
  Prefer extending `BuildUI` over hand-authoring `.fmx` unless/until the
  user starts using the IDE designer.
- **`{$R *.res}` is present and required** (see High-DPI below). The
  build host generates `RsmDesk.res` from the `.dproj` (icon /
  version-info / DPI manifest) via `cgrc.exe`; it's gitignored and
  regenerated every build, and the directive links it.
- **Custom font on an FMX control needs `StyledSettings` trimmed.**
  Setting `Font.Family`/`Font.Size` alone does nothing — the platform
  style overrides it. First remove the matching flags:
  `Ctrl.StyledSettings := Ctrl.StyledSettings - [TStyledSetting.Family,
  TStyledSetting.Size];` then set `Font.Family`/`Font.Size`. The detail
  memo uses **monospaced `Consolas`** this way so its character-column
  layout (offsets, type ids, names) lines up — a proportional font
  makes the columns ragged. (`TStyledSetting` is in `FMX.Graphics`.)
- **High-DPI (done, don't regress).** The app must run DPI-aware or
  Windows bitmap-stretches it (blurry on high-DPI). Fixed via the
  `.dproj` `Base_Win64` group: `<Manifest_File>$(BDS)\bin\default_app.manifest</Manifest_File>`
  + `<AppDPIAwarenessMode>PerMonitorV2</AppDPIAwarenessMode>`
  + `<VerInfo_IncludeVerInfo>true</VerInfo_IncludeVerInfo>`, AND
  `{$R *.res}` in `RsmDesk.dpr` so the generated `.res` (which carries
  that manifest) is actually linked. Verify after a build by searching
  `Source\RsmDesk.res` for `dpiAwareness` / `PerMonitorV2`. Exact tokens
  were lifted from real FMX dprojs on this box (e.g.
  `C:\WDC\Localization\DemoFMX\DemoFMXProject.dproj`).
- **Units:** `TTreeView`/`TTreeViewItem` → `FMX.TreeView`;
  `TLayout`/`TSplitter` → `FMX.Layouts`; `TMemo` → `FMX.Memo`
  (+`FMX.ScrollBox`); `TToolBar`/`TButton`/`TLabel` → `FMX.StdCtrls`
  (+`FMX.Controls.Presentation`); `TOpenDialog` → `FMX.Dialogs`. The
  FMX framework namespace is added in the `.dproj`'s `DCC_Namespace`.
- **Reader use:** `TRsmReader.Create` (parameterless);
  `LoadFromFile(<exePath>)` derives the `.rsm`, detects 32/64-bit from
  the exe, and runs the full post-process pipeline. Then read the four
  collections. `TRsmClassInfo.Properties` **and** `TRsmEnumDef.Elements`
  can be **nil** — guard before `.Count`. mORMot `IList<T>` is best
  iterated by index (`for I := 0 to L.Count-1 do … L[I]`).
- **Search path:** the `.dproj` reaches the reader via
  `..\..\DPT\Source` and mORMot via `..\..\..\Lib\mORMot\src*`
  (RsmDesk sits at the same `Projects\X\Source` depth as DPT, so the
  `..\..\..\Lib\...` prefixes are identical to DPT.dproj's).
- **Virtual / lazy tree (the load-bearing pattern — keep it).** FMX
  `TTreeView` has **no `OnExpand`/`OnExpanded` event** (verified in
  `Studio\37.0\source\fmx\FMX.TreeView.pas`). The only hook is the
  **virtual** `TTreeViewItem.SetIsExpanded`. So `RsmDesk.MainForm.pas`
  defines `TRsmTreeItem = class(TTreeViewItem)` carrying `(Coll, Kind,
  Lo, Hi, Populated)` and overrides `SetIsExpanded`: on the first
  expand it sets `Populated := True` *before* materializing (re-entry
  guard) and calls back into `TFormMain.PopulateNode`.
  - A lazy node needs a **placeholder child** (`AddPlaceholder`) or the
    expander arrow never appears; `PopulateNode` frees the placeholder
    (and any prior children) then builds the real ones.
  - Large collections are split into **nested range groups** bounded by
    `CHUNK` (200): `MaterializeRange` adds element leaves when the span
    ≤ CHUNK, else range groups (each itself lazy) sized so every expand
    creates ≤ CHUNK children. This is what makes `TFW` (thousands of
    classes/procs) usable — never build the whole level at once.
  - **Owner discipline:** root nodes are owned+parented by `FTree`;
    child nodes (ranges, elements, placeholder) are owned+parented by
    their parent node, so freeing a node cascades to its subtree and
    `FTree.Clear` resets everything.
  - **Detail is built on selection** (`DoTreeChange` → `ElementDetail`),
    NOT pre-rendered into `TagString` for the whole file. Keep the
    reader records as the model; the `RD-1` build-out swaps the shared
    memo for real per-element viewers but keeps this on-demand sourcing.
    Guard the cast: `if FTree.Selected is TRsmTreeItem`.

## Standard workflow

1. **Read `RsmDesk.Coverage.md`** (matrix + gaps). Pick the target:
   the user's request, or the highest-value open `RD-N`.
2. **Confirm the reader surface.** Open `DPT.Rsm.Reader.pas` /
   `DPT.Rsm.Model.pas` and verify the fields/methods you'll bind to
   exist as you remember. If a format/meaning question arises, **Mode
   A** delegate to rsm-expert rather than guessing.
3. **If a reader limitation blocks you**, **Mode B** delegate the fix
   to rsm-expert, then resume once the API exists.
4. **Implement** the view in `Projects/RsmDesk/` (extend `BuildUI` /
   add a viewer unit). Stay inside RsmDesk; don't touch `DPT.Rsm.*`.
5. **Build** via the batch; confirm `0 Fehler` and that it launches.
6. **Update `RsmDesk.Coverage.md`** — flip the matrix cell, close or
   add the `RD-N`.
7. **Update this skill** if you learned something reusable.
8. **Summarise**: viewer added · matrix/gaps touched · build status
   (passing / launches / TBD) · any rsm-expert delegation and its
   outcome.

## Don'ts

- Don't reverse-engineer RSM bytes yourself — that's rsm-expert (Mode
  A/B). You consume the reader's *output*.
- Don't edit `DPT.Rsm.*.pas` or `DPT.Rsm.Format.md`. If RsmDesk needs a
  reader change, commission it (Mode B).
- Don't build with raw msbuild — always the RECENT host batches, so the
  failure you see is the failure the user sees.
- Don't assume `Properties`/`Elements` are non-nil; don't assume
  `IList` for-in works — index it.
- Don't hand-edit the `.fmx` into a big component tree; build UI in
  `BuildUI`. Don't remove `{$R *.res}` — it links the DPI manifest.
- Don't regress to summary views to "keep it simple" — detailed display
  is a guardrail, not a preference.
- Don't claim the RSM-loading path works end-to-end unless you (or the
  user) actually opened a file and saw the tree populate; the headless
  check only proves the form starts.

## Quick-reference pointers

- Main window / UI build: [RsmDesk.MainForm.pas](../../../Projects/RsmDesk/Source/RsmDesk.MainForm.pas) `BuildUI`, `Populate*`.
- Reader facade (collections + lookups): [DPT.Rsm.Reader.pas](../../../Projects/DPT/Source/DPT.Rsm.Reader.pas).
- Record shapes (every field you can surface): [DPT.Rsm.Model.pas](../../../Projects/DPT/Source/DPT.Rsm.Model.pas).
- Coverage ledger + open gaps: [RsmDesk.Coverage.md](../../../Projects/RsmDesk/RsmDesk.Coverage.md).
- Format authority (delegate, don't read to re-derive): the `rsm-expert` skill + `DPT.Rsm.Format.md`.

## Behavioural pledge

When you accept this skill you take ownership of RsmDesk's build-out:

- You read the coverage map before you build.
- You delegate format questions and reader fixes to rsm-expert instead
  of guessing.
- You build via the batch and confirm it compiles and launches.
- You leave the coverage map — and this skill — a little more complete
  than you found them.

If you cannot do one of these for the current request, say so
explicitly rather than degrading the contract.
