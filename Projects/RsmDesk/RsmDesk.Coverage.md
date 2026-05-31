# RsmDesk — Coverage Map

This is the **single source of truth** for what RsmDesk can currently
visualize out of a processed `.rsm` file, at what fidelity, and what is
still missing. It is to RsmDesk what `DPT.Rsm.Format.md` is to the
reader: the `rsm-desk-expert` skill reads it first on every invocation
and leaves it a little more complete than it found it.

The driving rule: **RsmDesk's coverage must track the public surface of
`DPT.Rsm.Reader.TRsmReader`.** Every collection the reader exposes and
every field on the records in those collections is something the user
should eventually be able to see. This document is the ledger that says
how far we've gotten.

---

## 1. Architecture snapshot

FireMonkey (FMX) GUI application, Win64, Delphi 13. Built via the DPT
RECENT build host (see §5), not raw msbuild.

Main window layout (built at runtime in
[RsmDesk.MainForm.pas](Source/RsmDesk.MainForm.pas) `BuildUI`):

```
+--------------------------------------------------------------+
| [ Open .exe/.rsm... ]  <loaded path>                         |  TToolBar
+----------------------+---------------------------------------+
|  TTreeView (left)    |  Viewer host (TLayout, client)        |
|  visualizable        |  +---------------------------------+  |
|  elements,           |  | TMemo (detail, read-only)       |  |
|  hierarchical        |  |                                 |  |
|                      |  +---------------------------------+  |
+----------------------+---------------------------------------+
        ^ TSplitter between tree and viewer
```

The tree is fed from a single `TRsmReader` instance after
`LoadFromFile(<exe>)` (which derives the `.rsm` sidecar and runs the
full post-process pipeline). Population is **virtual / lazy**: only the
four category roots are created up front; each node materializes its
children on first expand, and large collections are split into nested
range groups so every expand stays bounded (see §3). Selecting a leaf
builds its detail text on demand into the viewer memo — nothing is
pre-rendered for the whole file.

This is the **bootstrap fidelity**: every element is rendered as plain
text in one shared memo. The build-out replaces the memo with
element-specific viewers (grids, hex panes, cross-links) — see §4.

---

## 2. The reader surface → UI coverage matrix

Reader collections come from the `TRsmReader` properties
`Procs` / `Classes` / `EnumDefs` / `UnitUseSegments`
([DPT.Rsm.Reader.pas](../DPT/Source/DPT.Rsm.Reader.pas)). Record shapes
are in [DPT.Rsm.Model.pas](../DPT/Source/DPT.Rsm.Model.pas).

Fidelity legend: **none** = not shown · **text** = dumped as text in the
shared memo · **rich** = dedicated viewer (grid / hex / navigable).

| Reader output | Record fields | Fidelity | Tree node |
|---|---|---|---|
| `Procs` : `IList<TRsmProc>` | Name, SegmentOffset (RVA), Size, Locals[] | text | "Procedures (N)" → per proc |
| └ `TRsmProc.Locals` : `TRsmLocal` | Name, BpOffset, TypeIdx, Kind, RegParamIdx | text | inline in proc detail |
| `Classes` : `IList<TRsmClassInfo>` | Name, TypeIdx, Kind, ParentName, ParentRawId | text | "Classes & Records (N)" → per type |
| └ `TRsmClassInfo.Members` : `TRsmClassMember` | Name, Offset, TypeIdx, Size, PrimitiveTypeId, PointerTargetTypeIdx | text | inline in class detail |
| └ `TRsmClassInfo.Properties` : `TRsmClassProperty` | Name, TargetId, PrimitiveTypeId, UnderlyingField | text | inline in class detail |
| `EnumDefs` : `IList<TRsmEnumDef>` | TypeName, UnitName, Elements[] | text | "Enum types (N)" → per enum |
| └ `TRsmEnumDef.Elements` : `TRsmEnumElement` | Name, Ordinal | text | inline in enum detail |
| `UnitUseSegments` : `IList<TRsmUnitUseSegment>` | UnitName, StartOffset, Refs[] | text | "Unit-use segments (N)" → per segment |
| └ `TRsmUnitUseSegment.Refs` : `TRsmUnitUseRef` | Kind, Name, Rva | text | inline in segment detail |

Query / lookup methods the reader exposes that are **not yet wired into
any view** but enable richer navigation (cross-links, jump-to-type):
`FindClassByName`, `FindStructByTypeIdx`, `FindClassIdxByRsmTypeId`,
`TryGetEnumConstantName`, `IsEnumTypeId`, `IsRecordTypeIdx`,
`FindTypeIdByName`, `FindRecordsByMemberName`,
`FindBestRecordForGlobalAndField`, `UnitsDeclaringType`,
`FindGlobalTypeIdx`, `TryGetGlobalVa`, `FindProcContaining`,
`FindProcByName`.

---

## 3. Coverage status summary

- **Complete at text fidelity:** all four reader collections and every
  scalar field on their records are surfaced. Nothing the reader
  *enumerates* is currently invisible.
- **Virtual / lazy tree (done):** FMX has no `OnExpand` event, so
  `TRsmTreeItem` subclasses `TTreeViewItem` and overrides the virtual
  `SetIsExpanded`. Each lazy node carries a placeholder child (so the
  expander shows) and materializes real children only on first expand;
  re-expanding a collapsed node does not rebuild. Collections larger
  than `CHUNK` (200) items are split into nested range groups
  (`[ lo .. hi ]`), recursively, so every single expand creates at most
  `CHUNK` children — this scales to `TFW.rsm` (~800 MB Win32 / ~1.17 GB
  Win64) without ever building tens of thousands of items at once. Leaf
  detail is built on selection, not pre-rendered.
- **High-DPI (done):** Per-Monitor-V2 manifest embedded via the
  `.dproj` (`Manifest_File` + `AppDPIAwarenessMode`) and linked through
  `{$R *.res}`, so the window renders crisply instead of being
  bitmap-stretched.
- **Bootstrap shortcuts (intentional, to be replaced):** one shared
  read-only memo (monospaced `Consolas`, so the character-column detail
  layout lines up) instead of per-element viewers; no search / filter;
  members/locals/elements/refs shown as detail text rather than as a
  deeper tree level.

---

## 4. Identified gaps (stable IDs — never recycled)

Use `RD-<next>` when adding an entry; the highest ever used is **RD-4**.
Remove an entry when it's closed and fold the explanation into §2/§3.

### RD-1 Per-element rich viewers (UI gap)

The detail viewer is a single read-only `TMemo` showing pre-rendered
text. Replace with element-specific viewers: a member grid for
classes/records (sortable by offset), an element grid for enums, a
locals grid for procs, and a hex/struct pane. Highest-value first item
of the build-out.

### RD-2 Cross-navigation by TypeIdx (UI gap, reader already supports)

Members/locals carry a `TypeIdx` (and `PointerTargetTypeIdx` for
pointer-to-record fields). The reader can resolve these
(`FindStructByTypeIdx`, `FindClassIdxByRsmTypeId`, `IsEnumTypeId`,
`TryGetEnumConstantName`), but the UI shows them as raw hex. Make a
`TypeIdx` a clickable link that selects the target type's tree node.

### RD-3 Global variables not enumerable (reader limitation — candidate rsm-expert task)

The reader exposes globals only via name-keyed lookups
(`FindGlobalTypeIdx`, `TryGetGlobalVa`) — there is **no public
collection** that enumerates all globals the scanner found (the `$27`
GLOBAL_PRIM and module-level `$20` records). RsmDesk therefore cannot
list a "Globals" tree branch. Closing this needs a reader change (expose
an `IList` of globals). When the build-out reaches this, commission the
`rsm-expert` skill (sub-agent) to add the enumerator — this is exactly
the "obvious limitation in the RSM structures" case the skill is told to
escalate.

### RD-4 Raw enum-constant ($25) records not surfaced separately

`EnumDefs` ($03) is the canonical, declaration-ordered view and is
shown. The raw `$25` ENUM_CONST records (per-element type-id + ordinal +
name, used by the formatter) are folded away and not separately
visible. Likely low value once RD-1's enum grid lands, but recorded so
the decision is explicit rather than forgotten. Confirm with
`rsm-expert` whether `$25` carries anything `$03` doesn't before
building a view for it.

---

## 5. Build & run

Built via the project's own DPT build host (the existing
`..\DPT\DPT.exe`), shadow-copied to dodge the MCP file lock:

```
Projects\RsmDesk\_RsmDesk.Build.bat          REM build only, Win64 Debug
Projects\RsmDesk\_RsmDesk.BuildAndRun.bat    REM build + launch
```

Override the config with `set BUILD_CONFIG=Release` before the batch.
Output: `Projects\RsmDesk\RsmDesk.exe` (gitignored, along with `DCU\`).

Capture-then-filter, like the rsm-expert build loop (the batch is the
cost, re-greps are free):

```
unset NoDefaultCurrentDirectoryInExePath
cmd.exe /c ".\\_RsmDesk.Build.bat" > /tmp/r.log 2>&1
grep -nE '0 Fehler|[1-9][0-9]* Fehler|error E|Build (successful|failed)|Buildvorgang' /tmp/r.log
```

No `{$R *.res}` yet (hand-authored skeleton). Opening the project once
in the Delphi 13 IDE regenerates `RsmDesk.res` and re-adds the
directive; the app builds and runs without it.

---

## 6. File map

| File | Role |
|---|---|
| [Source/RsmDesk.dpr](Source/RsmDesk.dpr) | Program entry, FMX bootstrap |
| [Source/RsmDesk.MainForm.pas](Source/RsmDesk.MainForm.pas) | Main window; tree + viewer; `BuildUI` builds the UI at runtime |
| [Source/RsmDesk.MainForm.fmx](Source/RsmDesk.MainForm.fmx) | Minimal empty form (UI is runtime-built) |
| [Source/RsmDesk.dproj](Source/RsmDesk.dproj) | FMX Win64 project; references DPT.Rsm.* + mORMot via search path |
| [_RsmDesk.Build.bat](_RsmDesk.Build.bat) / [_RsmDesk.BuildAndRun.bat](_RsmDesk.BuildAndRun.bat) | Build / build+run via DPT RECENT host |
| RsmDesk.Coverage.md | This document |
