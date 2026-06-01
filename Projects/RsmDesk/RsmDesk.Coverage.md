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
| `SourceFiles` : `IList<TRsmSourceFile>` | SourceFile, UnitName, StartOffset, Rva | text | drives the "Imports by unit" grouping (the importing-unit nodes) |
| `UnitUseSegments` : `IList<TRsmUnitUseSegment>` | UnitName (declaring), StartOffset, SourceFileIdx, Refs[] | text | "Imports by unit (N)" → per importing unit → per declaring unit |
| └ `TRsmUnitUseSegment.Refs` : `TRsmUnitUseRef` | Kind, Name, Rva | text | inline in segment detail |

The imports branch is **grouped by importing unit**: the root lists one
node per importing unit (resolved via `SourceFileIdx` →
`SourceFiles[].UnitName`, the §4.17 `$70` introducer), each expands to
the declaring units it imports from (`UnitName` + used-symbol count),
each of those to the `$66`/`$67`/`$70` refs. Grouping is precomputed
once into `FImporterGroups` (contiguous-run scan of `SourceFileIdx`).

Query / lookup methods the reader exposes that are **not yet wired into
any view** but enable richer navigation (cross-links, jump-to-type):
`FindClassByName`, `FindStructByTypeIdx`, `FindClassIdxByRsmTypeId`,
`TryGetEnumConstantName`, `IsEnumTypeId`, `IsRecordTypeIdx`,
`FindTypeIdByName`, `FindRecordsByMemberName`,
`FindBestRecordForGlobalAndField`, `UnitsDeclaringType`,
`UnitsImporting`, `FindGlobalTypeIdx`, `TryGetGlobalVa`,
`FindProcContaining`, `FindProcByName`.

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
- **Imports grouped by importing unit (done):** the imports branch now
  answers "which unit imports what". Backed by the reader's `$70`
  introducer attribution (`SourceFileIdx` / `SourceFiles`, Format.md
  §4.17 + §6.20 R6, commissioned via rsm-expert Mode B). The `rnImporter`
  node kind extends the lazy machinery: an importer's segments are a
  contiguous `UnitUseSegments` range, so it reuses `MaterializeRange`.
- **TypeIdx resolved to a name (done, carefully — §6.27):** there are
  THREE id spaces and they must not be cross-looked-up.
  - **Members** use the file-offset space (`FindStructByTypeIdx`, plus
    `PrimitiveTypeId` / `PointerTargetTypeIdx`) — globally unique and
    **trustworthy**; members show their resolved type.
  - **Local/param `TypeIdx` is a per-proc local-ref index, NOT a type
    id** — routing it through the registry gave confident WRONG names
    (`TFormMain.Create`'s `Self`=$84 → an unrelated class
    `TLayerCollectionAccess`). So locals show the **raw id only**,
    except `Self`, whose type is derived reliably from the **method's
    qualified name** (`ResolveSelfType`).
  - **Regime boundary — do NOT "fix" this to use the registry just
    because DebugTarget resolves cleanly.** The `$21`/`$22` slot has
    TWO regimes (rsm-expert verified both, June 2026): on a **small**
    binary (DebugTarget) it carries the class's real `$2A` primary
    (hi-byte `$2E`/`$2F`), so `FindClassIdxByRsmTypeId(local.TypeIdx)`
    resolves *correctly* — `Self`=`$2E21`=`TDerived`. On a **large**
    binary (TFW: 71k names under ≤256 ids) it collapses to a 1-byte
    per-proc ref that mis-resolves. RsmDesk opens **both** (TFW
    included), so the raw-id-only policy is the only universally safe
    one; the clean-regime success on DebugTarget is a trap that would
    pass local tests and break on TFW. Pinned reader-side by
    `TestSelfTypeIdxResolvesInCleanRegime32/64` (clean) and
    `TestTfwSelfTypeIdxIsPerProcRefNotRegistryId` (gap);
    `DPT.Rsm.Format.md` §6.27.
  - Enum *type* names from an id remain unresolved (`RD-5`).
- **Locals shown kind-aware (done):** register params render as
  `reg#N` (not a misleading `BP+0`), only `lkBpRel` locals show a `bp`
  offset; compiler-generated unnamed slots (`Name = '.'`, Format.md
  §4.4) are relabeled `<temp>` rather than shown as a bare dot.
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

Use `RD-<next>` when adding an entry; the highest ever used is **RD-5**.
Remove an entry when it's closed and fold the explanation into §2/§3.

### RD-1 Per-element rich viewers (UI gap)

The detail viewer is a single read-only `TMemo` showing pre-rendered
text. Replace with element-specific viewers: a member grid for
classes/records (sortable by offset), an element grid for enums, a
locals grid for procs, and a hex/struct pane. Highest-value first item
of the build-out.

### RD-2 Cross-navigation by TypeIdx (UI gap, reader already supports)

Members resolve their `TypeIdx` to a **plain-text type name** next to
the hex (`ResolveStructType` / `ResolveMemberType`, file-offset space);
`Self` resolves via the method name. Local/param ids are not statically
resolvable (§6.27, see §3). Still open: make a resolved name a
**clickable link** that selects the target type's tree node (needs
locating the node, which interacts with the lazy tree — a node may not
be materialized yet).

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

### RD-5 Enum type name not resolvable from a type id (reader limitation — Mode-B candidate)

The reader can say a type id *is* an enum (`IsEnumTypeId`) and resolve a
`(typeId, ordinal)` to a constant name (`TryGetEnumConstantName`), but
there is **no read-only API that returns the enum's TYPE name from a
type id** (works only when the enum also has a `$2A` registry entry,
which RTL/cross-unit-aliased and sparse enums often lack). So a local /
member typed as an enum currently shows just "enum" rather than e.g.
"TLandTyp". Closing this needs a reader change (expose enum-type-name by
id) — commission `rsm-expert` (Mode B) when the build-out needs it.

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
