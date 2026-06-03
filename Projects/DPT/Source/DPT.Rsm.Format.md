# RSM (CSH7) Symbol Container — Format Reference

This document describes the on-disk layout of the **RSM** symbol container
emitted by the Delphi linker under the `-VR` switch, as understood by the
`DPT.Rsm.*` units. It is reverse-engineered from real binaries (the
`DebugTarget` test fixture plus the much larger `TFW.rsm` corpus) — there
is no official specification.

The source of truth is the Pascal code in
[Projects/DPT/Source/](.). Where a comment in the code disagrees with what
the code actually does, the **code wins**. Sections marked with
`UNCERTAIN` or `GAP` flag shapes that are only partially understood or
known to be missing.

---

## 1. High-level picture

* **File extension**: `.rsm`, sitting next to the `.exe` / `.dll` it
  describes (e.g. `MyApp.exe` + `MyApp.rsm`). Produced by the Delphi
  linker option `-V -VR`.
* **Signature**: the first 4 bytes are the little-endian DWORD
  `$37485343`, i.e. the ASCII string `CSH7`. Anything else makes
  `TRsmScanner.LoadFromBuffer` early-exit without populating any
  collection. See [`TRsmTag.SigCSH7`](DPT.Rsm.Model.pas) and
  [`TRsmScanner.LoadFromBytes`](DPT.Rsm.Scanner.pas).
* **Not BER / ASN.1**: despite carrying tagged records, RSM has **no
  Tag-Length-Value framing**. There is no length octet after the tag;
  the body length is encoded structurally (per-tag fixed shapes,
  content-discriminated body variants, LSB-as-continuation tricks, and
  explicit sentinel bytes such as `$63 SCOPE_END`). The scanner is a
  tolerant byte-walker that single-byte-advances on shape mismatch and
  relies on tight anchor patterns to keep false positives low.
* **Two complementary "formats"** sitting in the same byte stream:
  * **Format B** (offset-only) — fields emitted right after their
    parent record name (`$0E ... $02 NameLen ...`). Decoded by
    `TRsmStructDiscoverer.ScanFieldsForwardFromRecordName` /
    `ScanFieldsBackwardFromClassName`.
  * **Format A** (named, typed) — `$2A` type-registry entries plus
    `$2C` field records that join each field to its declaring type and
    parent record. Decoded by `TRsmFormatALinker.LinkFieldsFromFormatA`.
  Both encodings cover overlapping but not identical information; the
  reader runs Format-B first (during the scanner's struct-discovery
  phase) and Format-A as a post-process to fill in member type ids and
  prune over-collected members.
* **Endianness**: little-endian throughout for multi-byte integers.
* **String encoding**: ANSI (`TEncoding.ANSI.GetString`), see
  [DPT.Rsm.BufferIO.pas:116](DPT.Rsm.BufferIO.pas#L116).

The scanner walks the stream byte-by-byte from offset 0, dispatching on
the tag byte:

```
while P + 2 < Sz do
  case Buf[P] of
    $28: HandleProcRecord(P);            // proc
    $22: HandleParamRecord(P);           // param (in proc)
    $21: HandleRegVarRecord(P);          // register-passed var (in proc)
    $20: HandleLocalRecord(P)            // stack local (in proc)
         or HandleModuleGlobalLocalTagRecord(P); // module global (out of proc)
    $27: HandleGlobalPrimRecord(P);      // primitive global
    $25: HandleEnumConstantRecord(P);    // enum constant
    $03: HandleEnumDefRecord(P);         // enum type def
    $2A: HandleTypeRegistryRecord(P);    // type registry
    $63: SCOPE_END
  end;
  // single-byte fallback advance on miss
  Inc(P);
```

After the symbol-stream walk completes, the scanner runs
`TRsmStructDiscoverer.Run` (class / record discovery, Format-B field
walks) and the reader's post-process passes
(`TRsmFormatALinker.Run` → `TRsmClassParentDeriver.Run` →
`TRsmCrossUnitParentResolver.Run` → `TRsmScopeLocalEnumBridge.Run` →
`TRsmFieldAliasEnumBridge.Run` → `TRsmPropertyLinker.Run`).

---

## 2. Common primitives

### Length-prefixed identifier

Most record names use the same shape:

```
<NameLen: u8>  <name-bytes>
```

`NameLen` is rejected outside `[1, 64]`. Each name byte must be one of
`A-Z`, `a-z`, `0-9`, `_`, `.`, `$`, `<`, `>`, `,`, `@`. The dotted /
generic / `@`-prefixed punctuation accepts identifiers such as
`TFormMain.Create`, `TList<TFoo>.Add`, `@MyName`,
`TFormMain.Create$ActRec`. See
[DPT.Rsm.BufferIO.pas:74-118](DPT.Rsm.BufferIO.pas#L74-L118).

Some record variants impose narrower limits (e.g. `$27` global-prim caps
`NameLen` at 40); the per-record sections below give exact bounds.

### 2-byte RSM type id

Most type references are 2 bytes wide (`Lo Hi`, LE). The **high byte**
acts as a kind discriminator:

| Hi byte | Meaning                                                          |
|---------|------------------------------------------------------------------|
| `$2E`   | Program-local enum primary id (e.g. `TLightStatus`)              |
| `$2F`   | Some Win64 set-types and structured ids                          |
| `$1E`   | Scope-local enum **alias** (per-(unit,type) ids for same-compilation cross-unit enums) |
| `$04`   | Observed as a cross-unit RTL primary (e.g. `$0441` `TThreadPriority`) |
| `$00`   | Same-compilation shared **secondary** (e.g. `$0002` collides across sibling enums) |
| other   | Plain primitive byte (Integer, string, Int64, ShortString, ...) is encoded as a **single byte** with whatever follows being the next field |

Several scanner branches use the hi byte to decide between a 1-byte
primitive id and a 2-byte structured id; in
[DPT.Rsm.Scanner.pas:733-738](DPT.Rsm.Scanner.pas#L733-L738) for
example, `Hi == $2E` or `Hi == $2F` selects the 2-byte read, anything
else falls back to the 1-byte read.

The well-known compiler-built-in primitive ids — `Integer = $03FD`,
`Word = $0415`, `Double = $041D`, etc. — are documented as a comment on
`TRsmClassMember.PrimitiveTypeId` in
[DPT.Rsm.Model.pas:94-103](DPT.Rsm.Model.pas#L94-L103).

### LSB-as-continuation encoding

Several fields use the LSB of the first byte as a "wide encoding" flag:
when the LSB is 0 the field is a single signed byte; when it is 1 a
following byte extends it into a 16-bit word and the recovered value is
`(W - 1) div 2` (for ordinals) or `(W - 1) div 4` (for BPRel offsets and
some sparse ordinals). This shows up in:

* `TRsmScanner.HandleLocalRecord` BPRel-offset decoder
  ([DPT.Rsm.Scanner.pas:825-839](DPT.Rsm.Scanner.pas#L825-L839))
* `TRsmScanner.HandleEnumConstantRecord` sparse-ordinal form
  ([DPT.Rsm.Scanner.pas:1064-1073](DPT.Rsm.Scanner.pas#L1064-L1073))

---

## 3. Record taxonomy

Tag constants live in [`TRsmTag`](DPT.Rsm.Model.pas).

| Tag    | Name                | Purpose                                                                                                 |
|--------|---------------------|---------------------------------------------------------------------------------------------------------|
| `$03`  | `ENUM_DEF_TAG`      | Authoritative enum type definition: ordered element list + owning unit name                             |
| `$0E`  | `RECORD_SENTINEL`   | Marker preceding every record-type name in the type stream (no sentinel for classes)                    |
| `$20`  | `LOCAL_TAG`         | Stack-local (inside proc) **or** module-level global (outside proc) — dispatcher disambiguates via state |
| `$21`  | `REGVAR_TAG`        | Register-passed variable (e.g. `Self`, `AOwner` for class methods)                                      |
| `$22`  | `PARAM_TAG`         | Stack / open-array parameter                                                                            |
| `$25`  | `ENUM_CONST_TAG`    | One enum element (`(typeId, ordinal, name)`); 4 body variants                                            |
| `$27`  | `GLOBAL_PRIM_TAG`   | Top-level primitive global (Integer, string, Int64, ShortString, object-typed nil)                      |
| `$28`  | `PROC_TAG`          | Procedure / method declaration with address payload                                                     |
| `$2A`  | `TYPE_REGISTRY_TAG` | Type-registry entry — joins a type name to its 2-byte primary id                                        |
| `$2C`  | (Format-A field)    | Named field record carrying the field's type and parent record id                                       |
| `$31`  | `PROPERTY_TAG`      | Property declaration (read-source target); sits in the same Format-A block as the class's $2C records  |
| `$63`  | `SCOPE_END`         | Closes the current proc scope (only effective after at least one local-shaped record has been seen)     |
| `$64`  | `UNIT_USE_INTRO`    | Opens a cross-unit symbol-import segment (`$64 NL UnitName $00 $00 $00`); see §4.17                     |
| `$65`  | `USED_UNIT_LIST`    | Opens a used-unit list. Follows the program/package main file's `$70` `.dpr`/`.dpk` introducer in place of `$64`; accepting it anchors the program module's procs to the program unit — see §4.18 |
| `$66`  | `UNIT_USE_TYPE`     | Imported type reference inside a `$64` segment (`$66 NL TypeName <RVA: u32 LE>`); see §4.17             |
| `$67`  | `UNIT_USE_SYMBOL`   | Imported symbol reference inside a `$64` segment (`$67 NL SymbolName <RVA: u32 LE>`); see §4.17         |
| `$70`  | `UNIT_USE_FILE`     | Source-file record (`$70 NL FileName <RVA: u32 LE>`); the **introducer** of a unit's uses block (`$64` for an imported `.pas`/`.inc`, `$65` for the program's full-path `.dpr`/`.dpk`), decoded into the `SourceFiles` table and used as the proc → declaring-unit anchor — see §4.17 / §4.18 |

`$28 PROC_TAG`, `$25`, `$03`, `$2A`, `$2C`, `$31` and the `$64`
unit-use family are valid both inside and outside a proc scope —
the structural anchors on each handler are tight enough that the
scope state isn't needed to disambiguate. The `$22`, `$21`, `$20`
(local-form), and `$27` paths gate on the scanner's `FScanInProc`
state.

The dispatcher uses **single-byte fallback advance** on shape mismatch:
when a handler rejects a candidate record (`Result := False`), the
outer loop simply `Inc(P)` and re-dispatches the next byte. There is no
indication of "record length" outside the per-tag shape rules, so this
fallback is the only mechanism that lets the parser recover from
incidental tag-byte collisions in arbitrary binary data such as proc
address payloads.

---

## 4. Per-record encoding

### 4.1 `$28` PROC_TAG — procedure / method record

```
$28  <NameLen: u8>  <Name: NameLen bytes>  <addr payload (variable)>
```

`NameLen` ∈ `[1, 64]`. `Name` may be dotted
(`TFormMain.Create`), generic (`TList<TFoo>.Add`), `@`-aliased, or carry
the `$ActRec` closure suffix.

The address payload after the name is **variable-length** and is
decoded by `DecodeProcAddrPayload`
([DPT.Rsm.Scanner.pas:429-629](DPT.Rsm.Scanner.pas#L429-L629)). Dispatch
happens on the byte at `name+0`:

| Sub-tag | Decoder (Win32 / Win64)                                | Notes                                                                       |
|---------|--------------------------------------------------------|-----------------------------------------------------------------------------|
| `$20`   | `TryWin32(name+3)` then `TryWin64(name+3)`             | Simple inline form (DebugTarget Win64 uses this)                            |
| `$A0`   | `TryWin32(name+7)` (Win32) / `TryWin64A0(name+7)` (Win64) | Extended form with type-ref / timestamp metadata; TFW.Win64 standard form |
| `$41`   | `TryWin32(name+4)` only                                | Method-record extended form found in large binaries (`41 02 10 00` header) |
| `$80`         | None                                                   | **External proc reference** (§6.7 closure). The most common non-`$A0` form: 43,330 occurrences in TFW vs `$A0`'s 155,649. Comes in two header variants `$80 $00 $00 <4-byte linker-token>` and `$80 $80 $00 <4-byte linker-token>`; bytes after the header are the proc's normal sub-records (`$21` REGVAR for parameters, `$22` PARAM, `$63` SCOPE_END). The 4-byte token at body+3..+6 is an opaque DCU symbol id of the same family as §4.6.2's `$25 $8A`-form token. No inline address — the linker resolves the actual VA via the matching `$A0` definition in the proc's owning unit. |
| `$00`         | None                                                   | **Cross-reference** (§6.7 closure). 2,802 occurrences in TFW. Same shape as `$80` minus the leading marker. No inline address. |
| other (`$01`..`$1F`, `$84`, ...) | None                              | Mostly false-positive matches in the global `$28` byte scan (incidental `$28 <NL> <name>` triples inside other record bodies). Each tag value seen ≤ 100 times across all of TFW; not a real proc-record form. |

The `$A0` Win32-vs-Win64 dispatch is gated on
`TRsmScanner.Is64Bit`, set in `LoadFromFile` by reading the .exe's
PE-header Machine field (`$014C` = i386, `$8664` = AMD64). When the
scanner is loaded via `LoadFromBytes` / `LoadFromBuffer` (no .exe to
inspect) the flag stays at its caller-supplied value (default
`False`).

**Win32 address encoding** (`TryWin32`, [Scanner.pas:478-500](DPT.Rsm.Scanner.pas#L478-L500)):

```
4 bytes: DWORD = (VA shl 4) or $07
```

The low nibble of byte 0 must be `$07`. The recovered RVA is
`(DWORD >> 4) - $401000` (`$400000` image base + `$1000` `.text` RVA).
Sanity range: `(0, $10000000)` — i.e. up to 256 MB of code.

**Win64 `$20` address encoding** (`TryWin64` in `DPT.Rsm.Scanner.pas`):

```
byte 0: (byte0 and $7F) must be $03         // encoding-kind tag
        byte0 bit 7 = VA bit 4
byte 1: VA bits 5..12  (full 8 bits)
byte 2: VA bits 13..20 (full 8 bits)
byte 3,4: encoded proc-size / local-layout info (not used for address)
```

A variable-length marker `$04 <middle: N bytes> <platform-anchor>
$2E <owner-ref>` follows the address payload (§6.22 closure). The
marker is emitted for every `$28` PROC record on both platforms; only
its final `<platform-anchor> $2E ??` tail is structurally rigid, the
middle bytes carry per-proc parameter-signature data whose decoding
is not currently consumed by any reader path.

```
$04  <middle: 2..5 bytes>  <platform-anchor>  $2E  <owner-ref>
                                                    \____ next record begins immediately after
```

| Position                            | Win32 | Win64 | Meaning                                                                                     |
|-------------------------------------|-------|-------|---------------------------------------------------------------------------------------------|
| +0                                  | `$04` | `$04` | marker start                                                                                |
| middle (length varies per-proc)     | 2..5 bytes | 0..3 bytes | parameter signature shape — `$9C 02` primitive-count prefix observed, plus type-id-shaped sub-references. Not decoded. |
| platform anchor (just before `$2E`) | `$11` | `$3D` | platform fingerprint byte                                                                   |
| platform-anchor + 1                 | `$2E` | `$2E` | end-of-middle marker — see "structural-stability caveat" below                              |
| `<owner-ref>`                       | 1 byte or 2 bytes | 1 byte or 2 bytes | **plain proc**: `$00`; **instance method**: 2 bytes LE = owning class's `$2A` raw type-id (matches `Reader.FindTypeIdByName(<ClassName>)`). |

Concrete Win32 evidence (`Projects/DPT/Test/Win32/DebugTarget.rsm`,
pinned by `Test.DPT.Rsm.Scanner.TestProcMarkerOwnerRefDecodes32`):

| Proc | bytes from `$04` onward | owner-ref |
|---|---|---|
| `TargetProcedure` (plain) | `04 81 03 02 10 11 2E 00` | `$00` → no owner |
| `LocalsProcedure` (plain) | `04 A1 05 02 10 11 2E 00` | `$00` → no owner |
| `TDerived.TouchSelf` | `04 F0 02 98 11 2E 21 2E` | `$2E21` LE = `TDerived` ✓ |
| `TStaleSelfHost.Probe` | `04 F1 02 02 98 11 2E E5 2E` | `$2EE5` LE = `TStaleSelfHost` ✓ |
| `TPropHost.Create` | `04 01 02 02 31 03 11 2E D5 2E` | `$2ED5` LE = `TPropHost` ✓ |
| `TPropHost.GetCalcInt` | `04 34 02 9C 02 D5 2E` *(short form)* | `$2ED5` LE = `TPropHost` ✓ (same class, no `$11 2E` middle anchor) |

Win64 differs only in the platform-anchor byte (`$3D` instead of
`$11`); owner-ref shape identical (pinned by sibling 64 test).

**Structural-stability caveat (§6.22 finding).** The `$2E` immediately
after the platform anchor IS NOT a constant — it is the HI byte of a
2-byte LE owner-id pair (`$2Exx` / `$2Fxx`), which happens to fall in
the `$2E00..$2FFF` range for every class observed in the current
DebugTarget + TFW corpora. The `TryWin64` sanity check (`Buf[MOff] =
$04 AND Buf[MOff+3] = $2E`) works in practice but is coincidence-
based. A linker build that scattered class type-ids across a wider
range (`$3Exx`, `$4Fxx`, …) would break the sanity check. Watch for
this if the trailer-check ever starts rejecting valid procs.

**Middle-byte `$9C` primitive prefix (§6.23 partial finding).** The
short-form marker `TPropHost.GetCalcInt = 04 34 02 9C 02 D5 2E`
includes `$9C` which is the §4.7 primitive-prefix tag — it correlates
with the proc returning a primitive type (Integer here). However,
the SAME information is then re-emitted in plain form by the
subsequent `$20 LOCAL Result $66 $00 $00 $02 $F0` record (Integer's
primitive id `$F002`). The marker middle bytes therefore SUMMARISE
what the following `$21 REGVAR` / `$20 LOCAL` records say in full,
adding no new information beyond what the §4.3 / §4.4 channels
already carry. Decoding the middle bytes is workable but redundant;
no current reader path consumes them.

> This is a §6.23 Round-1 partial finding folded here so the
> middle-byte observation isn't lost. §6.23 itself closed as
> "decoded-but-not-useful": accessor-name-anchored type binding via
> this signature is structurally void on TFW — `TAd` is a plain record
> with no methods, so no accessor proc exists whose signature could
> carry the type. That refutation is summarised in §4.15 Pass 3.

§6.17 closure: the earlier marker check required `byte 1 = $10 AND
byte 4 = $00`, which matched only no-Self procs like
`LocalsProcedure`. Every instance method (`TDerived.TouchSelf`,
`TStaleSelfHost.Probe`) fell through, got `SegmentOffset = 0`, became
`Size = 0` after `RecomputeProcSizes`, and `FindProcContaining` for a
PC inside such a method returned the **preceding** proc whose `Size`
extended across the gap (capped at `$4000`). Live-MCP-only symptom
because the Win64 MCP `Self`/locals tests had been routed through the
Win32 fixture as a workaround; the Win64 reader's identical PC
resolved to the wrong proc. Pinned by
`Test.DPT.Rsm.LocalsReader.TRsmReaderLegacyTests.TestWin64ProcBoundaryNoOffByOne64`
(control + two victim probes).

The recovered VA is masked to 21 bits (`$1FFFFF`), capping at ~2 MB of
code per binary. Small Win64 binaries (e.g. DebugTarget at ~1.3 MB)
fit; larger binaries route through `$A0` instead and don't hit this
ceiling.

**Win64 `$A0` address encoding** (`TryWin64A0`, [Scanner.pas:502-549](DPT.Rsm.Scanner.pas#L502-L549), §6.2 + §6.7 closures):

Same 4-byte LE wire format as Win32 (`(VA shl 4) or $07`) but the
encoder stores only the **lower 32 bits** of VA -- the Win64 image
base `$140000000` sits in VA bits 32-39 which the 4-byte slot
doesn't carry. The recovered SegmentOffset is therefore
`(DWORD >> 4) - $1000` (subtracting only the .text-section RVA,
**not** the Win32 image-base + .text constant `$401000`).

Verified on TFW.Win64 by
[Test.DPT.Rsm.Tfw.TRsmTfwTests.TestTfwWin64ProcAddressDecodesAboveCap](../Test/Test.DPT.Rsm.Tfw.pas):
`TFormMain.Create` payload bytes 3..10 = `A0 00 00 F4 A2 28 8C 07
05 E1 7B` -- `DWORD(7..10) = $7BE10507`, `>> 4 = $07BE1050`,
`-$1000 = $07BE0050`, which matches the .map entry
`0001:07BE0050 Tfw.Main.Form.TFormMain.Create` byte-exactly. The
three other pinned probes (`TFormMain.AfterMenuRebuild`,
`TFormVBh.Create`, `TFormVBh.CreateGsVBhBridge`) cover the same
window; together they pin the [~94 MB, ~129 MB] RVA range that the
historical 21-bit decoder mishandled.

The 28-bit encoding capacity caps recoverable RVAs at 256 MB; TFW's
Win64 .text is ~129 MB so the cap doesn't bite there. Whether the
linker emits a wider slot for binaries beyond 256 MB is unknown
(no corpus to test against).

**Duplicate-name handling**: the first `$28` for a name creates the proc
with whatever `Decoded` address (possibly 0). A later `$28` with the
same name and `Decoded > 0` patches the existing entry. This handles
the forward-declaration-then-definition pattern.

**Scope side-effect**: a successful `$28` flips `FScanInProc := True`,
resets `FScanLocalIdx` and `FScanRegParam` to 0, and clears
`FScanSeenLocalSinceProc`.

### 4.2 `$22` PARAM_TAG — stack / open-array parameter

```
$22  <NL: u8>  <Name>  $62 $00 $00  <typeId-lo: u8>  [<typeId-hi: u8>]  <pad>  <next-record-tag> ...
```

`NL` ∈ `[1, 64]`. The 3-byte anchor `$62 $00 $00` confirms the shape;
without it the record is rejected.

Type id decode (post-§6.15 structural disambiguator,
[Scanner.pas:DecodeTypeIdByStructuralLookahead](DPT.Rsm.Scanner.pas)):

* **Fast path** — if `Hi (byte +4) ∈ {$2E, $2F}` (the well-known
  structured-type markers), read as a 2-byte LE id at +3..+4.
* **Otherwise** — distinguish 1-byte vs 2-byte form by the position
  of the next record's tag byte:
  * Byte +5 ∈ `{$20, $21, $22, $25, $28, $63, $03}` → **1-byte**
    typeId at +3, byte +4 is padding, next record starts at +5.
  * Byte +6 in that set → **2-byte** typeId at +3..+4 (full LE
    read), byte +5 is padding, next record starts at +6.
  * Neither — fall back to a 1-byte read (preserves the
    open-array hidden sub-record `$20 $21` detection that the
    caller's post-advance heuristic uses).

The fallback structural read is necessary for cross-unit RTL types
whose linker-minted per-binary alias id has a Hi byte outside the
$2E/$2F gate (TThreadPriority `$0671`, TEncoding `$06xx`, etc.).
Without the disambiguator the 1-byte truncation collided with
whatever foreign type was registered at the truncated id — `TSatz33`
in TFW for `KonsCommonLoad(AKonsCommonTyp: TKonsCommonTyp)`.

The per-binary alias the linker emits in the typeId slot has no
direct entry in the `$2A` type registry (e.g. `$0671` resolves to
the unrelated TWordArray in DebugTarget.rsm's registry, not to
TThreadPriority's `$3370` primary). The alias is bridged to its
enum's element list via the F-prefix / A-prefix naming bridge
documented in §4.15.

Open-array parameters use **two register slots** (pointer + high-index).
After consuming the param body the scanner peeks the next 2 bytes; if
they start with `$20 $21` (a hidden high-index sub-record), it
increments `FScanRegParam` once more so subsequent parameters retain
correct register indices. See
[DPT.Rsm.Scanner.pas:752-756](DPT.Rsm.Scanner.pas#L752-L756).

Stored as `TRsmLocal` with `Kind = lkRegister`, `RegParamIdx =
FScanRegParam`.

> **Consumer note — register params carry no frame offset.** A `$22` /
> `$21` record records only *which* register slot carries the value, not
> a stack home (`BpOffset` stays 0). Delphi receives `Self` and the
> leading scalar params in EAX/EDX/ECX (Win32) or RCX/RDX/R8/R9 (Win64),
> but a method *with a stack frame* immediately spills each to a frame
> home in the prologue and reloads it from there for every later access.
> So at any PC past the prologue the live register is **stale** — reading
> it yields whatever the body last computed (the original `evaluate
> Self.FAd` → wrong pointer on TFW's `TFormAd`). The debugger therefore
> recovers the home slot by scanning the prologue for the spill store
> (`mov [ebp-NN], eax` = `89 45 NN` on Win32; `mov [rbp+NN], rcx` =
> `48 89 4D NN` on Win64) rather than trusting the register —
> [DPT.Debugger.pas TryFindRegParamSpillDisp](DPT.Debugger.pas) feeding
> `GetLocals`. When no spill was emitted (leaf / unspilled param) it
> falls back to the live register. Pinned by
> `Test.DPT.MCP.Server.TestMcpEvaluateSelfFromSpillHomeAfterRegisterClobber`.

> **Consumer note — VMT trumps RSM TypeIdx for class-instance dotted
> walks.** The dotted-walk evaluator (`DPT.Debugger.EvaluateVariable`)
> has a *priming* step that flips it into record-hop mode when the
> first segment's RSM TypeIdx resolves to a record. For register-passed
> `Self` / class params this priming must be **skipped**: the alias id
> the scanner reads from the `$22` / `$21` payload can map to an
> entirely unrelated record in a different build's type registry,
> while the live VMT is authoritative for the actual class. Observed
> on a TFW build: `Self`'s PARAM-record `TypeIdx = $073D` resolved to
> `TMemoryPoolPos` (a record), wrongly routing every `Self.<field>`
> evaluate into the record-hop branch and exiting before ever calling
> `FindClassMember`. The fix: when the first hop is `lkRegister` and a
> VMT walk on its value yields a class name present in `FClasses`,
> bypass the record-hop priming and let the class-hop branch use the
> VMT-derived class name. (Records by-reference still prime, because
> the VMT walk fails on a non-class pointer.)

> **The local/param `TypeIdx` is a per-proc ref, NOT statically
> resolvable to a type name on large binaries (decided — design limit).**
> The `<typeId-lo>` the structural lookahead reads is a per-(unit,type)
> *reference number* into a per-proc local type table the format does
> not expose, NOT the `$2A` registry primary. Verified by four
> hypotheses against `C:\MSE\TFW\TFW.rsm` (proc `TFormMain.Create`,
> `Self` lo `$84`):
> - **(A) refuted** — not a hi-truncated 2-byte registry id (`$F884`
>   isn't registered by any T/P `$2A` entry).
> - **(B) refuted** — not the type's registry primary (`TFormMain`'s
>   primary is `$DBD0` on this build, not `$84`).
> - **(C) confirmed** — type-stable within a class (`TFormMain.Create`
>   and `TFormMain.AfterMenuRebuild` both carry `Self` lo `$84`), so it
>   indexes a per-(unit,type) table, but one not in the `.rsm`.
> - **(D) refuted** — NOT an index into the §4.17 `$64`/`$66` unit-use
>   (import) table. `AOwner: TComponent` carries lo `$10` in
>   `TFormMain.Create` but `$66` in `TFormVBh.Create` (same type,
>   different ref); `TComponent`'s actual unit-use index in
>   `Tfw.Main.Form` is neither; and `TFormMain` (Self's type) is absent
>   from its own unit's import table entirely yet `Self` still has a ref.
>   Pinned by
>   [Test.DPT.Rsm.Tfw.TestTfwPerProcRefIsNotUnitUseTableIndex](../Test/Test.DPT.Rsm.Tfw.pas).
>
> Consequence for any consumer (live debugger AND static viewer):
> - **`Self`** — derive the class from the **VMT** (live) or the
>   **qualified proc name** split at the last `.` outside `<>` (static).
>   Never `FindClassIdxByRsmTypeId` on a local/param `TypeIdx`.
> - **a class-typed param** (`AOwner: TComponent`) — **no reliable
>   static source exists**; the VMT of the live instance is authoritative
>   (the note above). A no-process viewer can only show the raw id,
>   labelled un-resolved.
> - **compiler temps** (the unnamed `.` local) — no static type.
>
> The three id namespaces must never be cross-looked-up: the `$2A`
> registry primary, this `$21`/`$22` per-proc ref, and the
> `FindStructByTypeIdx` file-offset key (`TRsmClassInfo.TypeIdx`, the
> only globally-unique and trustworthy one for member-type resolution).
> Small-binary regime contrast: on `DebugTarget` the slot carries the
> owning class's real 2-byte registry primary (hi `$2E`/`$2F`) and
> resolves correctly — pinned by
> [TestSelfTypeIdxResolvesInCleanRegime32/64](../Test/Test.DPT.Rsm.LocalsReader.pas).
> The large-binary per-proc collapse is pinned by
> [TestTfwSelfTypeIdxIsPerProcRefNotRegistryId](../Test/Test.DPT.Rsm.Tfw.pas).
> (Was tracked as §6.27; closed as a design limit once Hyp D — the last
> decode lead — was refuted, since no reader change can recover a name
> the format does not carry and the consumer doesn't need it.)

### 4.3 `$21` REGVAR_TAG — register-passed variable

```
$21  <NL: u8>  <Name>  $66 $00 $00  <typeId-lo>  [<typeId-hi>]  <pad>  <next-record-tag>
```

Identical layout to `$22` except the anchor is `$66 $00 $00`. Used for
method-call `Self` / `AOwner` / etc. that ride EAX/EDX/ECX (x86) or
RCX/RDX/R8/R9 (Win64). Type id decode uses the same structural
disambiguator as §4.2.

### 4.4 `$20` LOCAL_TAG — stack local OR module-level global

Two forms sharing the same tag byte:

#### Stack-local form (when `FScanInProc` is True)

```
$20  <NL: u8>  <Name>  <typeinfo + BPRel-offset payload>
```

Decoded by `HandleLocalRecord`
([DPT.Rsm.Scanner.pas:801-905](DPT.Rsm.Scanner.pas#L801-L905)). The
payload starts at `P + 2 + NL`. Two main shapes:

**Shape A — structured-type id with BPRel offset:**

```
... <typeId-lo at +3> <Hi at +4 in {$2E, $2F}> <ofs0 at +5> [<ofs1 at +6>]
```

If the LSB of `byte5` is 0, BPRel offset is `ShortInt(byte5) div 2`
(single-byte form). If the LSB is 1, the offset is
`(SmallInt(byte5 or (byte6 shl 8)) - 1) div 4` (wide form).

Type id is `byte3 | (byte4 shl 8)`.

**Shape B — primitive-type id (single byte) with BPRel offset:**

The decoder looks at what follows the candidate offset byte:

* If `byte5 ∈ {LOCAL_TAG, PROC_TAG, SCOPE_END}` → byte 4 is the offset
  (single-byte form, `ShortInt(byte4) div 2`), type id is `byte3`.
* Otherwise if `byte6 ∈ {LOCAL_TAG, PROC_TAG, SCOPE_END}` → bytes 4..5
  form the wide offset, type id is `byte3`.

When neither shape recognises, `Loc.BpOffset` keeps its synthesized
fallback `-10000 - (FScanLocalIdx * 4)`. The
`TestEdgeCaseLocalsAllDecoded` and `TestLocalsHaveDistinctOffsets`
tests assert that **no synthesized fallback survives** on
`DebugTarget.LocalsProcedure` / `EdgeCaseLocalsProcedure` — i.e. all
encodings exercised there must round-trip.

**UNCERTAIN / GAP**: BPRel encodings outside the cases above (some less
common Delphi types) may still hit the fallback on unfamiliar binaries.
`TestEdgeCaseLocalsAllDecoded` is the canary.

Also: every `$20` record additionally publishes the `(name → 2-byte id)`
pair into the global maps `FGlobalByName` / `FGlobalFileOffset`
([Scanner.pas:877-902](DPT.Rsm.Scanner.pas#L877-L902)), because the
`FScanInProc` gate cannot reliably distinguish a stack local from a
module-level variable in every code path.

#### Module-global form (when `FScanInProc` is False)

```
$20  <NL: u8>  <Name>  $66 $00 $00  <id-lo>  <id-hi>  <VA: 4 bytes>
```

Decoded by `HandleModuleGlobalLocalTagRecord`
([Scanner.pas:907-942](DPT.Rsm.Scanner.pas#L907-L942)). `NL` ∈ `[1, 40]`.
The `$66 $00 $00` at +0..+2 (after the name) is the validation anchor;
the 2-byte type id is read at +3, +4. The 4-byte VA slot at +5..+8
shares the encoding with the `$27 GLOBAL_PRIM` and `$28 PROC_TAG` Win32
forms — see §4.5 for the decode and platform semantics. Stored in
`FGlobalVa[lower(name)]` when the `$07` low-nibble tag is present.

Side effects: writes `FGlobalByName[lower(name)] := id`,
`FGlobalFileOffset[lower(name)] := P`, and (when the VA slot tag
matches) `FGlobalVa[lower(name)] := decoded value`.

**Dispatcher routing caveat**: `HandleModuleGlobalLocalTagRecord` is
only reached when `FScanInProc` is False. In practice, module-global
records often appear inside what the scanner still believes is a proc
scope (because the previous `$63 SCOPE_END` was suppressed by the
`FScanSeenLocalSinceProc` guard). `HandleLocalRecord` then runs over
the bytes; it detects the `$66 $00 $00` anchor at body+0..+2 (which no
stack-local record ever carries) and publishes both the type id and
the decoded VA into the global maps as a fallback. See
[Scanner.pas:884-901](DPT.Rsm.Scanner.pas#L884-L901).

### 4.5 `$27` GLOBAL_PRIM_TAG — top-level primitive global

```
$27  <NL: u8>  <Name>  $66 $00 $00  <id-lo>  [<id-hi>]  <VA: 4 bytes>
```

Decoded by `HandleGlobalPrimRecord`
([Scanner.pas:944-1011](DPT.Rsm.Scanner.pas#L944-L1011)). `NL` ∈ `[1, 40]`.
Anchor `$66 $00 $00` immediately after the name.

Type-id decode is **the most general** of all the tag handlers: if the
hi byte at +4 is one of `$2E`, `$2F`, `$1E`, the id is 2 bytes; else it
is the single byte at +3. The VA slot's start position depends:

* 1-byte primitive id → VA at +4..+7
* 2-byte structured id → VA at +5..+8

The `$1E` case is the **scope-local enum alias**: same-compilation
cross-unit enums (e.g. `TStatus` declared in three sibling units) get a
shared secondary id at compile time, and Delphi additionally allocates
a per-consumer-unit alias id with hi-byte `$1E`. All variables of the
same `(unit, type)` pair share the SAME alias id — that's the property
the `TRsmScopeLocalEnumBridge` post-pass exploits to bind every variable
of that id to the correct `EnumDef` via a single anchor variable.

Important: this branch does **not** gate on `not FScanInProc`. The
comment at [Scanner.pas:947-957](DPT.Rsm.Scanner.pas#L947-L957) explains
why — early-region globals (`GGlobalInt`, `GGlobalString`) get silently
skipped when an earlier proc opened `InProc` but emitted no
local/param/regvar record to flip `FScanSeenLocalSinceProc`.

#### VA slot encoding

The 4-byte VA slot is encoded as a single LE DWORD:

```
DWORD = (Value shl 4) or $07
```

The `$07` low nibble is the same validation tag the Win32
`$28 PROC_TAG` form uses (see §4.1). The decoded `Value` (`DWORD shr 4`)
is platform-specific:

| Platform | `Value` is                                          | Recover absolute VA via |
|----------|-----------------------------------------------------|-------------------------|
| Win32    | Absolute VA (image base $00400000 already included) | `Value` (no math)       |
| Win64    | RVA relative to the image base ($140000000)         | `$140000000 + Value`    |

Pinned by
[Test.DPT.Rsm.Scanner.TestGlobalVADecodedFromGlobalRecord32/64](../Test/Test.DPT.Rsm.Scanner.pas)
against `DebugTarget.map` ground truth (`GGlobalInt` 1-byte id,
`GGlobalLight` 2-byte id, `GFieldHost` $20 module-global form). The
decoded value is published via `FGlobalVa[lower(name)]` on the scanner
and surfaced through `TRsmReader.TryGetGlobalVa`. The 4 bytes are
stored only when the `$07` tag is present; coincidental matches whose
low nibble differs are skipped silently.

### 4.6 `$25` ENUM_CONST_TAG — single enum element

Four body shapes, discriminated by content:

#### 4.6.1 Program-local form (8-byte body)

```
$25  <NL>  <Name>
$0A $00 $00  <typeId-lo>  $2E $00 $00  <2*ordinal: u8>
```

Anchor: byte 0 of body must be `$0A`, byte 4 must be `$2E`.

Ordinal is stored doubled (the LSB acts as the form discriminator, same
trick as BPRel offsets). Recover `ordinal = body[7] >> 1`. The type id
is built as `body[3] | ($2E << 8)`.

Reaches `RecordProgramLocalConstant(typeId, ordinal, name)`.

#### 4.6.2 Cross-unit RTL form (12-byte body)

```
$25  <NL>  <Name>
$8A $00 $00  <linker-token: u32>  <typeId-lo>  <typeId-hi != $00>  $00 $00  <2*ordinal: u8>
```

Anchor: `$8A $00 $00` opens the body, `typeId-hi` is non-zero (real
cross-unit primary, e.g. `$04` for `TThreadPriority` → id `$0441`).

The 4-byte slot at body offset +3..+6 was historically labelled
"RVA" (§6.5 in earlier revisions), but a direct dump across
DebugTarget Win32 and Win64 (whose image bases and section layouts
differ wildly) shows **identical values byte-for-byte for the same
source-level enum constant**. The values follow the linear pattern
`base + ord * 3` across an enum's elements (e.g. for `TThreadPriority`:
`tpIdle=$914BDCE9`, `tpLowest=$914BDCEC`, `tpLower=$914BDCEF`, ...,
`tpTimeCritical=$914BDCFB`) — a 3-byte stride per ordinal that does
not correspond to any plausible code or data offset in the binary,
and is too large (28 bits set) to be an in-image RVA at all. The
slot is an **opaque linker token** (likely a `.dcu`-internal symbol
id or similar) that the debugger has no use for. The decoder
correctly skips it.

Ordinal is `body[11] >> 1`. Reaches `RecordCrossUnitRtlConstant`.

#### 4.6.3 Same-compilation cross-unit, single-byte ordinal (11-byte body)

```
$25  <NL>  <Name>
$8A $00 $00  <linker-token: u32>  <secId-lo>  $00 $00 $00  <2*ordinal: u8>
```

(Same opaque linker token as §4.6.2; see that section for its
observed `base + ord * 3` behaviour.)

`typeId-hi == 0` AND `body[10]` LSB == 0. Ordinal = `body[10] >> 1`.

The secondary id is `body[7]` (single byte); the `$00`s at +8, +9, +10
form the padding before the ordinal.

Reaches `RecordCrossUnitSameCompConstant(secId, ordinal, name)` —
constants are **buffered** until the matching `$2A` registry entry
arrives (because the secondary collides across sibling-unit enums).

#### 4.6.4 Same-compilation cross-unit, sparse / 2-byte ordinal (12-byte body)

```
$25  <NL>  <Name>
$8A $00 $00  <linker-token: u32>  <secId-lo>  $00 $00 $00  <ord-byte-lo>  <ord-byte-hi>
```

`typeId-hi == 0` AND `body[10]` LSB == 1.

The ordinal uses Delphi's LSB-as-continuation form:
`W = body[10] | (body[11] << 8); ordinal = (W - 1) >> 2`.

Also reaches `RecordCrossUnitSameCompConstant`. Used for enums with
explicit ordinal values like `(a = 1, b = 100)` or sparse `(a = 128, ...)`.

> **Named `const`s also emit `$25` records — and carry NO per-record
> discriminator (§6.26 closure, refuted premise).** The `$25` tag is not
> exclusive to enum *elements*: the linker emits the same record shape
> for named ordinal **constants** (`crDefault`, `vkLButton`,
> `CSIDL_APPDATA`, `mrYesToAll`, `mrOk`, `IDOK`, …). They are not merely
> "similar" — they are **byte-structurally identical** to genuine enum
> elements. A direct dump of DebugTarget.rsm pins this: the enum element
> `saReady` (`TStatus`) and the consts `mrOk` (`TModalResult`),
> `crDefault` (`TCursor`), `vkReturn` all carry the same-comp body
> `$8A $00 $00 <token: u32> <secId> $00 $00 <2*ord>` with the HiByte at
> +8 == `$00` for the element AND the consts alike — i.e. they all
> decode through §4.6.3's same-comp path. The only varying slots are the
> opaque token (+3..+6, §4.6.2) and the 2-byte `secId` (+7), and the
> `secId` is a **type identity, not a kind flag**: it differs between two
> const families (`mrOk` `$20` vs `crDefault` `$7A`) AND collides across
> distinct types (`vkReturn` shares `mrOk`'s `$20`). There is therefore
> no byte at any fixed offset that the scanner could read to reject a
> const up front. Win32 and Win64 bodies are byte-identical for these
> probes. Pinned by
> [Test.DPT.Rsm.Scanner.Test25NoConstVsEnumElementDiscriminator32/64](../Test/Test.DPT.Rsm.Scanner.pas).
>
> The **only** signal that separates a genuine enum element's type from a
> const's subrange type is **type-level**: the enum's type has a
> `$03 ENUM_DEF` record (`TStatus`, `TLightStatus`, `TMsgDlgBtn` each
> have one), while a const's subrange type does not (`TModalResult`,
> `TCursor` have none). That is exactly the discriminator
> `TRsmReader.FilterPhantomEnumDefs` (§7) exploits downstream — it is not
> a stopgap awaiting a per-record signal, because no such per-record
> signal exists. Until the flush runs, the consts land in the same
> pending buffer and are flushed into `FEnumConstNames` like any other
> constant; the over-collection this causes in the synthesized `EnumDef`
> list is fenced off by the guards documented in §4.8 (jobs 3–4), §5.1,
> and the `$03`-coverage filter (§6.25). The constants themselves remain
> available for `(typeId, ord)` lookup.

### 4.7 `$03` ENUM_DEF_TAG — enum type definition

```
$03  <NL: u8>  <TypeName>
$01 $00 $00 $00 $00  <MaxOrd: u8>
( $00 ) * P                          ← VARIABLE-length zero padding
( <ElemLen: u8>  <ElemName> ) * (MaxOrd + 1)
<UnitLen: u8>  <UnitName>
```

`NL` ∈ `[2, 40]`. The fixed header prefix after the name is
`$01 $00 $00 $00 $00 <MaxOrd>` (6 bytes): the `$01` at +0, zero bytes at
+1..+4, and the max-ordinal at +5. **It is followed by a
VARIABLE-length run of zero bytes `P` before the first element.** The
pad width is toolchain-dependent: DebugTarget's build emits `P = 7`
(first element at +13, the historically-assumed fixed offset), but
DPT.exe's build emits `P = 11` (first element at +17). The reader
**skips the zero run to the first non-zero `ElemLen`** rather than
assuming a fixed +13 — hard-coding +13 made `HandleEnumDefRecord` read a
`$00` length on DPT.rsm and bail, so NONE of its `$03` records parsed
and all ~730 of its enums fell through to the lossy `$25`/`$2A`
synthesis (§6.25). The anchor (`$01`, zeros at +1..+4 and +6..+12) still
holds for both widths since +6..+12 are within the pad run. Pinned by
[Test.DPT.Rsm.Scanner.TestEnumDefParsesWithVariableHeaderPadding](../Test/Test.DPT.Rsm.Scanner.pas)
(synthetic CSH7 buffer, both 7- and 11-pad). A zero is unambiguously
padding because `ElemLen` ∈ `[1, 64]` is never `$00`.

Element count is capped at 512 to defend against coincidental `$03`
byte hits. Each element name is a `[1, 64]` length-prefixed identifier.
The unit name follows immediately after the last element.

**The dispatcher does NOT advance `P` past the body** (see comment at
[Scanner.pas:1149-1151](DPT.Rsm.Scanner.pas#L1149-L1151)). The single-byte
fallback advance re-walks the body but, since none of the inner bytes
form a valid record start under the strict shape checks of other
handlers, this is harmless.

Element ordinals default to **sequential `0..N-1`**. Sparse /
explicit-value enums (`type T = (a = 1, b = 5)`) skip the `$03`
channel entirely — the linker does **not** emit an `ENUM_DEF` record
for them at all (verified by
[Test.DPT.Rsm.Scanner.TestSparseEnumResolvesViaEnumConstNames32](../Test/Test.DPT.Rsm.Scanner.pas)
against `TSparseEnum = (seAlpha = 1, seBeta = 5, seGamma = 11)`).
Per-element ordinals for sparse enums arrive through the `$25` channel
instead, where the program-local form's `body[7] >> 1` already
recovers the explicit ordinal. The consequence is that
`TRsmReader.EnumDefs` does NOT list sparse enums; consumers that walk
the element list directly will miss them, while those that look up by
`(typeId, ordinal)` via `TryGetEnumConstantName` keep working. A
synthetic `EnumDef` could be built from the buffered `$25` records
the way the same-comp cross-unit case does (see §5.1) — the scanner
already has all the pieces, only the synthesis step is missing —
but that is a design choice, not a format gap.

The decoded record is appended to `FEnumDefs` (i.e. two sibling units
declaring the same `TStatus` produce two `TRsmEnumDef` entries, not
one).

### 4.8 `$2A` TYPE_REGISTRY_TAG — type-registry entry

```
$2A  <NL: u8>  <Name>  <BodyFlag: u8>  $00 $00  <primary-lo>  <primary-hi>  <payload? (variable)>
```

`NL` ∈ `[2, 40]`. The `$00 $00` at +1, +2 after the name is the only
hard anchor; the **`BodyFlag` byte at +0 is a body-shape selector**,
NOT a kind discriminator. Observed values within DebugTarget's
program-local registry cluster:

* **`$00` — narrow body**: the entire payload after the primary id is
  a single `$00` pad byte at +5; the next record (`$2A` again, or any
  other tag) starts immediately at +6. Both program-local enums
  (`TLightStatus`, `TSyncDirection`, ...) and "lightweight" records
  (`TPoint3D`, `TNarrowInts`, `TFloats`, `TEnumHostRec`,
  `TFieldStatusHost`, ...) use this form. The bytes the existing
  scanner reads from +7, +8 as `SecCandidate` for these entries
  belong to the next record — see §5.1 for why this is harmless.
* **`$20` — wide body**: at +5..+8 sits a 4-byte LE DWORD encoding
  a pointer to the type's RTTI structure. The wire format matches the
  per-platform proc-address encoding documented in §4.1:
  * **Win32**: `(VA shl 4) or $07` — recover `VA = DWORD shr 4`.
    Example: TInner's `87 2F DB 04` LE = `$04DB2F87`, `>> 4` =
    `$004DB2F8`, which lands 172 bytes past the .map symbol
    `DebugTarget..TInner` (`0001:000DA24C` → VA `$004DB24C`),
    i.e. inside the per-class RTTI metadata the linker emits there.
  * **Win64**: 21-bit packed form `(byte0 and $7F)==$03; bit 7=VA
    bit 4; byte 1=VA bits 5..12; byte 2=VA bits 13..20`. Example:
    TInner's `03 10 A5 26` decodes to SegmentOffset `$149200`.
  All program-local classes use this form (TInner, TDerived,
  TDeepDerived, TClassFieldHost, TWithRec, TNoFPrefixHost,
  TThPriHost, ...) plus the subset of records the linker emits
  full RTTI for. The record discriminator (§6.6.2 closure, pinned
  by `TestRecordRttiFlagDiscriminator32`):

  > A record gets `$20` iff it has **at least one RTTI-managed
  > field** (string / dynarray / interface / Variant / WideString
  > — i.e. `managed-field count > 0` in §4.13's simple-shape
  > header byte 0) **OR** it appears as a **non-variant field
  > type** of another record / class. Variant-case references
  > don't promote the flag.

  In DebugTarget the rule sorts cleanly:

  | Record           | managed | non-variant ref       | Flag |
  |------------------|---------|------------------------|------|
  | TPoint2D         | 0       | TRect2D, TWithRec      | `$20`|
  | TRect2D          | 0       | TWithRec               | `$20`|
  | TWhdrHeader      | 0       | TWithHeader            | `$20`|
  | TPair            | 1       | TWithRec               | `$20`|
  | TMixedRec        | 1       | —                      | `$20`|
  | TWithHeader      | 1       | —                      | `$20`|
  | TPrimitives      | 2       | —                      | `$20`|
  | TPoint3D         | 0       | —                      | `$00`|
  | TFloats          | 0       | —                      | `$00`|
  | TNarrowInts      | 0       | — (packed)             | `$00`|
  | TVariantSlot     | 0       | — (variant)            | `$00`|
  | TFieldStatusHost | 0       | — (packed)             | `$00`|
  | TEnumVariantHost | 0       | — (packed variant)     | `$00`|
  | TEnumHostRec     | 0       | TEnumVariantHost.FInner (**variant-case only**) | `$00`|

  TEnumHostRec is the pin's canonical "variant-refs do not
  promote" probe: it IS referenced by TEnumVariantHost, but only
  inside a `case ... of` branch, and the flag stays `$00`.
  The word at +7/+8 is also what the scanner reads as
  `SecCandidate` for the enum bridge; it's the high half of the
  RTTI-pointer DWORD and carries a legitimate value only because
  the address ends up in the `$04xxxxxx` window in DebugTarget.
* **`$08`, `$80`, `$88`, `$98`, `$A8`, `$B8`** — **cross-unit RTL body
  shapes** (§6.6 closure). All six carry the same 5-byte header
  (`<flag> $00 $00 <primary-lo> <primary-hi>`) the existing scanner
  reads, followed by a per-shape body. Empirically pinned against
  102,243 `$2A` entries in TFW.rsm with the distribution `$00`: 39,797 ·
  `$20`: 3,390 · `$A8`: 27,570 · `$88`: 21,177 · `$80`: 9,856 · `$98`:
  306 · `$B8`: 118 · `$08`: 29.

  | Flag | Type family                                                                                  | Body content (after the 5-byte header)                                                                                                                                                                                                                                       |
  |------|----------------------------------------------------------------------------------------------|----|
  | `$08` | Array helper types only (`TByteArray`, `TWordArray` — 2 distinct names, 29 hits)             | `$9C $11 <2-byte ref>` then a `$FF $2A <NL> <Name>` sibling cross-reference (e.g. `PByteArray`). Fixed ~13-byte body.                                                                                                                                                        |
  | `$80` | Primitive aliases (`WideChar`, `LongInt`, `LongWord`, `NativeInt`, `Int16`, `UTF8Char`, ...) | Two sub-shapes: ~38% extend with `$9C $13 <2-byte ref>` (alias to a registered primitive); ~44% are bare 9-byte name-alias headers immediately followed by the next `$2A` record.                                                                                            |
  | `$88` | Heavier primitive aliases (`Real`, `Real48`, `Extended80`, `Openstring`, `Text`, `PFixedInt`, `TVisibilityClass`, `TDispatchMessage`) | Same family as `$80` plus extra payload — typically `$9C $13 <2-byte ref> $08 $08` (primitive-with-extra) or `$08 $50 <NL> <C-decorated-name> $00`. Body length clusters at 9, 10, 19, 20.                                                                       |
  | `$98` | WinAPI typed handles (`HANDLE_PTR`, `HCOLORSPACE`, `HGLRC`, `HDESK`, `HSTR`, `HTASK`)         | Very uniform: `$F6 $FF $81 $02 <primary> <secondary> $9C $12 <2-byte ref> $2A $13 <same ref>`, with the body carrying the Pascal-to-C mangled `B p<n>H<NAME>` handle decoration. Body length 18-27.                                                                          |
  | `$A8` | Densest flag: core primitives + all RTL classes + structural records (`Boolean`, `Char`, `Integer`, `TObject`, `TGUID`, `TPersistent`, `TStrings`, `TComponent`, `TInterfaceEntry`, `TMethod`, `Pointer`, ...) | Fixed 12-byte prefix `<flag> $00 $00 <prim4> <2-byte sec> $01 $04`; ~65% extend past 32 bytes with `$9C $13 <2-byte ref> $08 ...` followed by C-equivalent name strings or sub-records (`$A0`, `$26`, `$42` markers all appear). |
  | `$B8` | Typed aliases with cross-references (`TFontName`, `TAlphaColor`, `TImageIndex`, `HWND`, `HHOOK`, `TScrollStyle`)                          | Same 12-byte `... $01 $04` prefix as `$A8`, then `$9C $12 <2-byte ref> $08 <len>` followed by an `$FF $2A <NL> <Name>` cross-reference to a related type.                                                                                                                      |

  The 4 bytes at +3..+6 in all six forms show uniform-nibble
  distribution — they are NOT the `(VA shl 4) | $07` RTTI-pointer
  encoding the `$20` form uses, but an opaque DCU-internal symbol
  token (no usable VA). The `$9C $1x <2-byte ref>` sub-marker that
  most bodies carry is a type-alias to another registered name —
  redundant with the `FRsmTypeIdToClassIdx` lookups the Reader gets
  for free via the primary id. The `$FF $2A`-prefixed cross-references
  re-point at already-registered siblings. **Net Reader value: zero**
  — every name that matters is also registered as its own `$2A`
  entry, and the only field that isn't redundant (the 4-byte token at
  +3..+6) is not resolvable to anything the debugger needs without an
  additional reverse-engineering pass into DCU-internal symbol tables.
  The decoder walks past these bodies via the single-byte fallback
  and the resulting state is complete for every observed evaluation
  path.

The kind hypothesis ("flag distinguishes class vs. record vs. enum
vs. primitive") is **refuted**: classes and records both appear under
`$20`, while enums and records both appear under `$00`. The pinning
test
[Test.DPT.Rsm.Scanner.Test2ATypeRegistryFlagIsBodyShapeNotKind32](../Test/Test.DPT.Rsm.Scanner.pas)
fixes this finding against DebugTarget.

The primary 2-byte id sits at +3, +4.

Crucially, the registry entry can be parsed **even when it isn't an
enum** — class names and record names also register here, populating
`FRsmTypeIdToClassIdx` (via `TRsmFormatALinker.ScanTypeRegistry`) and
`FTypeIdByName` (via the same pass).

**Owning-unit forward scan** (only when same-comp $25 constants are
pending). The owning unit name is the unit-init proc — a dotted
PROC_TAG whose name is a NAMESPACE (`'DPT.Dcu.Diff'`,
`'System.Variants'`, `'DebugTarget.EnumAlpha'`) — but it sits at the
END of the unit's symbol block, after all the unit's method PROC
records. So the scan uses two decoupled steps (`HandleTypeRegistryRecord`):

* **Synthesis gate (tight, 1 KB):** synthesize an `EnumDef` only when a
  dotted proc sits within 1 KB of the `$2A`. This keeps the synthesis
  set tight — widening the gate floods the `EnumDef` list with non-enum
  `$2A` entries whose nearest proc is far (a 1 KB→64 KB widening alone
  took DebugTarget 132→166 synthesized defs).
* **Name search (wide, 1 MB):** for a gated entry, search forward for
  the first dotted proc that is a clean dotted NAMESPACE — first
  segment NOT a Delphi type identifier (`T`-class / `E`-exception /
  `I`-interface + uppercase, which rejects methods like
  `'TDcuDiff.ListEntries'`) and no generic angle brackets (which
  rejects specialization procs like `'Collections.NewList<…>'`). The
  unit's own methods/specializations all precede its init proc, so the
  FIRST clean namespace IS the owning unit (no overshoot into the next
  unit). The window is wide because a large class pushes the init proc
  far out (TFW/DPT's `TMcpServer` ~21 KB; JCL/Indy units up to ~1 MB).
  If no clean namespace turns up the entry falls back to the nearby
  gate proc (a method name) so the enum is **never dropped** — an
  earlier attempt that dropped the def when no clean unit name was
  found collapsed DPT.rsm 95→22 (§6.25).

Pinned by
[Test.DPT.Rsm.Scanner.TestEnumDefsNotOverCollected32](../Test/Test.DPT.Rsm.Scanner.pas)
(no `EnumDef.UnitName` is a `TClass.Method`). The residual — a unit
whose init proc is >1 MB out or absent still falls back to a method
name — is tracked in §6.25.

The dispatcher does not advance `P` past the body (single-byte fallback
re-walks; harmless under tight shape checks).

`TRsmEnumDecoder.RecordTypeRegistry` then runs four jobs:

1. Bridge `primary → secondary` alias list — but only when the
   secondary candidate has been seen in a prior cross-unit `$25` record
   with the **exact same 2-byte secondary id** (filter via
   `FCrossUnitEnumIds`).
2. **Append a `(SecondaryLow, Primary, $2A file offset)` entry to
   `FLowByteEnumRefs`** when the secondary's LOW byte was seen by any
   prior same-comp $25 record (looser filter via `FCrossUnitLowBytes`).
   The $2A side routinely carries a HI byte the $25 side didn't (e.g.
   $25 registers `$002A`, the matching $2A surfaces `$072A`), so the
   exact-2-byte filter in job 1 rejects most same-comp cross-unit enums;
   this looser bridge captures them and the `$2C` field linker uses it
   via `FindNearestPrimaryByLowByte` to resolve enum-typed field bodies
   to their owning-unit primary (see §4.9).
3. Flush every pending same-comp $25 constant under the primary id, so
   `FEnumConstNames[primary:ordinal] := name`.
4. Synthesise an `EnumDef` from the pending buffer when the forward-scan
   recovered an owning unit name — **but only if the buffered constants
   carry unique ordinals**. A genuine Delphi enum cannot have two
   elements sharing an ordinal, so a buffer with duplicate ordinals is
   not one enum but an over-collection of unrelated named-`const`
   families (e.g. `Winapi.SHFolder`'s `CSIDL_*` and `SHGFP_*` both start
   at ordinal 0; `Vcl.Controls`/`System.UITypes`' `cr*`/`mr*`/`vk*` runs
   merged under a `LongWord` alias `$2A` like `TColorRef`). Such a buffer
   is still flushed into `FEnumConstNames` (job 3) for `(typeId, ord)`
   lookup but produces **no** `EnumDef`. This duplicate-ordinal guard is
   the sole over-collection fence (the class-method unit-name rejection
   that once accompanied it was reverted — see the forward-scan note
   above and §6.25). Pinned by
   [Test.DPT.Rsm.Scanner.TestEnumDefsNotOverCollected32](../Test/Test.DPT.Rsm.Scanner.pas).

### 4.9 `$2C` Format-A field record (no separate `TRsmTag` constant)

The Format-A field records are the **second source** of class / record
member information (Format-B is the structural discoverer in
`TRsmStructDiscoverer`). The Format-A linker reads them after the
scanner completes.

```
[FF]?  $2C  <NL: u8>  <Name>  $00 $02 $00  <field-id-lo>  <field-id-hi>
  <variable body>
$07 $00 $00 $08  <parent-id-lo>  <parent-id-hi>
```

* The optional `$FF` prefix marks a continuation block (a non-first
  field in a block).
* `NL` ∈ `[2, 40]`. The `$00 $02 $00` at offset `NL+2`..`NL+4` is the
  validation anchor.
* The end-of-record marker `$07 $00 $00 $08` is searched in a bounded
  window `[After+5, After+30]`.
* The 2 bytes following the terminator are the **parent record id**
  (encoding-specific, see below).

#### Parent id encoding

Two encodings are observed:

* **Wide encoding** (`parent-hi != $FF`): the 2-byte id is a "real" RSM
  primary that appears in the `$2A` registry. The linker resolves it
  via `FindClassIdxForRawId` (which uses the
  `FRsmTypeIdToClassIdx` map populated by `ScanTypeRegistry`).
* **Narrow encoding** (`parent-hi == $FF`): the parent id is a
  **unit-local** byte (`parent-lo`). These collide across units, so the
  linker falls back to a **block-owner index** that pairs each $2C
  block's start offset with the owning record found via the source-
  declaration order in the same unit. See `BuildBlockOwnerIndex` in
  [DPT.Rsm.FormatALinker.pas:254-440](DPT.Rsm.FormatALinker.pas#L254-L440).
  This bridge is critical for the TFW corpus where unit-local id `$44`
  is `TUserKonsOutlook` in one unit and an unrelated class in another.

  The block-owner pairing is a heuristic: it assumes the unit's `$0E`
  record markers appear in the same source-declaration order as the
  `$2C` field blocks. The §6.9 round-6 closure (FieldId → Enum bridge)
  validated this on every enum-typed `$2C` field in TFW
  (TUserKonsOutlook's 12 fields paired correctly, plus the broader
  scan over 113 `$2C` records via the LOW-byte → primary lookup), so
  no observed failure remains across the DebugTarget + TFW corpus.
  If a future fixture exposes an out-of-order unit, the §6.9 enum
  bridge's nearest-offset disambiguation provides the safety net.

#### Body shapes — field type information

`BodyLen = EndOff - After` (i.e. distance from the anchor to the
terminator). The linker discriminates four body shapes
([FormatALinker.pas:672-741](DPT.Rsm.FormatALinker.pas#L672-L741)):

| Body shape           | Description                                                                          |
|----------------------|--------------------------------------------------------------------------------------|
| `BodyLen == 14`      | Numeric primitive (Integer, Word, Byte, Int64, UnicodeString, Single, Double, Extended). `PrimitiveTypeId` = 2 bytes at `EndOff - 5`. |
| `BodyLen == 15`      | Numeric primitive with extra leading byte (Boolean, Currency, ...). Same recovery rule. |
| `BodyLen == 9` + `$9C $01` at `After+5..+6` | Managed reference primitive (AnsiString, WideString, ShortString). `PrimitiveTypeId` = 2 bytes at `After+3..+4`. |
| `BodyLen >= 10` + `$9C $01` at `After+6..+7` | Enum-typed field (compact form). See enum bridge below. |
| `BodyLen >= 11` + `$9C $01` at `After+7..+8` | Enum-typed field at parent offset ≥ 256 (extra separator). See enum bridge below. |

When the `FieldId` resolves to a known class/record (via
`FindClassIdxForRawId`), `Member.TypeIdx` is set to the discovered
class's `TypeIdx`. Otherwise `Member.PrimitiveTypeId` is populated
through one of the body-shape rules above. **Members not confirmed by
any Format-A record are pruned by `PruneSpuriousMembers`** to remove
Format-B over-collection from the backward window scan.

#### Enum-typed field bridge (LOW-byte + nearest-offset)

For the two enum-typed body shapes above, the linker checks
`byte(After+5) = $0C` (the same-compilation cross-unit marker — the
two-byte-offset separator the compiler emits for these fields). When
the marker matches, the body's byte `After+3` carries only the **LOW
byte of the secondary enum id** (NOT a 2-byte primary). The linker
then calls
`TRsmEnumDecoder.FindNearestPrimaryByLowByte(low, TagOff, primary)`
which looks up the LOW byte in `FLowByteEnumRefs` (populated by §4.8
job 2 above) and picks the entry with the smallest
`|TagOff - RegistryOffset|`. The nearest-offset rule is a per-unit
selector by construction: same-unit `$2A` enum entries sit within tens
of KB of the field block, cross-unit entries with the same LOW byte
sit megabytes away. `Member.PrimitiveTypeId` is set to the resolved
primary.

When the `$0C` marker is absent (program-local enums, cross-unit RTL
enums whose typeId IS the primary, non-enum primitives that happen to
hit the BodyLen>=10 path), the linker falls back to the original
2-byte read of `After+3..+4`. This preserves historical behaviour for
everything except the same-comp cross-unit enum-typed field case
that §6.9 round-4 isolated.

Sample (`TUserKonsOutlook.SyncDirection` in TFW.rsm, TagOff `$A8E6C35`):

```
$2C body: 00 02 00 2A 0D 0C 9C 01 C9 02 07 00 00 08
                   ^^ +3 = LOW byte $2A
                      ^^ +4 = per-record slot index (ignored)
                         ^^ +5 = $0C marker (same-comp cross-unit)
                            ^^^^^ +6..+7 = $9C $01 enum reference

FindNearestPrimaryByLowByte($2A, $A8E6C35, primary):
  scans FLowByteEnumRefs[$2A] (1399 entries in TFW), nearest is
  TUserKonsOutlookDirection's $2A at $A8E658B (distance ~$6AA),
  returns primary $7B7F. PrimitiveTypeId := $7B7F.
```

The pin lives at
[Test.DPT.Rsm.Tfw.TRsmTfwTests.TestTfwEnumTypedFieldResolvesToPrimary](../Test/Test.DPT.Rsm.Tfw.pas).

### 4.10 `$63` SCOPE_END

A single byte. Closes the active proc scope **only when**
`FScanSeenLocalSinceProc` is True (see
[`TRsmScanner.ScanSymbolStream`](DPT.Rsm.Scanner.pas)'s SCOPE_END
arm). The guard
prevents incidental `$63` bytes in the proc's address payload (the
`$A0` sub-form's payload is ~18 bytes of arbitrary data that routinely
contains `$63`) from prematurely closing the scope before
`Self`/params/locals have been read.

Note: `$63` is a regular byte that happens to be the chosen tag, so it
collides with the literal letter `c` (ASCII `$63`) in identifier
characters too. The `SeenLocalSinceProc` guard plus the
single-byte-advance fallback are what keep this tolerable in practice.

### 4.11 Class trailer pattern (no tag byte — discovered by shape)

Classes are not introduced by a `$0E` sentinel; instead the
`TRsmStructDiscoverer.Run` byte walker recognises them by a duplicated
length-prefixed name with a `$07` type tag between, after a 4-byte
(Win32) or 8-byte (Win64) zero pad:

**Win32 trailer**:
```
<NL>  <name>  04 00 00 00  07  <NL>  <name>  58 00 00 00
```

**Win64 trailer**:
```
<NL>  <name>  08 00 00 00 00 00 00 00  07  <NL>  <name>  C8 00 00 00 00 00 00 00
```

The duplicated length-prefixed name with the `$07` tag between is the
stable cross-platform marker. The constant DWORDs (`$58` / `$C8`) at
the end are the class-record size pointers but are not used by the
discoverer.

**For classes that declare methods**: the method records sit between
the first class-name and the trailer, pushing the trailer past the
tight `NameEnd + 4` / `NameEnd + 8` offsets. The discoverer falls back
to `FindClassTrailerWithin` which scans up to **16 KB** forward for the
trailer pattern ([StructDiscoverer.pas:597-648](DPT.Rsm.StructDiscoverer.pas#L597-L648),
call site [#L723](DPT.Rsm.StructDiscoverer.pas#L723)). The method-block
header is a **small DWORD at NameEnd+1..+4 whose high three bytes are
zero** (`+2..+4 = 0`; the low byte at `+1` is the actual record count /
size and varies per build — observed `0x1D` on the deployment build's
`TFormAd`). The type-record region's same-name occurrence has non-zero
garbage at all four bytes, so the high-three-zero filter cleanly
separates the two. Note: an earlier version of this filter also
required `+1 = 0` and happened to work on the C:\MSE\TFW corpus where
that byte was incidentally zero — but missed every method-bearing class
on builds emitting a non-zero count.

The 16 KB window (raised from an earlier 8 KB) is needed because a
class's method block can be large — `TFormAd` (~12.6 KB) and RTL
`TStream`/`TWriter` exceeded 8 KB and so were never discovered. But the
window cannot simply be widened: a class name occurs many times in the
stream, and an earlier *spurious* occurrence that also passes the
4-zero method-header gate can, with a wide window, reach and **steal a
far class-def's trailer**, winning the `FindClassIdxByName` dedup with
the wrong anchor (this regressed `TComponent` field discovery on
DebugTarget). So `FindClassTrailerWithin` aborts when it meets a
**closer same-name occurrence that is itself class-def-shaped** (4-zero
header, or the tight `04 00 00 00 07` / Win64 `08…07` marker via
`SecondNameMatchesBytes`) — the trailer belongs to that nearest
class-def, not a far one. Correctness is therefore window-independent;
16 KB is only the perf/coverage balance (`DiscoverAndParseAllStructs`
~20 s vs the 45 s budget; 64 KB was ~54 s). Pinned by
`Test.DPT.Rsm.Tfw.TestTfwClassInstanceFieldResolves` (TFormAd
discovered) with `TestMcpEvaluateInheritedFieldViaVmtWalk` (TComponent
not mis-anchored) green on both platforms.

Within the trailer-finding loop, the **two bytes immediately before**
the class name's length byte are captured as `ParentRawId`:

```
<parent-id-lo>  <parent-id-hi>  <NL>  <name>  ...
```

This 16-bit id is non-zero only when the class declares a **cross-unit
parent** (e.g. user class inheriting `TComponent`); same-unit
inheritance leaves these zero and is resolved by
`TRsmClassParentDeriver`'s layout heuristic.

### 4.12 `$0E` record sentinel (no length-prefixed body)

```
$0E  <NL: u8>  <RecordName>  ... (record fields follow via Format-B)
```

Recognised by `TRsmStructDiscoverer.Run` when the byte before a
`T`-prefixed length-prefixed identifier is `$0E`. Records have NO
trailer pattern; the `$0E` sentinel alone identifies them.

### 4.13 Format-B field record (for records, between record name and class trailer)

Each field record after a `$0E`-anchored name has the shape:

```
$02  <NL: u8>  <FieldName>
$02 $00  <last-flag>  $00 $00 $00          (6-byte typeinfo prefix)
[<offset>: 4 or 12 bytes per platform]
```

The 6-byte structural anchor `$02 $00 <last-flag> $00 $00 $00` (where
`last-flag ∈ {$00, $02}`) replaces an older "field-name must start
with F" heuristic. See `RsmIsValidFieldTypeinfoPrefix` in
[DPT.Rsm.BufferIO.pas:120-141](DPT.Rsm.BufferIO.pas#L120-L141).

* `last-flag == $00`: non-terminal field.
* `last-flag == $02`: terminal field (record ends here).

For non-terminal fields, the next-field's byte offset within the
record is encoded as:

* **Win32**: `<DWORD next-offset>` immediately after the 6-byte
  typeinfo (i.e. at `TypeinfoEnd + 0..+3`). The byte at
  `TypeinfoEnd + 4` is `$02` (the next field's tag).
* **Win64**: 4 zero pad bytes + `<DWORD next-offset>` + 4 zero pad
  bytes. The next-offset DWORD sits at `TypeinfoEnd + 4..+7`. The byte
  at `TypeinfoEnd + 4` is `$00` (start of the zero pad).

The walker uses the byte at `TypeinfoEnd + 4` as the layout selector:
`$02` → Win32, anything else → Win64. See
[StructDiscoverer.pas:457-471](DPT.Rsm.StructDiscoverer.pas#L457-L471).

Field 0's offset is hard-coded to 0; subsequent offsets come from each
prior field's next-offset DWORD.

**First-field locator**: the bytes between the record-name and the
first field tag form a header whose simple shape (mapped below)
covers **every record observed in the DebugTarget + TFW corpus**,
including TAppCaps which an earlier hypothesis singled out as
needing a separate "elaborate" form. The §6.4 pin
([Test.DPT.Rsm.Tfw.TestTfwSimpleRecordHeaderCoversTfwRecords](../Test/Test.DPT.Rsm.Tfw.pas))
verifies that the gap between PStart and the first valid `$02`-
prefixed field record is exactly `17 + N * 8` (Win32) for the
interface-scope records the §6.3 work also exercised. The walker
still scans up to 4 KB forward rather than jumping to the predicted
offset because the strict typeinfo anchor gives "right first field
or no match" detection essentially for free; on the entire corpus
the scan terminates on the very first iteration.

**Simple-shape record header**. Starting at `RecordNameOff + 1 +
NL + 4` (right after the record-name's size DWORD):

```
byte 0          : managed-field count (N)
                  = number of fields that carry RTTI managed payload
                    (string / dynarray / interface / Variant / WideString)
bytes 1..(K-1)  : zero pad
bytes K..(K+N*W-1) : per managed field — N entries of width W
byte (K+N*W)    : zero separator
byte (K+N*W+1)  : zero
byte (K+N*W+1+1) : field count (number of declared fields)
trailing pad    : zero bytes to the end of the header
```

Platform-specific constants:

| Platform | K (leading pad block) | W (bytes per managed entry) | Trailing pad | Header total length |
|----------|----------------------|------------------------------|--------------|---------------------|
| Win32    | 5                    | 8 (4-byte offset + 4-byte typeinfo placeholder, always zero in observed corpus) | 11 | `17 + N * 8`  |
| Win64    | 5                    | 16 (4-byte offset + 12-byte typeinfo placeholder, always zero in observed corpus) | 19 | `25 + N * 16` |

(For `N = 0`, the K leading pad and the per-entry block collapse so
the field-count byte sits at position 5, with the trailing zero pad
running to byte 16 / 24. The simple shape's "managed entry" lists the
byte offsets of fields the GC must visit when the record is finalised
— see `TPair.FLabel` at off 4 / 8, `TPrimitives.{FAnsi,FWide}` at
off 0,4 / 0,8 etc.)

The field-count byte is the most useful piece for sanity-checking the
forward walker's output: if `Discoverer.Members.Count` disagrees with
the header's declared field count, a field record was missed or an
extra phantom was admitted. The walker currently doesn't enforce this
cross-check; the lock-in is in
[Test.DPT.Rsm.Scanner.TestSimpleRecordHeaderFieldCount32/64](../Test/Test.DPT.Rsm.Scanner.pas)
which reads the header bytes directly.

### 4.14 Format-B field record (for classes, between fields and class trailer)

For **classes**, the field walker is **backward**, not forward —
`TRsmStructDiscoverer.ScanFieldsBackwardFromClassName`. Each field
record has the shape:

```
<DWORD field-offset>  <NL: u8>  <field-name>  $02 $00 <section-flag> $00  <pad..>
```

The DWORD must satisfy `1 <= Off <= $FFFF`: high two bytes zero, and
`Off = 0` is reserved for the VMT pointer (no real class field can
sit at offset 0). The backward window is bounded by `ScanWindow =
64 KB` upward but capped at the previous class's anchor
(`AMinStartOff`) to prevent cross-class leakage.

Up to `MaxFields = 2048` candidates are captured per class. This cap
exists only to bound the `O(Count^2)` overlap-reject and offset-sort
loops; cross-class leakage is prevented by `AMinStartOff`, **not** by
the cap. It must be generous: large legacy VCL forms carry far more
fields than intuition suggests — TFW's `TFormAd` has ~496 (published
controls + strict-private `F`-fields), and the historical `128` cap
silently truncated the list to the earliest-positioned 128, dropping
`FAd` (offset `0x0C5C`) and every later private field. That was the
live `evaluate("Self.FAd")` failure on `Tfw.Ad.Form:2274`; pinned by
`Test.DPT.Rsm.Tfw.TestTfwClassInstanceFieldResolves` (TFormAd.FAd
resolves at `0x0C5C`, with a leakage guard asserting the neighbouring
`TAdPriorityInfo.FPriorityInfoGUID` does not bleed in).

**Structural anchor**: the 4 bytes immediately after `<field-name>`
must form `$02 $00 <section-flag> $00`. Byte +2 acts as a section /
visibility marker (§6.14 closure, pinned by
`TestVisibilityMarkerTaxonomy32` against a TNoFPrefixHost fixture
that declares one field per visibility section):

| Value | Visibility section of this field                                                       |
|-------|----------------------------------------------------------------------------------------|
| $00   | Terminal record (last field of class) OR private field                                 |
| $01   | strict private / protected                                                              |
| $02   | strict protected / public (in non-$M+ classes; the linker collapses both into $02)     |
| $03   | public (in $M+ classes only -- a `published` section anywhere in the class flips $M+)  |

The walker predicate accepts `byte+2 in [$00..$0F]` -- looser than
the observed set so future visibility additions (interface fields,
class-helper-added members, ...) surface as discovered phantoms
rather than as silently dropped fields. The surrounding bytes 0/1/3
of the anchor still pin the shape tightly. This
is the same anchor's leading 4 bytes that the forward record-field
walker validates via `RsmIsValidFieldTypeinfoPrefix` (which checks 6
bytes). The shorter
4-byte form is used here because the terminal field of a class that
declares methods carries non-zero data at byte +4 of its typeinfo
(e.g. `TDerived.FDerivedLabel` ends with `02 00 00 00 01 00 BC 00 ...`);
a strict 6-byte check would drop those fields. The 4-byte anchor plus
the `Off > 0` floor proved sufficient on the DebugTarget + TFW corpus
to filter every phantom match (the known case being the
`<00 00 00 00> 04 Self <typeinfo>` byte sequence that every method-
bearing class emits via its implicit `Self` register-var record). See
[StructDiscoverer.pas:224-261](DPT.Rsm.StructDiscoverer.pas#L224-L261)
for the implementation and
[Test.DPT.Rsm.Scanner.TestNonFPrefixClassFieldsDiscovered32/64](../Test/Test.DPT.Rsm.Scanner.pas)
for the cross-platform pinning test (TNoFPrefixHost surfaces non-F
fields, TDerived/TClassFieldHost keep their terminal fields, no class
acquires a phantom `Self`).

The backward walker tends to **over-collect** (the window is wide and
matches every `<DWORD-off> <namelen> <name>` triple that passes the
4-byte anchor), so the Format-A linker's `PruneSpuriousMembers` runs
as a post-process to drop members that the `$2C` records never
confirmed (see §4.9 and
[FormatALinker.pas:752-786](DPT.Rsm.FormatALinker.pas#L752-L786)).

> **Consumer note — `Member.TypeIdx = 0` is the common case for
> record-typed fields.** The Format-A linker populates
> `Member.TypeIdx` for class-typed fields that resolve to a
> registry-known class, but for **record-typed** and
> **pointer-to-record** fields it leaves `TypeIdx = 0` (the `$2C`
> payload is present but not translated into a usable type id, since
> records share `TypeIdx` with `Classes[]` only via the file-offset
> token, not the 2-byte registry id space). The dotted-walk evaluator
> therefore needs a **name-based fallback** at hops where the
> previous segment was a pointer-to-record (e.g. TFW's
> `TFormAd.FAd: PAd = ^TAd`; DebugTarget's
> `TPtrToRecHost.FRecPtr: PMixedRec`). At the next hop the class-hop's
> `ReadRuntimeClassName` fails on the dereferenced pointer (records
> have no VMT), and the walker falls back to searching `FClasses` for
> a record whose `Members` contain the next segment name. If exactly
> one record matches, `ObjPtr` is treated as that record's base
> address and the hop proceeds as a record-hop. Pinned by
> `Test.DPT.MCP.Server.TestMcpEvaluateClassFieldPointerToRecordDeref`
> against `TPtrToRecHost.Probe` (DebugTarget.dpr:678,
> `FMixedInt = $1F2E3D4C`). After the §6.19 closure (see below)
> the fallback is no longer reached for canonical Delphi pointer
> aliases — it now serves as the backup for non-canonical aliases
> (e.g. a hypothetical `PFoo = ^TBar` that breaks the strict
> `P<X> = ^T<X>` naming convention) so the unique-match guard still
> protects against silent wrong-record picks for those.
>
> **§6.19 closure — pointer-alias TypeIdx binding via the
> `P<X> = ^T<X>` convention.** Two pins:
> `Test.DPT.MCP.Server.TestMcpEvaluateAmbiguousMemberNameDisambiguation`
> (DebugTarget, `$2C`-driven path) and
> `Test.DPT.Rsm.Tfw.TestTfwPointerAliasBindingDiagnoses` (TFW,
> name-convention bridge for `$2C`-less strict-private fields).
> Three pieces of work, all needed:
>
> 1. **`ScanTypeRegistry` captures pointer aliases.** The first-byte
>    filter now accepts both `T` and `P`. For each `P`-prefixed
>    entry whose name is followed by an uppercase letter — the
>    Delphi-canonical shape — the linker strips the leading `P`,
>    prepends `T`, looks the result up in `FClassByName`, and stores
>    the alias's raw id → target record's `TypeIdx` in a new
>    `FAliasToTargetTypeIdx` map. `FRsmTypeIdToClassIdx` is
>    intentionally NOT touched here so a pointer-to-record field's
>    `TypeIdx` stays 0 (the dotted walk uses that to distinguish
>    pointer-to-record from inline record).
> 2. **`LinkFieldsFromFormatA` populates `PointerTargetTypeIdx` for
>    fields whose `$2C` body carries a pointer-alias id.** When
>    `FindClassIdxForRawId` fails the linker consults
>    `FAliasToTargetTypeIdx` and writes the matched target's
>    `TypeIdx` to `Member.PointerTargetTypeIdx`. Covers DebugTarget's
>    `TPtrToRecHost.FRecPtr: PMixedRec` and any field of any class
>    whose `$2C` record the linker actually sees.
> 3. **`BindPointerAliasMembersByNameConvention` covers the
>    `$2C`-less case.** TFW's strict-private F-prefixed instance
>    fields (`TFormAd.FAd: PAd`) are NOT emitted as `$2C` records —
>    only the backward Format-B scan picks them up, and that carries
>    no type information. A new post-process pass after
>    `LinkFieldsFromFormatA` walks every Member where TypeIdx /
>    PrimitiveTypeId / PointerTargetTypeIdx are all 0 and the name
>    starts with `F` + uppercase. If `FTypeIdByName[lower('P' +
>    suffix)]` returns a hit AND that alias has a registered target
>    in `FAliasToTargetTypeIdx`, the member's `PointerTargetTypeIdx`
>    is set to that target. The combined F-name + matching P-alias
>    condition is what keeps the pass from over-binding (`FName:
>    AnsiString`, `FCount: Integer`, etc. have no matching P-alias
>    in the registry).
>
> The consumer in `DPT.Debugger.EvaluateVariable` sees
> `PointerTargetTypeIdx != 0` between segments AND
> `I < High(Segments)` (non-terminal — the terminal-segment guard is
> what makes `evaluate Self.FAd (int)` correctly return the FAd
> pointer value rather than the first DWORD of the dereferenced
> record). On a non-terminal hit it dereferences `FieldAddr` via
> `ReadTargetPointer`, sets `PrevContextIdx` from the target
> record's `TypeIdx`, and the next segment goes through the normal
> record-hop branch — the §6.18 name-based fallback is bypassed
> entirely for canonical aliases. TFW's `Self.FAd.Land` resolves
> through this path now: live `evaluate Self.FAd.Land (type=int)`
> returns `0` = `ltInland` ordinal, matching the manual
> `read_memory` ground-truth byte at `[FAd + 0x169D]`.

**Terminal-field byte width** (design limitation). Unlike records
which carry a size DWORD between the name and the field stream
(§4.13's recovery rule), classes carry no equivalent instance-size
slot anywhere in the RSM byte stream. The class trailer ends with
a per-platform constant DWORD (`$58` Win32 / `$C8` Win64) that
documents the trailer's structural size, NOT the class instance
size (TInner and TWithRec — wildly different instance sizes —
both ship `$58` on Win32). The class's true instance size lives in
its VMT in the `.text` section, out of reach of the symbol container
reader. Consequence: the terminal class field's `Size` stays 0 and
the evaluator falls back to the user-requested type's width. This
covers every concrete evaluation path the debugger needs.

**Inheritance visibility for loosely-packed RTL hierarchies** (design
limitation, §6.12 closure). The `AMinStartOff` cap is the load-bearing
defence against cross-class leakage on tightly-packed sibling pairs
(`TDerived → TDeepDerived`) where loosening the cap re-introduces
`FirstOffs` / `DeriveClassParents` corruption. The trade-off: a parent
class whose field records sit *before* the cap (i.e. before the
parent's own class anchor on the byte stream's previous-class side)
never gets its `Members` populated by discovery, and the Format-A
`$2C` pass updates existing members but does not synthesise missing
ones. Symptom: dotted evaluations into non-TComponent RTL hierarchies
(`TStringList.FUpdateCount` via `TStrings`, `TStream` subclasses,
`TList<T>` subclasses with non-TComponent ancestry) lose inherited-
field visibility, while TComponent-rooted hierarchies — which happen
to be packed tightly enough that the cap doesn't bite — resolve fully.
Pinned by
[Test.DPT.Rsm.LocalsReader.DoTestNonComponentRtlInheritance](../Test/Test.DPT.Rsm.LocalsReader.pas#L708-L737),
which asserts the own-field positive (`TStringList.FCount`) and
documents the inherited-field gap. Closure paths that have been
considered but not implemented (each carries non-trivial regression
risk against the leakage defence): (a) `$2C`-driven member synthesis
with an offset taken from the `$2C` body; (b) a second backward scan
gated on the parent class being resolved-but-empty, using a tighter
shape filter; (c) `$25`-region locality matching analogous to the
discarded `FLastSecondary` experiment (§6.11). The gap is left open
as an accepted limitation rather than a `GAP` because every
investigated closure path either (i) over-collects on the
DebugTarget tight-pair fixtures, or (ii) requires a field-offset
source that the `$2C` body shape does not currently carry.

### 4.15 Per-binary RTL type alias → enum bridge (`F`/`A`-prefix naming bridge)

Cross-unit RTL types (System.Classes.TThreadPriority, Base.Kons.Common.Typ.TKonsCommonTyp, etc.) get a per-binary alias id allocated by the linker. The alias appears as the typeId in `$21` REGVAR records, `$22` PARAM records, and the FieldId slot of `$2C` field records — but has **no entry in the `$2A` type registry**:

* TThreadPriority's `$2A` primary is `$3370`, but its per-binary register-param alias in DebugTarget.rsm is `$0671`
* `$0671` happens to resolve via the registry to the unrelated TWordArray
* TKonsCommonTyp's per-binary alias in TFW.rsm is `$18`, which collides with TSatz33's primary `$0018`

The bridge from the alias to the actual enum element list is recovered from Delphi's identifier-naming conventions. Implemented by
[TRsmFieldAliasEnumBridge](DPT.Rsm.FieldAliasEnumBridge.pas), which runs
as a post-process pass after the Format-A linker populates
`Member.PrimitiveTypeId` for every `$2C` field:

1. **Pass 1 — class fields.** For every class member with `PrimitiveTypeId != 0`:
    * If the member name starts with `F`, strip the leading `F` and prepend `T` to get a candidate type name (`FThreadPriority → TThreadPriority`)
    * If that candidate matches the `TypeName` of any `$03` ENUM_DEF, register `(PrimitiveTypeId → EnumDef)` in the `FScopeLocalTypeIdToEnumDef` map (the same map the `$1E`-marker scope-local enum bridge uses)
2. **Pass 2 — procedure parameters.** For every procedure local with `TypeIdx != 0`:
    * If the name starts with `A` (Delphi argument convention), apply the same `A<Name> → T<Name>` mapping and register if the EnumDef name matches (`AKonsCommonTyp → TKonsCommonTyp`)

After this pass:

* `TRsmReader.IsEnumTypeId(alias)` recognises the alias as enum-typed
* `TRsmReader.TryGetEnumConstantName(alias, ordinal, out name)` delegates to `TryResolveByScopeLocalTypeId`, returning the element name from the cached EnumDef

The bridge is purely **name-based** so it fires only when at least one F-prefixed field or A-prefixed parameter exists for the type in the binary. RTL enum types with neither convention applied (e.g. record fields named just `Typ` and never passed as a register/stack parameter with an A-prefix) still won't auto-detect — they require either a code path that exercises them through a Pass-1 or Pass-2 site, OR an explicit `type=int` argument to `evaluate`. Pinned by
[Test.DPT.Rsm.LocalsReader.TestFieldAliasEnumBridgeResolvesTThreadPriority32](../Test/Test.DPT.Rsm.LocalsReader.pas)
which asserts `IsEnumTypeId($0671) = True` and resolves ordinals
0 / 4 / 6 to `tpIdle` / `tpHigher` / `tpTimeCritical`.

#### Pass 3 — zero-id record/class fields by enum name convention (§6.24 closure)

Passes 1–2 above bridge fields/params that **already carry** a non-zero alias id. A different shape exists in TFW: a record field that **no structural channel ties to its type at all** — the canonical case is `TAd.Land: TLandTyp`, a strict-private field on a plain record. Such a member arrives with `PrimitiveTypeId = TypeIdx = PointerTargetTypeIdx = 0`, and an exhaustive search established that **no purely-structural decode can recover its type** (the conclusions that retired the original `$67`-decode gap):

* there is **no `$2C` field record** binding `Land` to a type id, and **no `$67` use-site** that identifies the *owning field* — the `$67 'ltInland'` use-sites are back-references to the canonical `$25` enum block (their RVA equals the canonical block's, byte-identical across every importer), so they carry the enum identity but not *which* class's `Land` field is being read;
* the importing-unit attribution (§4.17 `$70`/`SourceFiles`) is too coarse — 68 distinct importer scopes use `TLandTyp`, so "unit X imports TLandTyp" cannot single out `TAd.Land`;
* `TAd` is a plain record with **no accessor proc** (`TAd.SetLand` etc. do not exist in TFW), so there is no `$28` proc whose parameter signature could carry the type either.

So the enum **name** is unrecoverable structurally; a name convention is the only remaining signal.

`BindZeroIdFieldsByEnumNameConvention` recovers the enum **name** via the only remaining signal — Delphi's identifier convention that a field `<X>` of an enum type is usually named after that type (`<X>` → `T<X>`, or `T<X>Typ` in TFW's suffix convention). It is a **last-resort heuristic**, gated hard so it never invents a phantom binding. For every class/record member it fires only when ALL hold:

* `PrimitiveTypeId`, `TypeIdx`, `PointerTargetTypeIdx` are **all zero** — never clobber a structural binding;
* the name does **not** match the `F<Upper>` field convention (those are Pass 1 / the §6.19 pointer-alias bridge's territory);
* **exactly one** of `{T<X>, T<X>Typ}` resolves to a genuine, **non-synthesised `$03` `ENUM_DEF`** (a synthesised def may be a §6.25 phantom; the `$03` requirement is the "is it really an enum" oracle);
* that enum type name is **unique** among the `$03` defs (two sibling units declaring the same enum name is ambiguous — the pass does not guess a unit);
* the enum has a non-zero `$2A` registry id (looked up via `FTypeIdByName`).

On a hit it sets `Member.PrimitiveTypeId` to the enum's 2-byte `$2A` id (`RawIdKey` is always ≤ 16 bits) and registers `(id → EnumDef)` in the same `FScopeLocalTypeIdToEnumDef` map Passes 1–2 use, so `TRsmReader.IsEnumTypeId` and `TryGetEnumConstantName` resolve it through the established path and the evaluator's `AutoDetectFormatterName` Path 1 picks `'enum'`. Pinned against TFW by
[Test.DPT.Rsm.Tfw.TestTfwRecordFieldEnumNameConventionBindsLand](../Test/Test.DPT.Rsm.Tfw.pas)
(`TAd.Land` → `TLandTyp`, ordinals 0 / 1 → `ltInland` / `ltAusland`; leakage guards: neighbour `TAd.Waehrung` stays unbound, F-prefix `TFormAd.FAd` keeps its §6.19 pointer-target) and against the clean DebugTarget fixture by
[Test.DPT.Rsm.Reader.TestRecordFieldEnumNameConventionDoesNotPhantomBind32](../Test/Test.DPT.Rsm.Reader.pas)
(no `TMixedInt` enum exists, so `TMixedRec.FMixedInt` stays non-enum; TLightStatus still resolves). The TFW heuristic also legitimately binds sibling domain enums on the same convention (`TAd.Lng`, `TAd.Lf`, `TAd.LicenseKind`).

> **Complementary fallback (the raw-ordinal ceiling).** When Pass 3
> does NOT fire — a record-field terminal whose name has no unique
> `$03` enum match — the dotted-walk evaluator still must not mislabel
> it. `EvaluateVariable` (the discrimination just before
> `BuildAutoDetectHint`, via the nested `TerminalMemberResolvesToClass`
> helper and the `DottedTerminalIsRecordField` flag) emits the raw
> bytes **as `int`** for a terminal reached through the *record-hop*
> branch that carries no type metadata
> (`PointerTargetTypeIdx = 0`, `TypeIdx` not a class, `PrimitiveTypeId
> = 0`), instead of the misleading "possible nil class pointer" hint
> (which is kept only when the terminal could genuinely be a class
> reference — `TypeIdx` resolves to an `skClass`, or it was reached
> through the class-hop branch where nil is a legitimate nil object).
> So a structurally-untyped `Self.FAd.Land` that Pass 3 cannot name
> still reads as its ordinal (`0`), never as a phantom nil pointer.

### 4.16 `$31` PROPERTY_TAG — property declaration

The Delphi compiler emits one `$31` record per `property <Name>: <Type>
read <Field-or-Getter>` clause on a class. Layout:

```
[$FF]?  $31  <NL: u8>  <Name>
    $00 $02 $00            ; Format-A anchor (same as $2C field)
    <prim-type: u8>        ; $02=Integer, $04=string, ...
    $FE $0F $00 $00 $00 $80
    <target-lo> <target-hi>
    ...
```

The 2-byte target at body+10..+11 identifies the read source:

* **Field-backed property** (`property Foo: Integer read FFoo;`) —
  target matches the secondary alias id at body+7..+8 of one of the
  class's `$2C` field records. The bridge resolves the target to the
  field name so callers can navigate `Obj.Foo` through the field's
  byte offset exactly like `Obj.FFoo`.
* **Getter-backed property** (`property Foo: Integer read GetFoo;`)
  — target points at the getter method record. No `$2C` field
  carries the target, so the bridge leaves
  `TRsmClassProperty.UnderlyingField` empty and the caller can
  detect "getter-only" via that signal.

Owning-class attribution: properties sit in the same Format-A block
as the class's `$2C` field records. The block-owner pairing for
WIDE-encoded parent ids (the common case for `$2A`-registered
classes) is NOT covered by
`TRsmFormatALinker.BuildBlockOwnerIndex` (which only handles the
narrow / per-unit-local-id form by design). The property linker
therefore does its OWN walk, tracking the last `$2C` field record's
wide parent type-id and resolving it via
`FRsmTypeIdToClassIdx`.

**Records with methods / properties** (e.g. `TPropRec` in the test
fixture) get the same `$31` shape as classes — the host's `Kind` is
`skRecord` instead of `skClass`, otherwise identical. These records
are typically NOT visible in `Classes` after the StructDiscoverer's
Format-B walk because the discoverer expects a simpler shape than
records-with-methods emit. The property linker handles this by
walking `$2A` registry entries during the same pass to build a
local `(TypeId → Name)` lookup; when a `$2C` parent type-id misses
`FRsmTypeIdToClassIdx`, the linker SYNTHESIZES a `skRecord`
`TRsmClassInfo` (with an empty Members list) from the registry
name so property attribution can proceed. Dotted field navigation
through the synthetic record stays unsupported — only
`Reader.FindClassProperty` works for these types until the
StructDiscoverer is extended to handle records-with-methods.

A second `$2C` body-anchor variant lives here: `$00 $00 $00` (rather
than the canonical `$00 $02 $00`) marks field records that are the
direct read-target of a property. The block-owner index accepts both
to keep the linker's downstream filters intact while still seeing
the wider block (the strict `$00 $02 $00` filter stays in place for
`LinkFieldsFromFormatA`'s actual member-linking pass — variant blocks
just don't contribute member updates, only block ownership).

Implemented in
[TRsmPropertyLinker](DPT.Rsm.PropertyLinker.pas) and exposed as
`TRsmClassInfo.Properties` plus `Reader.FindClassProperty`. Pinned
against two fixtures in
[DebugTarget.dpr](../Test/DebugTarget.dpr):

* Class properties on `TPropHost` —
  [Test.DPT.Rsm.LocalsReader.TestPropertyLinkerSurfacesFieldAndGetterBackedReads32](../Test/Test.DPT.Rsm.LocalsReader.pas):
    * `PlainProp` (field-backed) → `UnderlyingField = 'FPlainInt'`
    * `CalcProp` (getter-backed) → `UnderlyingField = ''`,
      `PrimitiveTypeId = $02` (Integer)
    * `Greeting` (getter-backed) → `UnderlyingField = ''`,
      `PrimitiveTypeId = $04` (string)
* Record properties on `TPropRec` (skRecord, synthesised by the
  linker) —
  [Test.DPT.Rsm.LocalsReader.TestPropertyLinkerHandlesRecordProperties32](../Test/Test.DPT.Rsm.LocalsReader.pas):
    * `RecPlainProp` (field-backed) → `UnderlyingField = 'FRecPlain'`
    * `RecCalcProp` / `RecLabel` (getter-backed) → empty
      `UnderlyingField`, `PrimitiveTypeId` distinguishes Integer vs
      string.

### 4.17 `$64` / `$66` / `$67` / `$70` — cross-unit symbol-import table

The Delphi linker emits one segment per unit a compilation unit
imports a symbol from. Each segment is bracketed by:

```
$64  <NL: u8>  <UnitName>  $00 $00 $00          ; segment open
  (
    $66  <NL: u8>  <TypeName>    <RVA: u32 LE>  ; imported type
    $67  <NL: u8>  <SymbolName>  <RVA: u32 LE>  ; imported symbol
    $70  <NL: u8>  <FileName>    <RVA: u32 LE>  ; source-file ref
  )*
$63                                              ; SCOPE_END (optional)
```

The `$66` / `$67` / `$70` records may appear in any order and may
mix freely within the segment. The 4-byte payload is a little-endian
RVA into the imported unit's canonical declaration slot:

* For `$66 <TypeName>` it equals the bytes at body+3..+6 of the
  canonical `$2A <TypeName>` registry entry (verified against
  TFW's `$66 'TLandTyp'` = `08 90 A6 7D` ⇔ `$7DA69008`).
* For `$67 <SymbolName>` referring to an enum element it equals the
  bytes at body+3..+6 of the canonical `$25 <SymbolName>` block;
  siblings of a single enum carry payloads with a `+3` LSB stride
  matching the element's doubled ordinal (TFW's `ltInland`/
  `ltAusland`/`ltEUNetto`/`ltEUBrutto`/`ltBLNetto` carry
  `81 39 A4 10` / `84 39 A4 10` / `87 39 A4 10` / `8A 39 A4 10` /
  `8D 39 A4 10` — first byte `+3` per ordinal, the trailing three
  bytes conserved). For procedure / method symbols the payload is
  the call target's image RVA, allowing a future consumer to walk
  cross-unit call edges without re-walking the registry.
* For `$70 <FileName>` it identifies the source file the symbol was
  declared in (`.pas` / `.inc`), used by the linker for source-line
  attribution.

**Structural anchor**. The three trailing zero bytes after the unit
name are what tell the scanner this `$64` byte opens a segment
rather than being incidental data inside another record's payload.
TFW.rsm contains tens of thousands of incidental `$64` bytes
(low-byte of an offset, a type-id byte, an `'d'` ordinal inside a
proc-address payload). The scanner's
[`HandleUnitUseIntroRecord`](DPT.Rsm.Scanner.pas) takes a fast-
reject path that checks the trailing zero anchor and the length-
byte range before calling the printable-ASCII validator.

**Inner-entry termination**. The segment walks consecutive
`$66`/`$67`/`$70` entries until one of:

* `$63` SCOPE_END — consumed, segment closes normally.
* A byte outside `{$66, $67, $70, $63}` — the segment ends without
  an explicit close; the next dispatcher iteration takes the byte.
* An entry's name length is outside `[1..64]`, the name fails
  `RsmIsPrintableAscii`, or the payload is all-zero (the latter
  doubles as a leakage guard against long zero runs mimicking the
  shape).

**Counts on real corpora**. TFW.rsm contains roughly 4442 segments
with `UnitName = 'System'` (99.9% with the canonical `$00 $00 $00`
trailer), 1805 segments for `'Base.Types'`, and 68 segments where
the inner refs include `$66 'TLandTyp'`. DebugTarget.rsm carries a
hundreds-of-segments scale table dominated by `'System'` /
`'System.UITypes'` / `'Winapi.Windows'` entries.

**What this table is NOT**. The `$66` / `$67` records list only the
**code-level uses** of imported symbols (procedure calls, RTTI
queries, generic-argument bindings, default-value initialisers).
A record-field-of-type-TLandTyp **declaration** does NOT cause the
owning unit to emit a `$66 'TLandTyp'` or `$67 'ltInland'` entry
against the field's typed slot — verified experimentally (one of the
structural-decode refutations summarised in §4.15 Pass 3, which is why
`TAd.Land` needs the name-convention heuristic).

**Segment-name semantics — the importer IS encoded.** The
`$64 <UnitName>` is the **imported** unit, not the
importer. Each segment lists symbols that the surrounding
byte-stream scope imports FROM `<UnitName>`. The IMPORTING
unit-of-scope is carried explicitly by a `$70 <SourceFile>` record
that sits as an **introducer immediately before** each run of
`$64` segments — NOT as an inner entry inside a segment (see the
"Introducer vs. inner `$70`" note below). The importing unit name
is the `$70` source-file basename with the directory prefix and
`.pas`/`.inc` extension stripped. The attribution rule is: every
`$64`/`$66`/`$67` reference between one `$70` introducer and the
next belongs to that introducer's unit. Verified byte-exact on
DebugTarget.rsm (`$70 'Winapi.ImageHlp.pas'` @ 2041753 → block at
2041779) and DPT.rsm (`$70 'DPT.Application.pas'` @ 56486628 →
block at 56486654). The decoder **captures** this association in a
**normalized** form: each `$70` introducer becomes one
[`TRsmSourceFile`](DPT.Rsm.Model.pas) entry in
`TRsmScanner.SourceFiles` / `TRsmReader.SourceFiles` (`SourceFile`
= raw `…pas` name, `UnitName` = `StripDirAndExt`, plus `StartOffset`
+ `Rva`), and every `$64` segment carries a `SourceFileIdx`
**foreign key** into that list — so the importing unit's name is
stored **once** (deduplicated by name via the scanner's
`FSourceFileByName` map) rather than copied onto every segment of a
block. `HandleSourceFileIntroRecord` recognises the standalone
introducer (`$70 <…pas> <RVA:4> $00 $64`), records/dedups the
source file, and stamps `FCurrentSourceFileIdx` onto the segments
that follow. `TRsmReader.UnitsImporting(<type>)` is the exact
inverse of `UnitsDeclaringType`, resolving each matching segment to
`SourceFiles[Seg.SourceFileIdx].UnitName`. Pinned by
[Test.DPT.Rsm.Reader.TestSourceFileAttribution32/64](../Test/Test.DPT.Rsm.Reader.pas)
(Win32 + Win64): SourceFiles deduped, `Winapi.ImageHlp.pas` →
`Winapi.ImageHlp` with the uses-edge to `Winapi.Windows`
attributable via the FK, no orphan segments, the lone `System.pas`
not recorded, and `UnitsImporting('Boolean')` returning many
importers vs. `UnitsDeclaringType('Boolean')`'s single declarer.

**Introducer vs. inner `$70`**. Although the inner-entry grammar
above lists `$70` as a within-segment source-file ref, in practice
**every** `$70 <…pas>` record is a segment-block introducer: it is
followed by `<RVA:4> $00 $64` and opens the next unit's import
block. Measured: 30/31 such records in DebugTarget.rsm and 362/363
in DPT.rsm are introducers (the lone exception in each is the very
first `System.pas`, after which no `$64` follows because the System
unit imports nothing); **zero** `$70 …pas` records occur as an
inner entry of an open `$64` segment. `HandleUnitUseIntroRecord`
never mis-decodes a standalone `$70` because there is no open `$64`
before it — the dispatcher walks the introducer's bytes singly.

**Proc → declaring-unit IS recoverable from the `$70` introducer
cursor — see §4.18 (this supersedes the former §6.28 negative
result).** An earlier round concluded the cursor was unusable
because it "froze" on the trailing imported `.pas`
(`DebugTarget.EnumGamma`) and mis-attributed every program proc.
That was a **decode bug, not a format limit**: the program's own
source file is emitted as a full-path `.dpr` `$70` record followed
by `$00 $65` (used-unit list), a shape the handler rejected (wrong
extension + `$65` instead of `$64` trailer), so the cursor never
advanced onto the program module. Once the `.dpr`/`$65` introducer
is accepted the cursor lands on `DebugTarget` and the procs resolve
correctly. The full decode and the supporting evidence live in
§4.18.

Decoders live in
[`TRsmScanner.HandleUnitUseIntroRecord`](DPT.Rsm.Scanner.pas) (the
`$64` segments) and
[`TRsmScanner.HandleSourceFileIntroRecord`](DPT.Rsm.Scanner.pas)
(the `$70` introducers / `SourceFiles` table); the segments are
exposed via `TRsmReader.UnitUseSegments`, the source files via
`TRsmReader.SourceFiles`, and the segment→importer link via
`Seg.SourceFileIdx`. Two reverse-lookups close the loop, both
deduplicated and case-insensitive:

* [`TRsmReader.UnitsDeclaringType`](DPT.Rsm.Reader.pas) — units that
  DECLARE a type (a segment's `UnitName` is the unit that declares
  its `$66`/`$67`/`$70` symbols).
* [`TRsmReader.UnitsImporting`](DPT.Rsm.Reader.pas) — its exact
  inverse: units that IMPORT a type, resolved through the `$70`
  source-file attribution.

Pinned by:

* [`Test.DPT.Rsm.Scanner.TestUnitUseSegmentSystemIsDecoded`](../Test/Test.DPT.Rsm.Scanner.pas)
  — DebugTarget's `$64 'System'` segment decodes with a
  `$66 'Boolean'` entry at the canonical RVA `$62BC8138`.
* [`Test.DPT.Rsm.Scanner.TestUnitUseSymbolReferencePayloadHasPlusThreeStride`](../Test/Test.DPT.Rsm.Scanner.pas)
  — three `System.UITypes` `vk*` siblings carry the `+3` LSB stride
  the encoding interpretation depends on.
* [`Test.DPT.Rsm.Scanner.TestUnitUseFalsePositiveRejection`](../Test/Test.DPT.Rsm.Scanner.pas)
  — leakage guard: a synthetic `$64` lacking the trailing-zero
  anchor must not open a segment.
* [`Test.DPT.Rsm.Tfw.TestTfwUnitUseTableContainsTLandTyp`](../Test/Test.DPT.Rsm.Tfw.pas)
  — TFW.rsm carries ≥60 `$66 'TLandTyp'` entries with the canonical
  RVA `$7DA69008`.
* [`Test.DPT.Rsm.Tfw.TestTfwUnitUseTableLtInlandRvaMatchesCanonicalBlock`](../Test/Test.DPT.Rsm.Tfw.pas)
  — `$67 'ltInland'` payload `$10A43981` matches the bytes at
  file offset 54816766 + 11 (the canonical `$25 'ltInland'` block's
  body), anchoring the cross-record structural relationship.
* [`Test.DPT.Rsm.Reader.TestUnitsDeclaringTypeAggregatesAcrossSegments`](../Test/Test.DPT.Rsm.Reader.pas)
  — `Reader.UnitsDeclaringType('Boolean')` returns a deduplicated
  non-empty list of declaring units.
* [`Test.DPT.Rsm.Model.TestTagConstants`](../Test/Test.DPT.Rsm.Model.pas)
  — `UNIT_USE_INTRO` / `USED_UNIT_LIST` / `UNIT_USE_TYPE` /
  `UNIT_USE_SYMBOL` / `UNIT_USE_FILE` byte values pinned alongside the
  rest of `TRsmTag`.

The proc → declaring-unit anchor that this same `$70` cursor provides
is documented and pinned in §4.18.

### 4.18 `$70` source-file introducer as the proc → declaring-unit anchor

**Embarcadero's docs are right that the `.rsm` carries source-level
information** (it is the *Remote Symbol File* deployed in place of the
`.map` for remote debugging). A prior round concluded "the `.rsm` has
no proc → declaring-unit edge" and "source navigation lives only in
the `.map`" — **both claims were wrong** and are corrected here. The
edge IS present; it is the same `$70` source-file introducer §4.17
decodes, scoped to the proc stream.

#### Stream layout

The symbol stream is partitioned **per compilation unit**. Each unit's
block of records (its `$28` procs, `$2A` types, `$25` consts, …) is
immediately preceded by exactly **one `$70` source-file introducer**
naming that unit's own source file, then a uses/used-unit list
(`$35` module-dependency records) before the procs begin:

```
$70 <NL> <ThisUnit'sSourceFile> <RVA:4 LE> $00 (\$64|\$65)   ; introducer
  ... uses-block ($35 module records / $64 import segments) ...
$28 <NL> <ProcName> <addr payload> ...                       ; procs of ThisUnit
$28 ...
```

Two introducer flavours, distinguished by extension **and** the
trailing opener byte:

| Source file | Extension | Trailer opener | Recorded by |
|---|---|---|---|
| an imported unit | `.pas` / `.inc` (basename) | `$00 $64` (`UNIT_USE_INTRO`) | §4.17 (already) |
| the **program / package main file** | `.dpr` / `.dpk` (**full path**) | `$00 $65` (`USED_UNIT_LIST`) | §4.18 (new) |

The program's own main file is the case the prior round missed: it is
carried as a **full path** (e.g.
`C:\WDC\…\Test\DebugTarget.dpr`, name-length `$36` = 54) and is
followed by `$00 $65`, not `$00 $64`. The handler rejected it on
three counts — the `.dpr` extension, the `$65` trailer, and the path
characters (`\` `:` ` `) that the global identifier validator forbids
— so the cursor froze on the last imported `.pas`
(`DebugTarget.EnumGamma.pas`) and mis-attributed every program proc to
`DebugTarget.EnumGamma`. The fixes
([`HandleSourceFileIntroRecord`](DPT.Rsm.Scanner.pas)):

* a **per-extension trailer gate** — `.pas`/`.inc` still require
  `$64` (so the lone `System.pas`, which is followed by `$65` because
  System imports nothing, stays UN-recorded exactly as §4.17 needs),
  while `.dpr`/`.dpk` require `$65`. The widening is surgical: it
  admits only the program/package main file, nothing else.
* a **local path-aware name reader** that accepts `\ / : space ( ) -`
  in addition to the identifier alphabet, kept local to this handler
  so the global `RsmIsPrintableAscii` charset (used by every other
  record) is unchanged.
* `SourceFileToUnitName` strips the directory prefix and the
  `.dpr`/`.dpk` extension too, so `C:\…\DebugTarget.dpr` →
  `DebugTarget`.

#### Proc binding

`HandleProcRecord` stamps the live introducer cursor onto every proc
it creates: `TRsmProc.SourceFileIdx := FCurrentSourceFileIdx` (and
records `TRsmProc.StreamOffset` for reference). Because each unit's
proc block sits between its own `$70` introducer and the next unit's,
the cursor names the **declaring** unit, not "whose uses-block was
seen last". A later definition `$28` patching a forward declaration
refreshes the cursor (preferring a live `≥ 0` value).

`TRsmProc.SourceFileIdx = -1` means the proc precedes the first `$70`
introducer — in practice the linker-synthesised import **thunks**
(`MoveFile`, `CloseHandle`, …), which genuinely have no Delphi
declaring unit in the `.rsm`. `DeclaringUnitOfProc` returns the empty
string for them.

#### Reader facade

* [`TRsmReader.DeclaringUnitOfProc(AProcIdx): String`](DPT.Rsm.Reader.pas)
  — `SourceFiles[Procs[AProcIdx].SourceFileIdx].UnitName`, or `''`
  for a thunk (`SourceFileIdx < 0`) or out-of-range index.
* [`TRsmReader.DeclaringUnitOfProcNamed(AProcName): String`](DPT.Rsm.Reader.pas)
  — name-keyed convenience overload.

#### Verification (ground truth from the sibling `.map`)

`DebugTarget.map` block `Line numbers for DebugTarget(…DebugTarget.dpr)`
declares `TargetProcedure` in unit `DebugTarget`. The decode resolves
it correctly on **both** platforms:

| Fixture | nearest preceding `$70` | `DeclaringUnitOfProcNamed('TargetProcedure')` |
|---|---|---|
| Win32 | `…\DebugTarget.dpr` (full path, `$65`) | `DebugTarget` ✓ (was the wrong `DebugTarget.EnumGamma`) |
| Win64 | `…\DebugTarget.dpr` (full path, `$65`) | `DebugTarget` ✓ |

Generalises to large binaries: on `C:\MSE\TFW\TFW.rsm` the nearest
preceding `$70` before `TFormMain.Create` is `Tfw.Main.Form.pas`
(a normal `.pas`/`$64` introducer, already captured by §4.17) — the
correct declaring unit, with no intervening source-file `$70` in the
window.

Pinned by
[`Test.DPT.Rsm.Scanner.TestProcDeclaringUnitResolves32`/`…64`](../Test/Test.DPT.Rsm.Scanner.pas)
(positive: `TargetProcedure → DebugTarget`; negative guard: NOT
`DebugTarget.EnumGamma`; leakage guard: any `SourceFileIdx < 0` proc
and out-of-range indices resolve to `''`). The former §6.28 negative
pin `TestProcSourceFileCursorIsNotDeclaringUnit32` was replaced by
this positive pin; the §4.17 `System.pas`-not-recorded invariant is
still pinned by `TestSourceFileAttribution32/64` and is unaffected by
the per-extension `$65` gate.

> **What is still NOT decoded (§6.29).** The per-statement
> address → line table (the `.map`'s `Line numbers` arrays) is present
> in the `.rsm` per Embarcadero's docs but in an encoding not yet
> isolated: it is **not** a run of proc-entry address tokens. Encoding
> `TargetProcedure`'s line RVAs (`$DA00A`, `$DA032`, …) in the
> `(VA shl 4) or $07` proc-address wire form and scanning the whole
> file finds only the proc-entry RVA `$DA004` (the `$28` record's own
> address), not the per-statement RVAs — confirming the line table is
> delta/RLE-encoded in a dedicated section (the file is a TDS-style
> container: a 32-byte header carrying the image base + link
> timestamp, a source-path table near offset `$420`, the `$35`
> module-dependency tree, the `$70` source-file records, and a
> trailing data-segment `(VA,size)` table). Decoding the per-line
> deltas is tracked as §6.29; the proc → **declaring-unit** anchor
> this section delivers does not depend on it.

---

## 5. Cross-record state machines

### 5.1 Enum constant flow (`$25` → `$2A` flush)

Three pipelines, depending on the `$25` form:

| `$25` form         | Immediate action                                | Resolver use                                            |
|--------------------|-------------------------------------------------|---------------------------------------------------------|
| Program-local      | `FEnumConstNames[typeId:ord] := name`           | Direct `TryGetEnumConstantName(typeId, ord)`            |
| Cross-unit RTL     | Same as above PLUS `FCrossUnitEnumIds[id] := True` | Direct lookup; primary id is unique cross-unit          |
| Same-comp cross-unit | Buffered in `FPendingConstants`                | Flushed when matching `$2A` arrives; keyed by primary id |

The `$2A` flush walks the buffered constants and writes them under the
**primary** id from the registry entry. It also synthesises a
`TRsmEnumDef` when the unit-name forward scan succeeded — that's the
only way same-comp enums get an `EnumDef` (the `$03 ENUM_DEF` records
exist for them too in principle, but the scanner picks them up
independently and the synthesis is a belt-and-braces fallback).

**A duplicate-ordinal guard keeps the synthesis from over-collecting**
(see §4.8 job 4 for the byte-level detail). The pending buffer is keyed
only by a collision-prone secondary id, so named-`const` families that
the linker emits as `$25` records (cursor/modal-result/virtual-key
constants, the `CSIDL_*`/`SHGFP_*` shell ids, …) accumulate in it and
never match an enum `$2A` of their own — they sit there until some
unrelated non-enum `$2A` (a `LongWord` alias like `TColorRef`, a
function-pointer type like `PFNSHGETFOLDERPATHA`) flushes the whole
pile. Without a guard the synthesis dumped that pile into one bogus
`TRsmEnumDef` (RsmDesk's `TColorRef = 285 elements`). The guard: skip
the synthesis when the buffer carries **duplicate ordinals** — a real
Delphi enum can't, so duplicates prove the buffer is a multi-family
over-collection. The constants are still flushed into `FEnumConstNames`
for `(typeId, ordinal)` lookup; only the spurious `EnumDef` is
suppressed. Pinned by
[Test.DPT.Rsm.Scanner.TestEnumDefsNotOverCollected32](../Test/Test.DPT.Rsm.Scanner.pas).
The residual cases the ordinal guard cannot reach (a clean-ordinal
const family attributed to a wrong non-enum `$2A`, and mislabeled
`TClass.Method` unit names on legit survivors) are tracked in §6.25.

### 5.2 Class parent resolution (three-stage)

Inheritance is **not** explicitly emitted as a class → parent
reference. Three complementary passes derive it:

1. **`ParentRawId` capture** (during `TRsmStructDiscoverer.Run`): the
   two bytes immediately before the class-name length byte are read as
   a 16-bit candidate parent id. Cross-unit parents (e.g.
   `System.Classes.TComponent`) encode here; same-unit parents leave
   it zero.
2. **`TRsmClassParentDeriver.Run`** (layout heuristic): for each
   class `C` with no explicit parent, find the latest preceding class
   `P` whose computed instance size equals `C`'s first-own-field
   offset. A 16-byte tolerance fallback handles the RTL-ancestor case
   where the offset isn't byte-exact. **Tie-breaking is conservative**:
   when two unrelated classes both end at the same byte boundary,
   `ParentName` stays empty rather than risk picking the wrong one.
3. **`TRsmCrossUnitParentResolver.Run`**: for classes with non-zero
   `ParentRawId` but still-empty `ParentName`, look up the raw id in
   `FRsmTypeIdToClassIdx` (built by `TRsmFormatALinker.ScanTypeRegistry`)
   and set `ParentName` from the matching class's name.

### 5.3 Scope-local enum bridge

Globals whose stored type id has hi-byte `$1E` carry a scope-local
enum alias. `TRsmScopeLocalEnumBridge.Run` walks every global name,
looks at the type id's hi byte, and for each `$1E` id:

* Extracts the variable's name's trailing segment after the last `.`,
  then strips a leading `Enum` / `Module` / `Unit` / `Mod` prefix to
  get the conventional unit-short (e.g. `DebugTarget.EnumAlpha` →
  `Alpha`).
* Finds the `TRsmEnumDef` whose unit's trailing-short matches the end
  of the variable name (case-insensitive). The longest match wins.
* Stores the binding in `FScopeLocalTypeIdToEnumDef[typeId] := defIdx`.

Once bound, any variable carrying that scope-local id can be resolved
via `TRsmReader.TryResolveByScopeLocalTypeId(typeId, ordinal)` —
including variables whose own names give no unit hint at all.

The weaker fallback `TRsmReader.TryResolveScopeLocalEnum` uses the same
unit-suffix matching but operates on the variable name directly, with
a last-resort "uses-order last wins" pass when no unit hint applies.

---

## 6. Identified gaps and uncertainties

### 6.25 Same-comp `$25` pending buffer pollutes / mis-attributes synthesized `EnumDef`s (`GAP`)

[DPT.Rsm.EnumDecoder.pas `RecordTypeRegistry`](DPT.Rsm.EnumDecoder.pas)
+ the owning-unit forward scan in
[DPT.Rsm.Scanner.pas `HandleTypeRegistryRecord`](DPT.Rsm.Scanner.pas).
The same-compilation enum flow buffers every `$25` constant until the
next `$2A` flushes it (§5.1). But the linker emits the **same** `$25`
shape for named ordinal **`const`s** (`cr*`/`mr*`/`vk*`/`CSIDL_*`/…,
see §4.6 note), which are byte-indistinguishable from enum elements, so
they accumulate in the buffer and — having no enum `$2A` of their own —
get flushed under whatever unrelated non-enum `$2A` surfaces next
(`TColorRef` = `LongWord`, `PFNSHGETFOLDERPATHA` = function pointer).
The duplicate-ordinal guard (§4.8 job 4) suppresses the **multi-family**
over-collections (the egregious `TColorRef = 285 elements` case), but
three residuals remain undecoded:

* **R1 — clean-ordinal mis-attribution (closed for the common case).**
  A const family attributed to a wrong non-enum `$2A` produces a bogus
  `EnumDef` the ordinal guard can't see (`TPublishableVariantType`, a
  class, carrying `TTypeKind`'s values; `TObjectList` as an "enum").
  Closed by `TRsmReader.FilterPhantomEnumDefs` (§7), which became
  effective only after the `$03` reading-gap fix gave it authoritative
  `$03` defs to compare against — see the detailed write-up further down
  ("the actual reading gap"). It drops a synthesised def that is either a
  real `skClass` or fully covered by a single `$03` enum. **Residual:**
  record / interface / external-struct phantoms (`QOS_OBJECT_HDR`,
  `IRichChunk`, `tagXFORM`) that borrowed NON-`$03` constants — not in
  `FClasses`, no `$03` to map to. Pinned by
  `Test.DPT.Rsm.Reader.TestEnumDefsExcludeClassNames`.
* **R2 — mislabeled unit names (largely closed).** The forward scan
  used to grab the first dotted proc after the `$2A`, which is a class
  method (`TDcuDiff.ListEntries`), not the unit-init proc — so the
  `UnitName` came out as a `TClass.Method` (the RsmDesk report
  `TDcuDiffStatus → TDcuDiff.ListEntries`). Fixed by the wide
  clean-namespace name search (§4.8): DPT.rsm now resolves all 51 enums
  to real units (`DPT.Dcu.Diff`, `DPT.MCP.Server`, `System.Variants`).
  Residual: a unit whose init proc is >1 MB past the `$2A` (or absent)
  still falls back to the nearby method name. The robust closure is an
  offset-indexed post-process (collect every unit-init proc offset in
  one pass, assign each synthesized def the next unit boundary after its
  `$2A`) — deferred; the 1 MB window covers all observed units.
* **R3 — dropped polluted enums.** A legit enum whose buffer got
  polluted by a preceding const run inherits duplicate ordinals and is
  therefore dropped by the guard (its element list was already corrupt,
  so dropping is the lesser evil — but the type is then absent from
  `EnumDefs`).

The root cause is the absence of a byte-level signal separating
enum-element `$25` records from named-`const` `$25` records, and of a
reliable "is this `$2A` an enum?" oracle (the `$2A` body-flag is shape,
not kind — §4.8). Closing it needs either such a signal (RE the `$25`
const-vs-element bytes, or the `$2A` enum-vs-other discriminator) or
gating the flush on the primary→secondary bridge having fired for
*this* `$2A`. **Refuted approach:** rejecting class-method forward-scan
unit names — it dropped legit same-comp enums wholesale on real
binaries (DPT.rsm 95 → 22), reverted in favour of the ordinal-only
guard (DPT.rsm settles at 51, all clean-ordinal enums kept). Pin:
`Test.DPT.Rsm.Scanner.TestEnumDefsNotOverCollected32` holds the
duplicate-ordinal invariant + an over-removal floor on DebugTarget; a
real-binary guard is still missing because DebugTarget is too clean to
exhibit R2/R3 (the over-removal regression was invisible there — it
only showed on DPT.rsm).

**Decisive root-cause question — ANSWERED.** Every fix above (dup-ordinal
guard, `skClass` filter, clean-namespace unit scan) is a downstream
cleanup of the SYNTHESIS path, which exists only because the same-comp
`$2A` flush GUESSES the (type, unit, element-set) tuple. The question
was whether the whole synthesis — and therefore all these filters —
could be removed by sourcing enums from the authoritative `$03 ENUM_DEF`
channel instead. **It cannot.** Two experiments settled it:

1. **Raw `$03` probe** (DebugTarget, removed after use): the linker DOES
   emit a `$03 ENUM_DEF` record for **contiguous** same-comp enums and
   our anchor accepts them — `TStatus` (EnumAlpha), `TLightStatus`,
   `TSyncDirection`, `TFieldStatusKind`, `TFieldUnregKind` each have a
   valid `$03` record (`anchorOK=True`). So for contiguous enums the
   synthesis is REDUNDANT with `$03`.
2. **Synthesis-disabled regression**: turning off the `EnumDef`
   synthesis broke exactly one case —
   `TestMcpEvaluateCrossUnitEnumWithSameTypeName` on **`GStatusBeta`**
   (`DebugTarget.EnumBeta.TStatus`, **sparse**: `sbStopped = 10`),
   which then "could not be auto-typed". **Sparse enums skip the `$03`
   channel** (§4.7) — the linker emits NO `$03` for them, so the
   `$25`→`$2A` synthesis is their ONLY source.

**Conclusion.** The synthesis is unavoidable for *sparse* same-comp
enums; it cannot be deleted. So the filters are NOT merely covering a
"we-fail-to-read-an-existing-record" gap — for sparse enums there is no
clean record to read; the format itself carries only loose `$25`
constants plus a collision-prone `$2A`, and the synthesis must guess.

**RE round — the authoritative enum oracle is `$03`, not a `$2A` byte.**
A direct probe of DPT.rsm (`TestDiagnose2AKindBytes32`, removed after
use) settled where the kind lives. The `$2A` body carries NO
position-stable kind byte — eight DPT enums all show flag `$A8`, but so
do most DPT classes, confirming §4.8 (the flag is shape, not kind).
What DOES discriminate cleanly is the **presence of a `$03 ENUM_DEF`
record**:

```
kind     name                  $2A   $03(ENUM_DEF)
ENUM     TDebugState/TStepType/TRsmStructKind/... (×8)   1   1   (all)
CLASS    TDebugger/TDcuDiff/TBreakpoint/...        (×6)   1   0   (all)
RECORD   TStackFrame/TRegisters/TLocalVar/...      (×6)   1   0   (all)
```

Every (contiguous) enum has exactly one `$03`; no class or record has
any. So the discriminator was never a missing byte to decode — it is the
`$03` record. But wiring it in exposed the REAL root cause:

**The actual reading gap — DPT.rsm's `$03` records never parsed.** On
DPT.rsm the reader produced **51 EnumDefs, ALL synthesised, ZERO from
`$03`** — yet the raw probe found valid `$03` records with valid anchors
for every DPT enum. The cause: `$03`'s element list follows a
VARIABLE-length zero padding (§4.7) — DebugTarget emits 7 pad bytes
(elements at +13), DPT.exe emits 11 (+17). `HandleEnumDefRecord`
hard-coded the element offset at +13, so on DPT.rsm it read a `$00`
length and bailed for EVERY `$03`. So this WAS a "we-fail-to-read-an-
existing-record" gap after all — exactly the original suspicion — just
not the one first guessed. Fixing it (skip the zero run to the first
`ElemLen`) took DPT.rsm from **51 → 756 EnumDefs**, with **730 now
authoritatively `$03`-sourced** (DebugTarget unaffected: still 7-pad).
Pinned by `TestEnumDefParsesWithVariableHeaderPadding`.

**Closure (implemented).** Two parts:
1. **`$03` reading-gap fix** (`HandleEnumDefRecord`, §4.7): the bulk win.
   Real enums now come from the authoritative `$03` channel.
2. **`TRsmReader.FilterPhantomEnumDefs`**: with `$03` defs finally
   present, the reader drops a SYNTHESISED def when (a) its name is a
   real `skClass` (a class is not an enum — catches class phantoms whose
   borrowed constants are not in any `$03`), or (b) all its elements map
   to a single `$03` enum (a phantom that borrowed those constants, OR a
   redundant synthesised duplicate of a contiguous enum). `$03`-sourced
   defs are never removed; sparse enums (no `$03`, name not a class)
   survive. `TRsmEnumDef.Synthesized` carries the source tag.

Result on DPT.rsm: the class phantoms (`TPublishableVariantType`,
`TObjectList`, `TIdSchedulerOfThreadDefault`, …) and the synthesised
duplicates are gone; the 730 real `$03` enums + genuine sparse enums
remain.

**Re-investigation against TFW.rsm (the residual is LARGE, not a
handful — this is the open part).** Probing every surviving synthesised
`EnumDef` on `C:\MSE\TFW\TFW.rsm`
([Test.DPT.Rsm.Tfw.TestTfwSynthEnumPhantomResidual](../Test/Test.DPT.Rsm.Tfw.pas))
shows **114 synthesised survivors** (of 4140 total `EnumDef`s), the
**majority clear phantoms** — far more than the "single-family
const-borrow" remnant this entry previously implied. They cluster by a
**definitively-non-enum naming convention** the filters don't yet use:

* **Pointer aliases `P<Upper>`** — `PtrInt`, `PCLSID`, `PBlobRef`,
  `PAviFileInfoA`, `PKernelEntry`, `PNZWCH`, `PID3D10Blob`, …
* **Interfaces `I<Upper>`** — `IRichChunk`, `IWICDevelopRawNotificationCallback`,
  `IPropertySystem`, `IPenInputPanel`, `IAVIStream`, `ISpellChecker`, …
* **Windows ALL_CAPS / `_`-leading / `tag<lower>` structs** —
  `QOS_OBJECT_HDR`, `_QOS_SD_MODE`, `tagXFORM`, `NET_STRING`,
  `GETTEXTEX`, `DXGI_COLOR_SPACE_TYPE`, `D3DVALUE` (a float typedef!), …
* **Exception / static-class names `E<Upper>` / `C<Upper>`** —
  `EAplError`, `EAppExec`, `CExcel`, `CBarcode`, `CMainForm`, … (these
  are codebase-specific conventions, lower-confidence than `P*`/`I*`).

Genuine sparse enums (`TPictureFormat`, `TLngTyp`, `TKeyIndex`, `TZO`,
`TProgressStatus`, …) are almost all `T<Name>`-prefixed and must
survive. Only ~9 of the 114 are in `FClasses` at all, so a type-kind
oracle can see only those few; the rest carry NON-`$03` constants
(named `const`s / sparse-enum elements) with no `$03` and no `FClasses`
entry — exactly the §6.26-refuted case (no per-record byte
discriminator).

**The `skRecord` kind-oracle is UNSAFE (corrects the stale
`FilterPhantomEnumDefs` comment).** The code comment claims filter (a)
must stay `skClass`-only because the record heuristic mis-registers
`TThreadPriority` / `TUnicodeBreak` as `skRecord`. On current TFW that
example is **refuted** — both are `<not-in-FClasses>`, not `skRecord`.
The *real* counterexample is **`TZUGFeRDXMLObjectTyp`**: a genuine
131-element enum (`xmlNone`, `xmlC…`) that the record discoverer ALSO
registers as `skRecord` in `FClasses`. Extending filter (a) to
`skRecord` would delete it. So `skClass`-only is correct, but for a
different reason than the comment states.

**Closure (c) — naming-convention drop (IMPLEMENTED).**
`FilterPhantomEnumDefs` now has a third drop condition
([DPT.Rsm.Reader.pas `IsNonEnumTypeName`](DPT.Rsm.Reader.pas)): a
SYNTHESISED `EnumDef` (never a `$03`-sourced one, never a name shadowing
a real `$03`) whose name follows a high-confidence non-enum convention
is dropped —
* `I<Upper>` (interface alias): `IRichChunk`, `ISpellChecker`, …
* `P<Upper>` (pointer alias): `PCLSID`, `PBlobRef`, … (`PtrInt` —
  `P`+lowercase — is intentionally NOT matched; conservative.)
* leading `_` (C-translated struct): `_QOS_SD_MODE`, …
* `tag<Upper>` (Windows struct): `tagXFORM`, `tagMSG`.
* ALL-CAPS API typedef (every char `A-Z`/`0-9`/`_`, ≥1 letter, len ≥ 4):
  `QOS_OBJECT_HDR`, `NET_STRING`, `GETTEXTEX`, `D3DVALUE`,
  `DXGI_COLOR_SPACE_TYPE`, …

Leakage guard — **`T<Upper>` is protected first**, so every genuine
enum survives, including the dangerous all-caps `TZO`
(`zoNone`/`zoBilanz`/`zoGuV`) and the dual-registered
`TZUGFeRDXMLObjectTyp` (enum + `skRecord`). Pinned by
[Test.DPT.Rsm.Tfw.TestTfwSynthEnumPhantomResidual](../Test/Test.DPT.Rsm.Tfw.pas)
(asserts the representative phantoms are gone, that NO surviving synth
def still matches a dropped convention, and the leakage guards above);
`TestTfwRecordFieldEnumNameConventionBindsLand` /
`TestTfwEnumTypedFieldResolvesToPrimary` confirm real enum binding is
unaffected.

**Remaining residual (smaller, not high-confidence-droppable).** After
(c), what still leaks is:
* **`T<Upper>`-named const-borrow phantoms** (`TWMNotifyRE`,
  `TThumbButtonState`, `TIdPortList`, … carrying named `const`s) — name
  is byte- and convention-indistinguishable from a real enum, and §6.26
  refuted any per-record byte discriminator, so these are the
  **irreducible** remainder. The `skRecord` kind-oracle can't safely
  prune them either (the `TZUGFeRDXMLObjectTyp` counterexample). The
  multi-family subset is still caught by the dup-ordinal guard.
* **`E<Upper>` / `C<Upper>` families** (`EAplError`, `CExcel`, …) —
  droppable by extending the convention list, but **deferred by design**
  (codebase-specific conventions, higher false-positive risk than
  `I*`/`P*`/Windows-struct).
* **R2 (unit names)** and **R3 (dropped polluted enums)** as before —
  deferred (no current repro / lesser-evil tradeoff).
* `TTokenKind` still doubles (a `$03` plus a synthesised variant whose
  element set only partially overlaps the `$03`).

---

> **Next-gap numbering:** §6 numbers are **stable identifiers**, not
> sequence positions. §6.28 (the proc → declaring-unit attribution
> gap) was reopened **three times** and is now **closed — RESOLVED,
> not refuted.** The first two closures concluded "no proc → unit
> anchor in the `.rsm`"; **both were wrong-premised.** Round 1 refuted
> only the *uses-introducer* cursor reuse. Round 2 ("re-affirmed
> closed: source navigation lives in the `.map`, the `.rsm` carries no
> line table / no proc source index, the program's own source file is
> not even a `$70` record") was the most wrong: its byte search looked
> for line RVAs in the **proc-entry address wire form**
> (`(VA shl 4) or $07`), which structurally cannot find a
> delta-encoded line table even when present, and it missed the
> program's own `$70` because that record is a **full path** with a
> `.dpr` extension and a `$65` (not `$64`) trailer that the handler
> rejected. Round 3 (this one, prompted by Embarcadero's RSM Debug
> File docs) **found the anchor**: the per-unit `$70` source-file
> introducer, scoped to the proc stream, names the declaring unit; the
> handler now accepts the program's full-path `.dpr`/`$65` introducer
> and `HandleProcRecord` stamps `TRsmProc.SourceFileIdx`. The full
> decode, the per-extension `$65` gate, the path-aware name reader, the
> `DeclaringUnitOfProc` facade, the Win32/Win64 `TargetProcedure →
> DebugTarget` verification, and the TFW `TFormMain.Create →
> Tfw.Main.Form` generalisation are in **§4.18**, pinned by
> `Test.DPT.Rsm.Scanner.TestProcDeclaringUnitResolves32`/`…64`. This
> RESOLVES RsmDesk `RD-6`: the declaring unit of a proc IS surfaceable
> from the `.rsm`. (The `.map` remains DPT's source-line navigator for
> breakpoints; that is a consumer choice, not a `.rsm` limit — the
> per-statement line table does exist in the `.rsm` but in an
> un-decoded encoding, now tracked as **§6.29** below.) §6.27 was the local/param
> `$21`/`$22` `TypeIdx` per-proc-ref vs. registry-id confusion) — now
> **closed and removed**: a re-investigation refuted the last decode
> lead (Hypothesis D — that the per-proc ref indexes the §4.17
> `$64`/`$66` unit-use table; see the §4.2 consumer note and
> `Test.DPT.Rsm.Tfw.TestTfwPerProcRefIsNotUnitUseTableIndex`), leaving
> a verified **design limit** (the per-proc local type table is not in
> the `.rsm` and the consumer doesn't need it). The per-proc-ref facts
> and the four-hypothesis history are folded into the §4.2 consumer
> note; nothing open remains. §6.26 was the `$25`
> const-vs-enum-element decode signal — **closed as a refuted
> premise**: the `$25` record carries no per-record const-vs-element
> discriminator (enum-element and const bodies are byte-structurally
> identical; the only signal is type-level `$03` presence), folded into
> the §4.6 "Named `const`s also emit `$25`" callout and pinned by
> `Test25NoConstVsEnumElementDiscriminator32/64`. §6.25 is the same-comp
> `$25`-pending-buffer pollution / `EnumDef` mis-attribution gap (plus
> the `$03` variable-header-padding reading-gap fix). §6.24 was the
> Pfad-3.5 heuristic enum late-binding option for record fields — now
> **implemented and closed**: it lives as Pass 3 of the §4.15 field-alias
> bridge (`BindZeroIdFieldsByEnumNameConvention`), binds TFW's
> `TAd.Land → TLandTyp` (and sibling domain enums) by name convention
> under hard leakage guards, and is pinned by
> `TestTfwRecordFieldEnumNameConventionBindsLand` +
> `TestRecordFieldEnumNameConventionDoesNotPhantomBind32`. §6.23 was the `$28` marker middle-bytes
> investigation — opened and closed in a single Round-1,
> decoded-but-not-useful, the `$9C` primitive-prefix
> observation folded into §4.1 as a footnote. §6.20 (the `$67`
> enum-use-site / `TAd.Land` saga) was **closed and removed**: `$67`
> and the importer attribution decoded into §4.17, the structural
> `TAd.Land → TLandTyp` decode refuted (folded into §4.15 Pass 3), and
> the enum NAME delivered by the §6.24 heuristic — nothing open
> remained.
> The last number used is **§6.29** (the un-decoded per-statement
> address → line table, an OPEN entry below). The next gap discovered
> MUST be numbered **§6.30**, not §6.1. Numbers are never reused or
> recycled — commit messages, code comments, and pin docstrings
> reference closed §6.N entries by their original number long after
> the §6 entry itself is gone, and renumbering would silently
> invalidate those references.

### 6.29 Per-statement address → line table encoding (`GAP`)

[DPT.Rsm.Scanner.pas `HandleSourceFileIntroRecord`](DPT.Rsm.Scanner.pas)
decodes the `$70` source-file records (incl. the proc → declaring-unit
anchor, §4.18) but **not** the per-statement address → line table that
Embarcadero's RSM Debug File docs say the container carries (the
equivalent of the `.map`'s `Line numbers for <Unit>` arrays). What is
known: it is **not** a run of proc-entry address tokens — encoding
`TargetProcedure`'s `.map` line RVAs (`$DA00A`, `$DA032`, …) in the
`(VA shl 4) or $07` wire form and scanning all of DebugTarget.rsm
(Win32 + Win64) finds only the proc-entry RVA `$DA004` (the `$28`
record's own address payload), so the table is delta/RLE-encoded in a
dedicated section, not inline per-proc and not absolute-addressed.
Structural context already mapped (see §4.18): the file is a TDS-style
container — 32-byte header (`CSH7`, dir offset `$420`, header size
`$20`, image base, link timestamp), a source-search-path table near
`$420` (typed `{u16 type, u32 size, payload}` records, types 1–4), the
`$35` module-dependency tree, the `$70` source-file records, and a
trailing data-segment `(VA, size)` table (`$4DAF84/$55F4`, … in
DebugTarget.Win32 — these are `.data`/`.bss` ranges, not the line
table). **Next investigator's lead:** the line table most likely hangs
off the `$70`/`$35` per-module records as a parallel
`offsets[]`/`lines[]` pair (classic TDS `sstSrcModule` shape) — look
for a run keyed on the unit's segment with line numbers as small words
and address advances as deltas, in the region between a module's `$35`
block and its first `$28`. Decoding it would let a consumer do
address → line without the `.map`. The proc → **declaring-unit** edge
(§4.18) does NOT depend on this and is already delivered. Pinned (the
narrow known fact) by
[`Test.DPT.Rsm.Scanner.TestRsmProcEntryRvaNotInLineTableWireForm32`](../Test/Test.DPT.Rsm.Scanner.pas).

---

## 7. Loader contract (caller perspective)

`TRsmReader.LoadFromFile(AExePath)`:

1. Derives `RsmPath := ChangeFileExt(AExePath, '.rsm')`.
2. Memory-maps the file (`mORMot TMemoryMap.Map(..., aForceMap=True)`).
   The mapping is **kept alive** for the lifetime of the reader because
   the Format-A linker and the discoverer walk the same bytes after
   the scanner returns.
3. Verifies the first 4 bytes are `CSH7`; bails silently otherwise.
4. Runs `ScanSymbolStream` → `RecomputeProcSizes` →
   `TRsmStructDiscoverer.Run`.
5. Runs the post-process passes in `RunPostProcess`, in order:
   `FilterPhantomEnumDefs` (drops synthesized phantom `EnumDef`s — those
   named after a VMT `skClass`, or fully covered by a single authoritative
   `$03` enum — §6.25 R1. Runs **first** because the enum bridges below
   store indices into `EnumDefs`, so deleting entries afterwards would
   invalidate them — `FClasses` and the `$03`-sourced defs are already
   populated by step 4) →
   `TRsmFormatALinker.Run` →
   `TRsmClassParentDeriver.Run` →
   `TRsmCrossUnitParentResolver.Run` →
   `TRsmScopeLocalEnumBridge.Run` →
   `TRsmFieldAliasEnumBridge.Run` →
   `TRsmPropertyLinker.Run`.
6. Reports each phase via `OnPhase: TProc<String>` when assigned.

Failure modes are **silent** by design:

* Missing `.rsm` sidecar → empty reader, no exception.
* Wrong magic → empty reader.
* Buffer too small (< 8 bytes) → empty reader.

The collections always come back in a usable state (initialised
mORMot `IList<T>` / `IKeyValue<K,V>`), so callers can treat `Count = 0`
uniformly without nil checks.

---

## 8. Quick reference — collections produced

| Field on `TRsmReader` / `TRsmScanner` | Source pass                       | Key                                     | Value                          |
|--------------------------------------|-----------------------------------|-----------------------------------------|--------------------------------|
| `Procs: IList<TRsmProc>`             | `HandleProcRecord`                | (index)                                 | `TRsmProc { Name, SegmentOffset, Size, Locals }` |
| `Classes: IList<TRsmClassInfo>`      | `TRsmStructDiscoverer.Run`        | (index)                                 | `TRsmClassInfo { Name, TypeIdx, Kind, Members, ParentName, ParentRawId }` |
| `ProcByName`                         | `HandleProcRecord`                | lower(name) + lower(stripped `@`)       | `Procs` index                  |
| `ClassByName`                        | `TRsmStructDiscoverer.Run`        | lower(name)                             | `Classes` index                |
| `GlobalByName`                       | `$20` outside proc + `$27`        | lower(name)                             | 2-byte RSM type id             |
| `GlobalFileOffset`                   | same as above                     | lower(name)                             | byte offset of the `$20`/`$27` record |
| `GlobalVa`                           | `$20`/`$27` VA-slot decoder       | lower(name)                             | absolute VA (Win32) or RVA-from-image-base (Win64) |
| `EnumConstNames`                     | `$25` handlers + `$2A` flush      | `"<typeId>:<ordinal>"`                  | enum-constant identifier name  |
| `EnumTypeIds`                        | `$25` handlers + `$2A` flush      | 2-byte type id                          | True                           |
| `CrossUnitEnumIds`                   | cross-unit `$25` forms            | 2-byte secondary id                     | True                           |
| `EnumAliasesByPrimary`               | `$2A` bridge                      | primary 2-byte id                       | `IList<UInt32>` of secondaries |
| `EnumDefs`                           | `$03` + `$2A` synthesis           | (index)                                 | `TRsmEnumDef { TypeName, UnitName, Elements }` |
| `FRsmTypeIdToClassIdx` (reader)      | `TRsmFormatALinker.ScanTypeRegistry` | 2-byte primary id                       | `Classes` index                |
| `FTypeIdByName` (reader)             | same                              | lower(type name)                        | 2-byte primary id              |
| `FScopeLocalTypeIdToEnumDef` (reader)| `TRsmScopeLocalEnumBridge.Run`    | scope-local 2-byte id (`$1E` hi byte)   | `EnumDefs` index               |
| `UnitUseSegments`                    | `HandleUnitUseIntroRecord` (§4.17)| (index)                                 | `TRsmUnitUseSegment { UnitName, StartOffset, SourceFileIdx, Refs }` |
| `SourceFiles`                        | `HandleSourceFileIntroRecord` (§4.17)| (index, deduped by unit name)        | `TRsmSourceFile { SourceFile, UnitName, StartOffset, Rva }` — the importing unit a segment's `SourceFileIdx` points at |

---

## 9. File / unit map

| Concern                            | Unit                                    |
|------------------------------------|-----------------------------------------|
| Tag constants, records, model      | [DPT.Rsm.Model.pas](DPT.Rsm.Model.pas)  |
| Byte-level helpers                 | [DPT.Rsm.BufferIO.pas](DPT.Rsm.BufferIO.pas) |
| Symbol-stream walker (single pass) | [DPT.Rsm.Scanner.pas](DPT.Rsm.Scanner.pas) |
| Enum state machine                 | [DPT.Rsm.EnumDecoder.pas](DPT.Rsm.EnumDecoder.pas) |
| Class / record discovery (Format-B)| [DPT.Rsm.StructDiscoverer.pas](DPT.Rsm.StructDiscoverer.pas) |
| `$2A` + `$2C` linker (Format-A)    | [DPT.Rsm.FormatALinker.pas](DPT.Rsm.FormatALinker.pas) |
| Same-unit inheritance (layout)     | [DPT.Rsm.ClassParentDeriver.pas](DPT.Rsm.ClassParentDeriver.pas) |
| Cross-unit inheritance (type-id)   | [DPT.Rsm.CrossUnitParentResolver.pas](DPT.Rsm.CrossUnitParentResolver.pas) |
| Scope-local enum bridge            | [DPT.Rsm.ScopeLocalEnumBridge.pas](DPT.Rsm.ScopeLocalEnumBridge.pas) |
| Public facade                      | [DPT.Rsm.Reader.pas](DPT.Rsm.Reader.pas) |
| Scanner-only contract tests        | [Test.DPT.Rsm.Scanner.pas](../Test/Test.DPT.Rsm.Scanner.pas) |
| Reader facade contract tests       | [Test.DPT.Rsm.Reader.pas](../Test/Test.DPT.Rsm.Reader.pas) |
| Wider behavioural tests + perf     | [Test.DPT.Rsm.LocalsReader.pas](../Test/Test.DPT.Rsm.LocalsReader.pas) |
| Model smoke tests (tag constants)  | [Test.DPT.Rsm.Model.pas](../Test/Test.DPT.Rsm.Model.pas) |
