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

* [`TRsmScanner.HandleLocalRecord`](DPT.Rsm.Scanner.pas) BPRel-offset decoder
* [`TRsmScanner.HandleEnumConstantRecord`](DPT.Rsm.Scanner.pas) sparse-ordinal form

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
| `$66`  | `UNIT_USE_TYPE`     | Imported type reference inside a `$64` segment (`$66 NL TypeName <token: u32 LE>` — an opaque linker token, not an RVA; see §4.17)             |
| `$67`  | `UNIT_USE_SYMBOL`   | Imported symbol reference inside a `$64` segment (`$67 NL SymbolName <token: u32 LE>` — the §4.6.2 linker-token family; see §4.17)         |
| `$70`  | `UNIT_USE_FILE`     | Source-file record (`$70 NL FileName <token: u32 LE>`); the **introducer** of a unit's uses block (`$64` for an imported `.pas`/`.inc`, `$65` for the program's full-path `.dpr`/`.dpk`), decoded into the `SourceFiles` table and used as the proc → declaring-unit anchor — see §4.17 / §4.18 |

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
[Test.DPT.Rsm.Taifun.TRsmTfwTests.TestTfwWin64ProcAddressDecodesAboveCap](../Test/Test.DPT.Rsm.Taifun.pas):
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
>
> **This covers value-typed register params too, not just `Self`.** A
> `const String` param (a pointer-sized reference in EDX/ECX) reads back
> correctly at a post-clobber PC because `GetLocals` sources its bytes from
> the spill home and `evaluate … (string)` dereferences that pointer (it
> uses the bytes, not a stack address — so register params need no
> `GetLocalAddress`). Pinned by
> `Test.DPT.MCP.Server.TestMcpEvaluateConstStringRegParamAfterClobber`
> (`DebugTarget.dpr` `TConstStrParamHost.Probe(const AFirst, ASecond:
> String)`: a `Writeln` clobbers EDX/ECX, the BP line names neither param,
> both are used afterwards ⇒ both live in spill homes ⇒
> `evaluate AFirst (string)` = "Hallo", `evaluate ASecond` = "Welt").
>
> **Hard limit — this only works when the compiler EMITS a home.** The
> recovery is reading a spill slot the prologue actually wrote. A home is
> emitted in debug builds (`{$O-}`, as `DebugTarget` is) and whenever a
> param must survive a call. But under `{$O+}` the optimiser may keep a
> once-used register param in its register and then reuse the register with
> **no home at all** — at a later PC the original value exists **nowhere**
> (not the register, not memory). No debugger, the Delphi IDE included, can
> recover it; the IDE reports "inaccessible value". This is the observed
> difference on the OPTIMIZED `Test.Lib` build, where
> `Test(const AWords,Input: String; …)`'s params read as stale-register
> garbage a few lines into the method (`get_locals` shows `bp_offset = 0`,
> no home), versus the debug `DebugTarget` fixture above where they recover
> cleanly (`bp_offset = -8 / -12`). It is a property of optimised native
> code, **not** a `.rsm` decode gap — there is nothing to decode when the
> value was never spilled. The actionable guidance is therefore: to inspect
> register params throughout a method, debug a `{$O-}` build (or break at
> the method entry on an optimised one).

> **Consumer note — frameless (ESP-addressed) procedures (§6.35
> closure).** The above assumes the classic x86 frame
> `push ebp; mov ebp,esp; sub esp,N`, where the TD32 frame-relative
> offsets are negative displacements off **EBP**. The optimiser
> (`{$STACKFRAMES OFF}`, the default once `{$O+}` lets the frame be
> elided) emits a **frameless** form instead: a run of callee-saved
> register pushes — which may include `push ebp` saving it merely as a
> **scratch** register — followed directly by `sub esp,N`, with **no
> `mov ebp,esp`**. The locals then live at `[esp+N]` (positive offsets
> from the frame bottom), and EBP holds whatever the body last computed.
> Athens compiles DUnitX's RTTI-`Invoke`-dispatched test methods this way
> (observed live on Test.Lib's `TestTVariantDbValue.Implicit`: prologue
> `53 81C4… 8BDA…`, an `Int64` local read back as a stack-address
> garbage value off the caller's EBP). The reader records the offsets
> **correctly** (ESP-relative); the defect was purely that the consumer
> applied them to EBP. [DPT.Debugger.pas ProcUsesEbpFrame](DPT.Debugger.pas)
> classifies the prologue (skip leading `push` opcodes `$50..$57`; a
> following `mov ebp,esp` ⇒ EBP frame, a bare `sub esp`/`add esp,-imm` ⇒
> frameless; ambiguous ⇒ EBP, so a real frame is never mis-rebased), and
> `GetLocals` / `GetLocalAddress` read frameless locals off **ESP** (the
> frame bottom = `Regs.Esp` at a statement-boundary PC) instead of EBP.
> Pinned by `Test.DPT.MCP.Server.TestMcpEvaluateFramelessProcLocalsDecoded`
> (the `DebugTarget.dpr` `TFramelessHost.Probe` fixture, built
> `{$STACKFRAMES OFF}{$O+}` with six live `Int64` locals so the overflow
> spills to `[esp+N]`).
>
> **Register parameters in such a proc are spilled into a callee-saved
> register, not a frame slot** (`mov esi,eax` = Self → ESI, `mov ebx,edx`
> = 2nd param → EBX), so the live inbound register is stale once the body
> reuses it. [DPT.Debugger.pas TryFindRegParamRegSpill](DPT.Debugger.pas)
> scans the prologue's register-move run (after the pushes + `sub esp`)
> for the spill of the param's inbound register into EBX/ESI/EDI and reads
> that register at the BP — tried in `GetLocals` after the memory-spill
> path and only for frameless procs, before the live-register fallback.
> Pinned by `Test.DPT.MCP.Server.TestMcpEvaluateFramelessRegParamFromCalleeSavedSpill`
> (`Self` → ESI, EAX clobbered before the BP).
>
> **Sub-4-byte register params are spilled to memory with a NARROW
> store** even in a normal EBP frame — `mov [ebp-NN],dl` = `88 55 NN`
> (byte), `66 89 55 NN` (word) — which share the dword store's ModRM byte
> and so differ from `89 55 NN` only in the opcode. `TryFindRegParamSpillDisp`
> now matches the `88` / `66 89` forms too and returns the **store width**;
> `GetLocals` reads exactly that many bytes and zero-extends, so a
> `Byte`/`Word`/`Boolean`/`enum` param reads back clean (e.g. `199 =
> 0x000000C7`) instead of leaking the inbound register's stale high bytes
> (`0x00B9FDC7`). Pinned by
> `Test.DPT.MCP.Server.TestMcpEvaluateByteParamFromMemorySpill`.
>
> **A LOCAL the optimiser keeps wholly in a register** (no stack home at
> all) is emitted with a distinct `LOCAL_TAG` (`$20`) form: the payload
> begins `16 00 00 <type> <2*regindex>` where a frame local has
> `66 00 00 <type> <2*offset>` — the `$16`/`$66` byte at +0 is the
> residency discriminator (independent of the `<type>` byte at +3:
> `LExtra` and `LScratch` are both Integer `$02` yet differ). `byte+4` is
> the register: `2*index` over the allocatable GP set
> `[EAX,ECX,EDX,EBX,ESI,EDI]` (verified EBX=`$06`, ESI=`$08`, EDI=`$0A`).
> The scanner ([DPT.Rsm.Scanner.pas HandleLocalRecord](DPT.Rsm.Scanner.pas))
> decodes the `$16` form to `Kind = lkRegisterResident` + `CpuRegIndex`;
> `GetLocals` reads the live register (x86 only — the map was RE'd on x86;
> x64 leaves the value as "unknown" rather than read a wrong register).
> Before this, the `$16` byte+4 (`$0A`) was misread as `2*offset` →
> bogus `BpOffset=5` and an `[ebp+5]` garbage read. Pinned by
> `Test.DPT.Rsm.Scanner.TestRegisterResidentLocalUsesDistinctForm32` (the
> `$16`-vs-`$66` discriminator) and
> `Test.DPT.MCP.Server.TestMcpEvaluateRegisterResidentLocal` (RR1→EBX,
> RR2→ESI, RR3→EDI read back their sentinels).

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
>
> **§6.32 closure — the VMT-priority override now covers BP-relative and
> global object locals, not just register `Self`/params.** The same
> alias hazard applies to an ordinary object *local*: an inline-declared
> `var V := CJwksValidator.Create(...)` carries a per-proc reference id
> in its `$20` record (§4.4 inline-var form), and that id can collide
> with an unrelated record in the registry — so the record-hop priming
> flipped `ContextIsRecord := True` and every `V.<field>` evaluate failed
> even though `CJwksValidator` and its fields were correctly discovered
> (verified: the standalone reader resolves `CJwksValidator.FExpectedTenantId`
> at offset 16 on the 172 MB `Test.Lib.rsm`). The priming-skip probe now,
> for a non-register first hop, **dereferences the slot** (`Addr`) to get
> the candidate instance pointer and VMT-walks *that*; a record-typed
> slot derefs to inline data whose VMT walk fails, so records still prime
> correctly. With the skip in place the class-hop branch resolves
> `V → CJwksValidator → FindClassMember` and
> `evaluate V.FExpectedTenantId` (`type=string`) returns `tenant-1`
> live. The small-binary clean regime (§6.27) cannot reproduce the
> alias collision, so this is verified against the live large-binary MCP
> session rather than a DebugTarget unit pin. (Bare `evaluate` without an
> explicit `type=` now *auto*-types these String fields too: §6.33's
> three defects — the `$2C` guard, the body=9 offset-baked id, and the
> unit-local-colliding parent-id attribution — are all closed; see §4.9.)

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
>   [Test.DPT.Rsm.Taifun.TestTfwPerProcRefIsNotUnitUseTableIndex](../Test/Test.DPT.Rsm.Taifun.pas).
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
> [TestTfwSelfTypeIdxIsPerProcRefNotRegistryId](../Test/Test.DPT.Rsm.Taifun.pas).
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

Decoded by [`HandleLocalRecord`](DPT.Rsm.Scanner.pas). The
payload starts at `P + 2 + NL`. Three shapes:

**Shape A — 2-byte type id with BPRel offset:**

```
... <typeId-lo at +3> <Hi at +4 in {$2E, $2F, $1E}> <ofs0 at +5> [<ofs1 at +6>]
```

If the LSB of `byte5` is 0, BPRel offset is `ShortInt(byte5) div 2`
(single-byte form). If the LSB is 1, the offset is
`(SmallInt(byte5 or (byte6 shl 8)) - 1) div 4` (wide form).

Type id is `byte3 | (byte4 shl 8)`.

The Hi byte at `+4` is the 2-byte-id marker (matching the §4.4 width
rule: `+4 ∈ {$2E, $2F, $1E}` ⇒ 2 bytes). `$2E`/`$2F` are the
same-compilation 2-byte type refs; **`$1E` is the per-(unit,type)
CROSS-UNIT alias** — both for records imported from another unit
(§4.2/§4.15) and for scope-local enums (§4.4 hi-byte note). `$1E` was
admitted to this gate to fix a pointer-to-cross-unit-record local
(`CxPtr: PComplexRec`, payload `66 00 00 B1 1E F8 63`): its alias Hi
byte `$1E` was otherwise read by Shape B as the low byte of a wide
offset (`$F81E` → BPRel **−504**, an out-of-frame slot below ESP)
because the `SCOPE_END` (`$63`) terminator at `+6` made the malformed
wide shape validate. Under Shape A the offset is read at `+5` (`$F8` →
`ShortInt(−8) div 2` = **−4**, the pointer slot just below EBP). `$1E`
can never be a 1-byte-type local's offset-low byte: a single offset is
negative (byte `$80..$FE`) and a wide offset's low byte is odd, but
`$1E` = 30 is even and positive. Pinned by
[`Test.DPT.Rsm.Reader.TestComplexRecLayoutAndPointerLocalOffset32`](../Test/Test.DPT.Rsm.Reader.pas).

**Shape B — primitive-type id (single byte) with BPRel offset:**

The decoder looks at what follows the candidate offset byte:

* If `byte5 ∈ {LOCAL_TAG, PROC_TAG, SCOPE_END}` → byte 4 is the offset
  (single-byte form, `ShortInt(byte4) div 2`), type id is `byte3`.
* Otherwise if `byte6 ∈ {LOCAL_TAG, PROC_TAG, SCOPE_END}` **AND `byte4`
  is odd** → bytes 4..5 form the wide offset, type id is `byte3`.

**The wide path requires an ODD `byte4` (§6.40 closure).** A genuine
wide offset's low byte carries the LSB-continuation marker, so `byte4`
is always odd; `(SmallInt(byte4 | byte5 shl 8) − 1) div 4` only
reconstructs the value when that bit is set. An **even** `byte4` here is
not a wide offset at all — it is the Hi byte of a 2-byte cross-unit type
alias whose marker is **none of** `$2E`/`$2F`/`$1E`, with the real
single-byte offset sitting one byte later at `+5`. TFW's
`CKonsMisDict.DataLoad` has `KonsMis: PKonsMis` (pointer to a cross-unit
record) with payload `66 00 00 E5 02 E8 <20>`: alias `$02E5` at `+3..+4`
(Hi byte `$02`), single-byte offset `$E8` at `+5` =
`ShortInt(−24) div 2` = **−12** (`[ebp-0x0C]`, verified live under the
MCP debugger). Before the odd-`byte4` gate, the wide path swallowed it —
reading `byte4=$02`,`byte5=$E8` as `$E802` → `(SmallInt($E802)−1) div 4`
= **−1535**, an out-of-frame slot that made `evaluate KonsMis.Name`
dereference a nil pointer. With the gate, the even `byte4` is rejected
and the record falls through to **Shape C** below, which reads the
offset at `+5`. Pinned by
[`Test.DPT.Rsm.Scanner.TestCrossUnitPtrLocalNonMarkerHiByteOffset`](../Test/Test.DPT.Rsm.Scanner.pas)
(synthetic CSH7 buffer; leakage guards: a genuine wide local with an odd
low byte still decodes, and a single-byte-offset local is unaffected).

The cross-unit record/enum alias's Hi byte is `$1E`, so it now decodes
through Shape A above. `DebugTarget.RecordLocalNestedProbe`'s
`AdrLoc: DebugTarget.RecTypes.TXAdresse` (a 153-byte cross-unit record)
is the canonical example: payload `66 00 00 A5 1E 9D FD 63` → alias
`$1EA5` at `+3..+4`, wide offset `$FD9D` at `+5..+6` =
`(SmallInt($FD9D) − 1) div 4` = **−153** (= −$99, the record at the
frame bottom), `SCOPE_END` (`$63`) at `+7`. Pinned by
[`Test.DPT.Rsm.Reader.TestCrossUnitRecordLocalOffsetAndLayout32`](../Test/Test.DPT.Rsm.Reader.pas).

**Shape C — 2-byte alias fallback with BPRel offset at +5 (§6.36
closure):**

```
$66 $00 $00  <aliasLo at +3>  <aliasHi at +4, NOT $2E/$2F/$1E>  <ofs at +5> [<+6>]  <terminator>
```

A safety net for any future cross-unit alias whose Hi byte is **not**
the documented `$2E`/`$2F`/`$1E` (so Shape A's gate misses and Shape B
would read the alias-Hi byte at `+4` as the offset): the real BPRel
offset sits **one byte later, at `+5`** (single or wide sub-form). It is
tried **only** when Shapes A/B left the synthesized fallback, and
accepted only when the decoded offset is **non-positive** (locals live
below EBP) AND `IsLocalOffsetTerminatorAt` confirms the post-offset byte
opens a real record (`$20`/`$28`/`$63`/`$22`/`$21`/`$25`/`$03`/`$2A`, or
`$9C $17`). Type id is `aliasLo | (aliasHi shl 8)`. The `+5` offset
itself comes in **both** sub-forms: a single even byte
(`ShortInt(ofs) div 2`) or the wide odd-low-byte pair
(`(SmallInt(W) − 1) div 4`).

This fallback fires for two distinct §6.36/§6.40 shapes that Shape A's
`$2E`/`$2F`/`$1E` gate doesn't cover:

* **wide offset** — `DebugTarget`'s `AdrLoc: TXAdresse`,
  `66 00 00 A5 1E 9D FD 63`: alias `$1EA5`, **wide** offset `$FD9D` at
  `+5..+6` = −153 (§6.36). (Now also reachable via Shape A since `$1E`
  joined its gate, but the fallback still backstops any non-`$1E` wide
  alias.)
* **single-byte offset** — TFW's `KonsMis: PKonsMis`,
  `66 00 00 E5 02 E8 <20>`: alias `$02E5` (Hi byte `$02`, not a marker),
  **single** offset `$E8` at `+5` = −12 (§6.40). This is the case the
  odd-`byte4` Shape-B gate above hands down: without that gate Shape B
  consumed the record with a garbage wide offset (−1535) and this
  fallback never ran.

When none of A/B/C recognises, `Loc.BpOffset` keeps its synthesized
fallback `-10000 - (FScanLocalIdx * 4)`. The
`TestEdgeCaseLocalsAllDecoded` and `TestLocalsHaveDistinctOffsets`
tests assert that **no synthesized fallback survives** on
`DebugTarget.LocalsProcedure` / `EdgeCaseLocalsProcedure` — i.e. all
encodings exercised there must round-trip.

**UNCERTAIN / GAP**: BPRel encodings outside the cases above (some less
common Delphi types) may still hit the fallback on unfamiliar binaries.
`TestEdgeCaseLocalsAllDecoded` is the canary.

#### Inline-declared var form (`var X := expr;` in a proc body) — §6.30 closure

Delphi inline variable declarations emit a **different `$20` body anchor**
than a classic `var` block. Where the classic stack-local opens with
`$66 $00 $00`, the inline form opens with the 4-byte anchor
`$66 $00 $01 $04`, which shifts the type ref and BPRel offset one byte to
the right:

```
$20  <NL>  <Name>   $66 $00 $01 $04   <type ref>   <offset>   <terminator>
                    └── anchor ──┘   1 or 2 bytes  1 or 2 by.
```

* **type ref** — 1 byte, OR 2 bytes when the byte at anchor+5 is a
  `$2E`/`$2F` structured-type hi marker. Per §4.2 this is a per-proc
  reference id, not a registry primary; it is **not** reliably resolvable
  to a type name on large binaries (and the consumer doesn't need it —
  the dotted walk uses the VMT).
* **offset** — the BPRel offset, in the usual two sub-forms: a single
  signed byte `ShortInt(b) div 2` when even, or the wide
  LSB-continuation pair `(SmallInt(W) - 1) div 4` when the low byte is
  odd.
* **terminator** — either the next record's tag byte
  (`$20`/`$28`/`$63`/…) or the `$9C $17` trailing-init metadata
  signature that literal-initialised inline vars carry.

Because the type-width is not flagged, the decoder
([Scanner.pas `HandleLocalRecord`](DPT.Rsm.Scanner.pas)) probes
`TypeW ∈ {1,2}` and accepts the first whose offset is a plausible
**non-positive** BPRel value followed by a valid terminator — this
rejects the type-hi byte (positive / non-terminated) being misread as the
offset. The anchor `$66 $00 $01 $04` is read as one LE DWORD
`$04010066` on the hot path. All four (type-width × offset-width) combos
are pinned by
`Test.DPT.Rsm.LocalsReader.TestInlineVarLocalsDecoded32/64`
(single/2-byte type, single/wide offset) and
`TestInlineVarWideOffsetMonotonic32/64` (24 inline ints stepping `-4`
across the single→wide boundary).

Concrete Win32 evidence (`DebugTarget.InlineVarLocalsProcedure`):

| local | bytes after anchor | type | offset |
|---|---|---|---|
| `LocalIVStr` (string) | `06 F8 9C 17…` | 1-byte | `ShortInt($F8)/2` = −4 |
| `LocalIVObj` (TStringList) | `69 06 F0 20` | 2-byte | `ShortInt($F0)/2` = −8 |
| `LocalIVRec` (record) | `3D 2E E0 20` | 2-byte (`$2E` hi) | `ShortInt($E0)/2` = −16 |
| `LocalIVBig` (512-byte array) | `09 2F A1 F7 9C 17…` | 2-byte (`$2F` hi) | `(SmallInt($F7A1)−1)/4` = −536 |

> **Before this was decoded** (§6.30), every inline-var local mis-decoded:
> the short forms misfired into Shape-B's wide path (`(W−1)/4` reading the
> type byte as part of the offset → ~128× too large), and the
> literal-init long form fell to the synthesized fallback. On
> `Test.Lib.CTestJwksValidator.Validate_Success` this made
> `V` (a `var V := CJwksValidator.Create(...)`) decode to BpOffset −4587
> instead of −36, so `evaluate V.FExpectedTenantId` read a garbage object
> pointer. The fix restores `V` to −36 and the four expected fields
> resolve (verified live).

Also: every `$20` record additionally publishes the `(name → 2-byte id)`
pair into the global maps `FGlobalByName` / `FGlobalFileOffset`
([`HandleLocalRecord`](DPT.Rsm.Scanner.pas)), because the
`FScanInProc` gate cannot reliably distinguish a stack local from a
module-level variable in every code path.

#### Module-global form (when `FScanInProc` is False)

```
$20  <NL: u8>  <Name>  $66 $00 $00  <id-lo>  <id-hi>  <VA: 4 bytes>
```

Decoded by [`HandleModuleGlobalLocalTagRecord`](DPT.Rsm.Scanner.pas).
`NL` ∈ `[1, 40]`.
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
[`HandleModuleGlobalLocalTagRecord`](DPT.Rsm.Scanner.pas).

### 4.5 `$27` GLOBAL_PRIM_TAG — top-level primitive global

```
$27  <NL: u8>  <Name>  $66 $00 $00  <id-lo>  [<id-hi>]  <VA: 4 bytes>
```

Decoded by [`HandleGlobalPrimRecord`](DPT.Rsm.Scanner.pas).
`NL` ∈ `[1, 40]`.
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
comment in [`HandleGlobalPrimRecord`](DPT.Rsm.Scanner.pas) explains
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
[Scanner.pas:1548-1550](DPT.Rsm.Scanner.pas#L1548-L1550)). The single-byte
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

**Interfaces register here too — and the `$2A` registry is their ONLY
discovery anchor (§6.36-A static route).** An interface has no VMT
class-trailer (§4.11) and no `$0E` record sentinel (§4.12), so
`TRsmStructDiscoverer` never finds it — yet its name + 2-byte id sit in
the `$2A` registry exactly like a class's (e.g. DebugTarget's user
interface `IDbgRecoverable`: body `2A 0F 'IDbgRecoverable' 20 00 00
<idLo> <idHi>`, id `$4712` Win32 / `$0312` Win64 — per-build,
platform-divergent). Because the body-flag is shape, not kind (above),
the only available interface signal at the `$2A` level is the
`I<Upper>` name convention (the same basis §6.25's phantom-enum drop
uses). `ScanTypeRegistry` therefore admits `I<Upper>` names alongside
`T`/`P`, and **synthesizes a `skInterface` `TRsmClassInfo`** for each
such name that no discovered class already claims — appended to
`FClasses` after the registry walk (so every pre-existing index and the
skRecord-only offset/owner indices stay valid), registered in
`FClassByName`, and mapped into `FRsmTypeIdToClassIdx` under its id
(only when that id is free). The synthesized entry carries a **non-nil
empty** `Members` list (`TRsmClassParentDeriver` / `TRsmFieldAliasEnumBridge`
read `Members.Count` unconditionally) and zero parent id, so every
`Kind = skClass` / `Kind = skRecord` consumer branch skips it — an
interface is neither formatted as `'object'` (§4.16 `ClassLookup`) nor
record-hopped into (the §6.36 mis-typing hazard cannot reach an
`skInterface`). This is a **static**-route win — it surfaces interfaces
by name + id to the no-process viewer (RsmDesk) and lets a class
*member* typed as an interface resolve its id to the interface name. It
does **not** fix the live `Lst: ILazyUniqueList<TObject>` case, whose
one-byte stack-local type code is lossy (§6.36 mechanism) and reaches
no interface id at all. Pinned by
[Test.DPT.Rsm.Reader.TestInterfaceDiscoveredAsStruct32/64](../Test/Test.DPT.Rsm.Reader.pas)
(name → `skInterface`, non-zero id round-trips via
`FindClassIdxByRsmTypeId`, non-nil empty members; leakage guards:
`TMixedRec` stays `skRecord`, `TDerived` stays `skClass`).

**Interface METHODS are not surfaceable (recon, not pursued).** The
synthesized `skInterface` carries an empty `Members` list by design:
interface methods are abstract slots with no code, so the linker emits
**no `$28` proc** for `IDbgRecoverable.Marker` — only for the concrete
`TDbgRecoverableImpl.Marker` (verified: `IDbgRecoverable.Marker` is absent
from `DebugTarget.rsm`; only the impl-class method exists). Attaching
methods to the interface would require reverse-engineering the
**implements** relationship (`IDbgRecoverable → TDbgRecoverableImpl`) from
the unexplained bytes between the two `$2A` entries
(`… 12 47 | f9 da 04 36 00 20 00 00 c7 fd da 04 b8 02 26 14 2e | "TDbgRecoverableImpl"`)
and then back-mapping the impl's methods — a likely confident-negative,
deliberately not pursued. The static route delivers interface *name + id*,
not its method list.

**Owning-unit resolution** (only when same-comp $25 constants are
pending). The synthesized `EnumDef` needs the owning unit name. Two
decoupled steps (`HandleTypeRegistryRecord`):

* **Synthesis gate (tight, 1 KB):** synthesize an `EnumDef` only when a
  dotted proc sits within 1 KB of the `$2A`. This keeps the synthesis
  set tight — widening the gate floods the `EnumDef` list with non-enum
  `$2A` entries whose nearest proc is far (a 1 KB→64 KB widening alone
  took DebugTarget 132→166 synthesized defs). This gate decides *which*
  `$2A` synthesize; it is decoupled from the unit name below.
* **Unit name — the §4.18 `$70` introducer (§6.25 R2 closure):** for a
  gated entry, the owning unit is `SourceFiles[FCurrentSourceFileIdx]`
  — the `$70` source-file introducer the scan is currently inside, the
  SAME trusted declaring-unit anchor `HandleProcRecord` stamps onto
  procs (each unit's `$2A` block is preceded by its own `$70`
  introducer, §4.18). This is a structural O(1) lookup, not a search.
  It **replaced** a former 1 MB forward "name search" that hunted for
  the unit-init proc's dotted namespace and used a `T`/`E`/`I`+Upper
  naming-convention filter to reject method names — a crutch that
  overshot into unrelated namespaces, missed `C`-prefix method names,
  and fell back to a method when the init proc sat >1 MB out (the R2
  defect). The `$70` anchor fixes all three and retires the crutch. The
  rare early-RTL `$2A` with no preceding `$70`
  (`FCurrentSourceFileIdx < 0`) falls back to the nearby gate proc so
  the enum is **never dropped**; all such entries are phantom-filtered
  downstream (§7), so the fallback's quality is immaterial. (An earlier
  attempt that *dropped* the def when no clean unit name was found
  collapsed DPT.rsm 95→22 — never drop; §6.25.)

Pinned by
[Test.DPT.Rsm.Scanner.TestEnumDefsNotOverCollected32](../Test/Test.DPT.Rsm.Scanner.pas)
(no synth `EnumDef.UnitName` is a `TClass.Method`; DebugTarget's
sibling-unit `TStatus` resolves to its `DebugTarget.*` unit) and
[Test.DPT.Rsm.Taifun.TestTfwSynthEnumPhantomResidual](../Test/Test.DPT.Rsm.Taifun.pas)
step (5) (TFW: no surviving synth EnumDef carries a method-name unit).

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
4. Synthesise an `EnumDef` from the pending buffer when an owning unit
   name was resolved (the §4.18 `$70` introducer above) — **but only if
   the buffered constants carry unique ordinals**. A genuine Delphi enum cannot have two
   elements sharing an ordinal, so a buffer with duplicate ordinals is
   not one enum but an over-collection of unrelated named-`const`
   families (e.g. `Winapi.SHFolder`'s `CSIDL_*` and `SHGFP_*` both start
   at ordinal 0; `Vcl.Controls`/`System.UITypes`' `cr*`/`mr*`/`vk*` runs
   merged under a `LongWord` alias `$2A` like `TColorRef`). Such a buffer
   is still flushed into `FEnumConstNames` (job 3) for `(typeId, ord)`
   lookup but produces **no** `EnumDef`. This duplicate-ordinal guard is
   the sole over-collection fence (the class-method unit-name rejection
   that once accompanied it was reverted — see the owning-unit note
   above and §6.25). Pinned by
   [Test.DPT.Rsm.Scanner.TestEnumDefsNotOverCollected32](../Test/Test.DPT.Rsm.Scanner.pas).

### 4.9 `$2C` Format-A field record (no separate `TRsmTag` constant)

The Format-A field records are the **second source** of class / record
member information (Format-B is the structural discoverer in
`TRsmStructDiscoverer`). The Format-A linker reads them after the
scanner completes.

```
[FF]?  $2C  <NL: u8>  <Name>  $00 <scope> $00  <field-id-lo>  <field-id-hi>
  <variable body>
$07 $00 $00 $08  <parent-id-lo>  <parent-id-hi>
```

* The optional `$FF` prefix marks a continuation block (a non-first
  field in a block).
* `NL` ∈ `[2, 40]`. The 3 bytes at offset `NL+2`..`NL+4` (i.e.
  `After+0..+2`) are the validation anchor. **§6.33: the anchor is
  `$00 <scope> $00`, NOT the fixed `$00 $02 $00` it was long believed
  to be.** `After+0` and `After+2` are reliably `$00`; the middle byte
  `After+1` is a per-binary / per-scope discriminator that is `$02`
  only on the small single-unit DebugTarget fixture and takes other
  small values on multi-unit binaries (`$00` for DebugTarget's own
  property-backing `FBackingStr`, `$10` for Test.Lib's
  `CJwksValidator` string fields, `$14`, …). The linker validates the
  two zero bytes only; constraining the middle byte to `$02` silently
  dropped every field record outside that one shape.
* The end-of-record marker `$07 $00 $00 $08` is searched in a bounded
  window `[After+5, After+30]`.
* The 2 bytes following the terminator are the **parent record id**
  (encoding-specific, see below).

#### Parent id encoding

Two encodings are observed:

* **Wide encoding** (`parent-hi != $FF`): a 2-byte id. **Usually** a
  "real" RSM primary that appears in the `$2A` registry, which the
  linker resolves via `FindClassIdxForRawId` (the
  `FRsmTypeIdToClassIdx` map populated by `ScanTypeRegistry`).
  **§6.33 caveat: this is NOT always a global registry primary.** On
  the large Test.Lib corpus the wide parent id can be a *unit-local*
  2-byte id that collides across many unrelated types — id `$0391`
  alone is reused by ~18 distinct `$2A` names (`TKey`, `TEvent`,
  `TButtonDisplay`, …) at different file offsets, and is the parent id
  of `CJwksValidator`'s field records even though `CJwksValidator`'s
  own `$2A` registry id is `$8297`. When the wide parent id is a
  colliding unit-local id, `FindClassIdxForRawId` last-wins to the
  wrong (or no) class. The member-name-set fallback below recovers
  these.
* **Narrow encoding** (`parent-hi == $FF`): the parent id is a
  **unit-local** byte (`parent-lo`). These collide across units, so the
  linker falls back to a **block-owner index** that pairs each $2C
  block's start offset with the owning record found via the source-
  declaration order in the same unit. See
  [DPT.Rsm.FormatALinker.pas `BuildBlockOwnerIndex`](DPT.Rsm.FormatALinker.pas).
  This bridge is critical for the TFW corpus where unit-local id `$44`
  is `TUserKonsOutlook` in one unit and an unrelated class in another.
  **The block-owner index covers only narrow-encoded blocks paired
  with `$0E` record markers (records, not classes); §6.33's
  wide-encoded class blocks are not covered.**

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
terminator). The linker discriminates the body shapes in
[DPT.Rsm.FormatALinker.pas `LinkFieldsFromFormatA`](DPT.Rsm.FormatALinker.pas):

| Body shape           | Description                                                                          |
|----------------------|--------------------------------------------------------------------------------------|
| `BodyLen == 14`      | Numeric primitive (Integer, Word, Byte, Int64, UnicodeString, Single, Double, Extended). `PrimitiveTypeId` = 2 bytes at `EndOff - 5` (e.g. `$0401` UnicodeString, `$03FD` Integer). |
| `BodyLen == 15`      | Numeric primitive with extra leading byte (Boolean `$0425`, Currency `$0429`, ...). Same recovery rule. |
| `BodyLen == 9` + `$9C $01` at `After+5..+6` | Managed reference primitive. `PrimitiveTypeId` = the **single byte** at `After+3` (§6.33). |
| `BodyLen >= 10` + `$9C $01` at `After+6..+7` | Enum-typed field (compact form). See enum bridge below. |
| `BodyLen >= 11` + `$9C $01` at `After+7..+8` | Enum-typed field at parent offset ≥ 256 (extra separator). See enum bridge below. |

##### §6.33: the body=9 managed-reference single-byte id

The body=9 managed-reference form was long mis-read as a 2-byte id
`After+3 | (After+4 shl 8)`. That is **wrong**: only `After+3` is the
type id, and it lives in the compiler's **single-byte** primitive id
space (the same space the `$21` local and `$27` global records use):

| `After+3` | Managed type             |
|-----------|--------------------------|
| `$04`     | `string` / UnicodeString |
| `$0C`     | `ShortString`            |
| `$1C`     | `AnsiString`             |
| `$1E`     | `WideString`             |

`After+4` is **2 × the field's instance offset** — a positional byte,
not a type-id high byte. Proven across 7 fixtures: DebugTarget
`TPrimitives.FAnsi@0 → $00`, `FWide@4 → $08`, `FShort@8 → $10`; and
Test.Lib `CJwksValidator` strings at offsets 8/12/16/20 →
`$10/$18/$20/$28`. The old read therefore baked the field offset into
the id — the table entries `$001C`/`$081E`/`$100C` "worked" only
because DebugTarget's `FAnsi`/`FWide`/`FShort` happen to sit at
offsets 0/4/8. The same managed type at any other offset produced an
unmapped id (e.g. Test.Lib's `string` fields → `$1004`/`$1804`/
`$2004`/`$2804`), so `AutoDetectFormatterName` Path 1 missed and bare
`evaluate` failed. The linker now stores `After+3` alone and the
evaluator's primitive-formatter table keys the managed types on the
single bytes above. The body=9 shape is also recognised **before** the
`FindClassIdxForRawId(FieldId)` / pointer-alias lookups, because its
`FieldId` (`After+3..+4` = `(type-id-low, 2×offset)`) is not a registry
id and could spuriously hit a class on a large binary. Pinned by
[Test.DPT.Rsm.Reader.TestManagedStringFieldIdsOffsetIndependent32](../Test/Test.DPT.Rsm.Reader.pas)
(DebugTarget) and the shape by
[Test.DPT.Rsm.Taifun.TestExpectedTenantIdFieldRecordShapePinned](../Test/Test.DPT.Rsm.Taifun.pas)
(Test.Lib).

When the `FieldId` resolves to a known class/record (via
`FindClassIdxForRawId`), `Member.TypeIdx` is set to the discovered
class's `TypeIdx`. Otherwise `Member.PrimitiveTypeId` is populated
through one of the body-shape rules above. **Members not confirmed by
any Format-A record are pruned by `PruneSpuriousMembers`** to remove
Format-B over-collection from the backward window scan (a class with
*no* confirmed member is left untouched, not erased).

#### §6.33-C: member-name-set attribution for colliding wide blocks

When the wide parent id is a *unit-local colliding* id (above), neither
the registry path nor the block-owner index (narrow / `$0E`-record
only) can name the owning class, so the block's fields are left
unlinked. `BindUnresolvedWideBlocksByMemberNameSet`
([DPT.Rsm.FormatALinker.pas](DPT.Rsm.FormatALinker.pas)) recovers them
after `PruneSpuriousMembers`:

1. Index every discovered class/record by its **sorted** member-name
   set (sorting makes the match independent of the block's stream order
   vs the discoverer's offset order), tracking a collision count.
2. Walk the byte stream grouping `$2C` records into contiguous **blocks**
   (a block starts at a `$2C` *not* preceded by the `$FF` continuation
   marker and runs through the `$FF $2C` records that follow).
3. For a block whose sorted field-name set **uniquely** (collision
   count = 1) matches a class that is **currently fully unlinked**,
   attribute each record's primitive type to the matching member by
   name. Ambiguous (>1 class shares the set) and no-match blocks are
   skipped — the uniqueness + already-unlinked guards make the pass
   never disturb anything the registry / block-owner paths resolved.

The set is the disambiguator the colliding parent id cannot provide:
`CJwksValidator`'s
`FCache|FExpectedAudience|FExpectedIssuer|FExpectedProductId|FExpectedTenantId`
is unique among 14 405 classes. On Test.Lib this attributes ~2 769
otherwise-unlinkable classes; cost is one extra `$2C` walk + the index,
~430 ms on the 814 MB TFW (`LinkMemberTypeIdsFromFormatA` phase 2.3 →
2.8 s, budget 25 s). **Scope limit:** only the unambiguous primitive
shapes are recovered (body=9 single byte at `After+3`; body=14/15 from
the tail). The marker@+6/+7 enum/class shapes are skipped because their
`FieldId` is the same unreliable unit-local id — see the §6.34-closed
note below. Pinned by
[Test.DPT.Rsm.Taifun.TestWideBlockNameSetBindsCJwksValidatorStrings](../Test/Test.DPT.Rsm.Taifun.pas)
(CJwksValidator strings → `$04`; leakage guard: class-typed `FCache`
stays unstamped).

##### §6.34 (closed — design limit): enum/class-typed fields of colliding-parent convention classes

The §6.33-C pass recovers only the **primitive** members of a name-set-
matched class. The **marker@+6/+7 records whose `byte+5 != $0C`** —
class-typed fields (`CJwksValidator.FCache`), window handles, `Boolean`s,
plain integers — are left unstamped because their field-type `FieldId`
(`After+3..+4`, with the 2× instance offset at `+5`) is a **per-unit
local reference, not a `$2A` registry primary** — the field-side analogue
of the §6.27 / §4.2 local/param per-proc-ref design limit. Three
resolution leads were refuted on Test.Lib (diagnostics removed after the
finding):

* **§6.9 nearest-LOW-byte enum bridge** — fires only on the `byte+5 = $0C`
  same-comp form; these records are `byte+5 != $0C` and dominated by
  non-enums (`Boolean` `byte+3=$02`, `TStrings` refs `$DA`, handles `$80`),
  so the bridge neither applies nor would be safe if forced.
* **Global registry uniqueness** — of 17 923 such records in name-set-
  matched blocks, only 14 % carry a globally-unique id and **those resolve
  to garbage** unrelated types (`TVarData.VDate`'s id → `TKeyValuePair`,
  `FCache`'s `$0659` → `TEnumerator`); 35 % carry an id absent from the
  registry, 51 % a colliding one.
* **File-offset proximity** (the §6.10 block-owner idea applied to the
  field type) — for `FCache`'s records the file-nearest `$2A` with the
  matching id sits **5–79 MB away** (or is absent, 7/19), never in the
  field's own unit; and 3 204 colliding ids have a different-named `$2A`
  within 100 KB, so unit-scale proximity cannot disambiguate either.

So the field type is **not statically recoverable** from the `.rsm` for
these records. Consequence: the fields remain evaluable with an explicit
`type=`, and for **class-typed** fields the **live VMT** of the instance
is the authoritative type source at runtime (§4.2) — the static reader /
viewer can only show them un-typed. An **enum-typed** field of such a
class has no VMT to consult and so needs the explicit `type=`. This is a
design limit, not a decodable gap; the surviving §6.33-C leakage guard
(`FCache` stays unstamped) ensures these records are never *mis*-attributed.

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
[Test.DPT.Rsm.Taifun.TRsmTfwTests.TestTfwEnumTypedFieldResolvesToPrimary](../Test/Test.DPT.Rsm.Taifun.pas).

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
`Test.DPT.Rsm.Taifun.TestTfwClassInstanceFieldResolves` (TFormAd
discovered) with `TestMcpEvaluateInheritedFieldViaVmtWalk` (TComponent
not mis-anchored) green on both platforms.

**Name-convention-free candidate gate (§6.31 closure).** The byte-walk's
cheap pre-filter (`IsClassNameCandidate`) used to require the candidate's
first byte be `'T'` ("every class/record name we care about starts with
T"). That silently dropped every non-`T` class — `C`-prefixed
(`CJwksValidator`, `CTestJwksValidator`), `E`-prefixed exceptions, etc. —
so their fields never resolved in `evaluate`. The gate is now
convention-free: it admits any candidate that carries a **structural
anchor** near the name (the `$07` class-trailer marker at `NameEnd+4`/`+8`,
the method-block 3-zero header, or the `$0E` record sentinel before it),
not a particular first letter. The `'T'` gate was, however,
*load-bearing for performance* — it kept the per-byte-position printable
scan and the 16 KB forward scan rare. To stay fast convention-free, `Run`
**precomputes a sorted index of every class-trailer marker position**
(`TrailerStarts`, one O(N) pass using LE-DWORD reads of the
`04 00 00 00 07` / `08…07` prefixes), and `FindClassTrailerWithin` then
**visits only the markers inside a candidate's window** (binary-searched
via `LowerBoundTrailer`) instead of byte-scanning it; the closer-same-name
defer (§6.16) is bounded by the matched trailer. Net result:
`DiscoverAndParseAllStructs` on TFW is ~10 s — *faster* than the ~14 s
`'T'`-gated baseline — so the `Tfw` per-phase budget is **unchanged**.
Pinned by `TestNonTPrefixClassDiscovered32/64` (DebugTarget's
`CNonTPrefixHost` discovered with fields; neighbour `TInner` still
found) and `TestLargeBinaryNonTClassDiscovered`
(`CJwksValidator.FExpectedTenantId @16` in the 172 MB `Test.Lib.rsm`).
This is the first of the reader's naming-convention crutches to be
retired; the `IsPrintableAscii` charset and the §4.15 `F`/`T` field-name
bridges remain conventions for later increments.

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
length-prefixed identifier is `$0E` (the `$0E` itself is the
structural anchor the §6.31 convention-free gate keys on — no first-letter
assumption). Records have NO trailer pattern; the `$0E` sentinel alone
identifies them.

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
([Test.DPT.Rsm.Taifun.TestTfwSimpleRecordHeaderCoversTfwRecords](../Test/Test.DPT.Rsm.Taifun.pas))
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
`Test.DPT.Rsm.Taifun.TestTfwClassInstanceFieldResolves` (TFormAd.FAd
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
[DPT.Rsm.FormatALinker.pas `PruneSpuriousMembers`](DPT.Rsm.FormatALinker.pas)).

> **The field block IS preceded by an own-field COUNT — but it cannot
> tighten the `ScanWindow`.** Immediately before the first (offset-4)
> field record sits `<count:u16> 02 00 00 00 00`, where the `u16` is the
> number of the class's OWN fields. Verified across distinct counts:
> `TInner`=2, `TOuter`=3, `TClassFieldHost`=3, `TWithRec`=5,
> `TDeepDerived`=1, `TMyComp`=1 (pinned by
> [Test.DPT.Rsm.Scanner.TestClassFieldBlockOwnFieldCount32](../Test/Test.DPT.Rsm.Scanner.pas)).
> This is the "length" the 64 KB backward `ScanWindow` heuristic stands
> in for — but it is **not usable as a scan bound**, because the intro's
> `02 00 00 00 00` is byte-identical to the inter-field separator: a
> non-terminal field ends `… 02 00 <flag=02> 00`, so the seven bytes
> before the *next* field's offset-DWORD read as `02 00 02 00 00 00 00`
> — i.e. a spurious "count = 2" followed by the same `02 00 00 00 00`.
> The genuine count is therefore only distinguishable **at field 1**, and
> field 1 cannot be located by pattern alone (that is exactly what the
> backward walk is trying to find). So the count can *validate* an
> already-bounded run but cannot *bound* the scan; the `ScanWindow` +
> `AMinStartOff` + `PruneSpuriousMembers` remains the robust collector.
> (The decode is recorded as knowledge by the pin above; the negative is
> the reason this heuristic window stays.)

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
> `Test.DPT.Rsm.Taifun.TestTfwPointerAliasBindingDiagnoses` (TFW,
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
>
> **§6.36 closure — record-context bridges for cross-unit record
> locals and inline NESTED records (no usable type id anywhere).**
> A value-type record **local** of a cross-unit type carries no
> resolvable type id at all (`AdrLoc: TXAdresse` decodes with
> `Local.TypeIdx = $1EA5`, a per-binary alias that resolves to no
> registry struct), and a record-typed **member** of another record
> (`TXAdresse.Anschrift: TXAnschrift`) carries `Member.TypeIdx = 0`
> (the record-field-id-is-zero gap above). The id-based priming
> (`FindClassIdxByRsmTypeId` / `FindStructByTypeIdx`) therefore can't
> establish record context at either hop, and a record has no VMT for
> the §6.32 priming to lean on. Two structural bridges in
> `DPT.Debugger.EvaluateVariable` close it, both unique-match-guarded
> like §6.18/§6.19:
>
> 1. **First hop (record local — inline OR pointer).** When the first
>    segment is a local whose id resolves to no record AND the
>    global-proximity fallback can't apply (locals have no `$20` file
>    offset), `FindRecordsByMemberName(Segments[1])` returns the records
>    that declare the accessed field name; if exactly one is a record, it
>    is the local's (pointed-to) type. Recovers `AdrLoc` → `TXAdresse`
>    via the field `Anschrift`.
>    A **typed-pointer** first segment (`CxPtr: PComplexRec := @CxLoc`)
>    resolves the same record TYPE here, but its slot holds a *pointer to*
>    the record, not the record inline — so the record-hop base must be
>    `*Addr`, not `Addr`. The cross-unit pointer alias is the same
>    unresolvable §4.2 per-proc id (`CxPtr`'s `$1EB1` ≠ `PComplexRec`'s
>    registry id `$8E80`), so there is no type-based "this is a pointer"
>    signal. The walk **probes at runtime**: gated to `FirstSegIsLocal`
>    (a record-typed *global* is always inline at its VA, and some have a
>    live pointer as their FIRST field — e.g. `GGlobalPrim.FAnsi`, an
>    `AnsiString` — whose readable target would otherwise trip the probe),
>    if the slot contents form an address from which the ENTIRE record
>    reads back, the slot is a pointer and `FieldAddr` is set to `*Addr`.
>    An inline record local's first-field bytes (`CxLoc.CxR1.C1Int =
>    $C1C1C1C1`, or `AdrLoc.Name`'s shortstring length+chars) are
>    virtually never a readable record-sized region, so the inline case
>    keeps `Addr`. Pinned by
>    `Test.DPT.MCP.Server.TestMcpEvaluateComplexRecordAndTypedPointer`
>    (`CxPtr.CxR1.C1Int` etc. must match the `CxLoc.*` reads).
> 2. **Nested hop (inline record member).** When a member resolved
>    via the record-hop branch carries no type id (`TypeIdx = 0`,
>    `PointerTargetTypeIdx = 0`) yet the walk continues, it must be a
>    nested inline record. `FindRecordBySizeAndMemberName(Member.Size,
>    Segments[I+1])` matches on the member's `Size` AND the next field
>    name; a unique hit primes the next record-hop. The size match is
>    **alignment-padding-tolerant**: the member's slot size is the gap to
>    the next field in the PARENT, which can exceed the nested record's
>    own extent by up to 7 bytes of padding the parent inserts to align a
>    following field (`TComplexRec.CxR2`'s slot is 12 though `TCxRec2` is
>    8 bytes, because the Int64-bearing `CxR3` must start 8-aligned). The
>    match accepts `nestedExtent <= slotSize` with `slotSize - nestedExtent
>    < 8`; the unique member-name match + `MatchCnt = 1` guard still
>    protect against a wrong-record pick. (`TXAdresse.Anschrift.Size = 112`
>    = `TXAnschrift`'s `61 + 51` exactly — the pre-padding shortstring
>    fixture that masked this until the aligned `TComplexRec` surfaced it.)
>    `FieldAddr` already points inline at the nested record's base (the
>    record-hop advanced it by `Member.Offset`, no deref), so only the
>    context needs priming.
>
> Note the controlled DebugTarget fixture also surfaced the §6.36
> offset-side decode (Shape C in §4.4): `AdrLoc`'s frame offset itself
> failed to decode before that fix, so the address was wrong on top of
> the context-priming gap — both had to land for the dotted walk to
> reach `TXAnschrift.Str`. Pinned end-to-end by
> `Test.DPT.MCP.Server.TestMcpEvaluateCrossUnitRecordLocalDottedWalk`
> (`AdrLoc.Anschrift.Str` = "Hauptstr.", with `AdrLoc.Name` = "Firma X"
> and `AdrLoc.Anschrift.Ort` = "Berlin" as nested-layout leakage
> guards). The Test.Lib manifestation (where the address DID decode but
> the type auto-detected as a stray enum/record) is the same end-user
> symptom reached by a different internal path; the bridges fix both.
>
> **Design limit — these record fields need an explicit `type=`; the
> no-type auto-detect cannot infer them.** The bridges above fix the
> *navigation* (the dotted walk reaches the right address), but
> `AutoDetectFormatterName` still cannot infer the terminal *type* of a
> cross-unit record local or its fields, because the RSM carries no
> usable type id for them: the record local's per-proc id is the
> unresolvable §4.15 alias (`AdrLoc` → `$1EA5`, resolves to no struct),
> and the record-field records the scanner captures hold **no primitive
> type id** — a `string[N]` field's record is just
> `$02 <NL> <name> $02 $00 <last-flag> $00 $00 $00` plus the size DWORD
> (verified byte-level on `TXAnschrift.Str`/`Ort`; the §6.33
> primitive-id decode applies to *class* `$2C` field records, which these
> inline record fields don't get). So `evaluate AdrLoc.Anschrift.Str`
> with no `type=` falls back to **int** (it reads the correct field
> bytes — `$75614809` = `09 'H' 'a' 'u'` — just under the wrong
> formatter), and bare `evaluate AdrLoc` **declines** with
> "could not be auto-typed" rather than fabricating a type. The decline
> is the *correct* behavior: unlike §6.36 manifestation A (where an
> interface local's alias id resolves to a bogus record and auto-detect
> confidently mis-names it), the record-local id resolves to nothing, so
> the consumer honestly refuses to guess. **The contract is: pass
> `type=shortstring` (or the right primitive) for record-field
> terminals.** Pinned by the no-type assertions in
> `TestMcpEvaluateCrossUnitRecordLocalDottedWalk` (bare `AdrLoc` declines
> and does not name a record). (The earlier "is a `$2C`-style sibling record
> with the type id emitted elsewhere?" caveat is now resolved: a byte-level
> sweep found none — the type is genuinely absent, not stored off to the
> side. The §6.33 primitive-id decode is specific to *class* `$2C` field
> records; inline record fields don't get one.)

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

* there is **no `$2C` field record** binding `Land` to a type id, and **no `$67` use-site** that identifies the *owning field* — the `$67 'ltInland'` use-sites are back-references to the canonical `$25` enum block (their linker token equals the canonical block's, byte-identical across every importer — §4.17), so they carry the enum identity but not *which* class's `Land` field is being read;
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
[Test.DPT.Rsm.Taifun.TestTfwRecordFieldEnumNameConventionBindsLand](../Test/Test.DPT.Rsm.Taifun.pas)
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

#### Getter method records (`$2E`) — getter-name recovery

A getter-backed property's `$31` TargetId (body+10..+11) does **not**
match any `$2C` field; it points at the getter's **`$2E` method
record**, which carries the SAME 2-byte id at `name_end+4..+5` behind
the fixed body anchor `00 00 00 E8`:

```
$2E  <NL>  <MethodName>   00 00 00 E8  <id-lo> <id-hi>  01  ...
                          └─ anchor ─┘  └ = TargetId ┘
```

Concrete (DebugTarget Win32): `GetCalcInt` → id `$3391` ==
`CalcProp`'s TargetId; `GetGreeting` → `$339D` == `Greeting`'s
TargetId. [TRsmPropertyLinker](DPT.Rsm.PropertyLinker.pas) builds a
`(method id → short name)` map from the `$2E` records during the same
walk and, for a getter-backed property, sets
`TRsmClassProperty.GetterName` (e.g. `GetCalcInt`). The owning class
(reported by `FindClassProperty`'s `AOwnerClass` out-param) qualifies it
to the getter's proc name `TPropHost.GetCalcInt`, which
`Reader.FindProcByName` resolves to a `SegmentOffset` →
`FBaseAddress + $1000 + SegmentOffset` code address. Pinned by
[Test.DPT.Rsm.LocalsReader.TestPropertyGetterNameRecovered32](../Test/Test.DPT.Rsm.LocalsReader.pas)
(with a leakage guard that field-backed `PlainProp` keeps an EMPTY
`GetterName`).

> **Consumer note — the dotted walk resolves properties two ways.** When
> a dotted segment (`Obj.Caption`) is neither an own field nor an inherited
> field on the runtime class chain, the class-hop branch of
> [DPT.Debugger.pas EvaluateVariable](DPT.Debugger.pas) resolves it as a
> property. Two sources are tried, **live RTTI first**:
>
> **1. Live runtime RTTI — primary, collision-proof (the path the Delphi
> IDE uses).** `TryResolveRttiProperty` reads the running instance's
> RTTI straight from the VMT: `[Obj] → VMT`, `[VMT − vmtTypeInfo] →`
> class `TTypeInfo`, then at each level of the `ParentInfo` ancestor chain
> walks **two** property tables for the name
> (`vmtTypeInfo = vmtClassName − 4·PtrSize` = −72 Win32 / −168 Win64; see
> System.TypInfo):
>
> * the **published `PropData`** table (`PropCount:Word` + that many
>   `TPropInfo`), which carries `published` properties (`Caption` etc.);
> * the **extended-RTTI `PropDataEx`** table that immediately follows it
>   (`PropCount:Word` + that many `TPropInfoEx` = `Flags:Byte`,
>   `Info:PPropInfo`, `AttrData(Len:Word` **including itself**`)`), which
>   carries `public` properties — the default `{$RTTI}` state emits
>   property RTTI for `[vcPublic, vcPublished]`. Each `TPropInfoEx.Info`
>   points at a `TPropInfo` of the **identical shape**, so the same decode
>   is reused; entries are gated to visibility `Flags and 3 >= 2`
>   (public/published) so a private/protected entry stays unreachable (§6.38).
>
> The matched `TPropInfo.GetProc` is decoded by its top
> byte/qword: `PROPSLOT_FIELD ($FF…)` → a **field offset** (read inline);
> `PROPSLOT_VIRTUAL ($FE…)` → a **VMT slot** (resolved against the live
> VMT) → call injection; otherwise a **static getter address** → call
> injection. The property type's RTTI kind (`tkUString`/`tkLString`/
> `tkWString` → managed string) drives the call's hidden-`@Result` ABI.
> This is **collision-free** because it reads the actual runtime metadata,
> so it works on arbitrarily large binaries where the RSM Format-A id
> cross-references collide (§6.37 history). It covers any **published or
> public** property. Pinned by
> `Test.DPT.MCP.Server.TestMcpEvaluatePublishedPropertyViaLiveRtti`
> (`DebugTarget` `{$M+} TRttiPropHost`: published `RttiPlain` field-offset,
> `RttiCalc` static-getter + call-injection, `RttiText` managed-string
> getter) and `TestMcpEvaluatePublicPropertyViaExtendedRtti`
> (`{$M+} TExtPubPropHost`: public `ExtPubPlain` field-offset + public
> `ExtPubText` getter, the second guarding the `TPropInfoEx` stride-skip).
> Proven live on TFW: `evaluate Result.Caption` → `001- Adresse suchen`
> (published, the form title the IDE shows) and `evaluate Result.IniName` →
> `TFormAd` (public — `CBaseForm.IniName`, `read GetIniName` returning
> `ClassName`, resolved via the `PropDataEx` walk + getter call injection),
> while the backing field `Result.FText` reads empty (the caption lives in
> native window state, recovered only by running `GetText`).
>
> **2. RSM `$31` property records — fallback (small / clean binaries).**
> When the property is on neither RTTI table (e.g. a `strict private`
> getter property the default `{$RTTI}` does not emit), the walk falls back
> to `FindPropertyViaVmtWalk`
> (`Reader.FindClassProperty` on the runtime class AND each live-VMT
> ancestor). Field-backed (`UnderlyingField <> ''`) bridges to the
> underlying field's offset; getter-backed (`GetterName` via the §4.16
> `$2E` decode + owning class) call-injects the getter. Pinned by
> `TestMcpEvaluateFieldBackedPropertyResolvesUnderlyingField` and
> `TestMcpEvaluateGetterBackedPropertyViaCallInjection`
> (`GGlobalPropHost.PlainProp`/`CalcProp`/`Greeting`). This path is
> reliable on small binaries; on large binaries the `$31`/`$2C`/`$2E`
> 2-byte id cross-references collide (the three-round investigation
> formerly tracked as §6.37), so this fallback is unreliable at TFW scale.
> Both **published and public** properties avoid it entirely now — they go
> through path 1, including via the extended-RTTI `PropDataEx` table (§6.38,
> closed). The only residual is a property emitted on NEITHER RTTI table
> (e.g. a `strict private` getter the default `{$RTTI}` omits) on a huge
> binary: it has no collision-free source and may not resolve — a narrow
> design limit, not a tracked gap, since such properties are rarely the
> inspection target.
>
> **Call-injection mechanism (shared by both paths).** Because the
> debuggee is frozen between `WaitForDebugEvent` / `ContinueDebugEvent` and
> only the sole `TDebuggerThread` may pump events, the MCP worker thread
> cannot run a getter itself: `CallTargetFunction` parks a request
> (`FCallInjectRequest`) and wakes the loop's breakpoint-pause wait;
> `PerformInjectedCallOnDebugThread` (on the debug thread) snapshots the
> paused context, rewrites it into a call frame — `Self` in EAX (Win32) /
> RCX (Win64), the hidden `@Result` pointer in EDX/RDX for a managed
> (`string`) return, the return address pointing at a `$CC` INT3
> trampoline on a `VirtualAllocEx` scratch page, PC = getter, HW
> breakpoints disabled, 16-byte-aligned stack grown below the live frame —
> `ContinueDebugEvent`s, and a self-contained nested event pump waits for
> the trampoline INT3. It captures EAX/RAX (ordinal/pointer) or reads the
> scratch result slot (managed string), restores the snapshot context, and
> leaves the target frozen in the original paused state. The result is
> deposited into the scratch slot so the **normal formatter pipeline**
> reads it through `FieldAddr` exactly like a field.

### 4.17 `$64` / `$66` / `$67` / `$70` — cross-unit symbol-import table

The Delphi linker emits one segment per unit a compilation unit
imports a symbol from. Each segment is bracketed by:

```
$64  <NL: u8>  <UnitName>  $00 $00 $00            ; segment open
  (
    $66  <NL: u8>  <TypeName>    <token: u32 LE>  ; imported type
    $67  <NL: u8>  <SymbolName>  <token: u32 LE>  ; imported symbol
    $70  <NL: u8>  <FileName>    <token: u32 LE>  ; source-file ref
  )*
$63                                                ; SCOPE_END (optional)
```

The `$66` / `$67` / `$70` records may appear in any order and may mix
freely within the segment. The 4-byte payload is an **opaque linker
token**, NOT an in-image RVA. (Earlier revisions of this section, and
the model field — then named `Rva` — labelled it an RVA; that was a
former §6.29 side-item, corrected here. The decoder field is now
`TRsmUnitUseRef.LinkToken` / `TRsmSourceFile.LinkToken`.) Two
independent proofs:

* **Magnitude** — the values far exceed any image RVA: `$66 'Boolean'`
  = `$62BC8138` (≈ 1.66 GB) on the ~1.3 MB DebugTarget, `$66 'TLandTyp'`
  = `$7DA69008` (≈ 2.1 GB) on TFW. No offset into a multi-MB image can
  be that large. Pinned by the `> $10000000` ceiling assertions in
  `TestUnitUseSegmentSystemIsDecoded` (Win32) and
  `TestTfwUnitUseTableContainsTLandTyp` (TFW).
* **Structural identity with the §4.6.2 `$25` token** — the payload is
  the same opaque token the `$25` enum-constant body carries at +3..+6,
  which §4.6.2 already established is byte-identical across Win32/Win64
  (different image bases) and follows a `base + ord*3` stride, so it
  cannot be an address.

What each kind's token mirrors:

* For `$66 <TypeName>` it equals the bytes at body+3..+6 of the
  canonical `$2A <TypeName>` registry entry (verified against
  TFW's `$66 'TLandTyp'` = `08 90 A6 7D` ⇔ `$7DA69008`).
* For `$67 <SymbolName>` referring to an enum element it equals the
  bytes at body+3..+6 of the canonical `$25 <SymbolName>` block (the
  §4.6.2 linker token); siblings of a single enum carry tokens with a
  `+3` LSB stride matching the element's doubled ordinal (TFW's
  `ltInland`/`ltAusland`/`ltEUNetto`/`ltEUBrutto`/`ltBLNetto` carry
  `81 39 A4 10` / `84 39 A4 10` / `87 39 A4 10` / `8A 39 A4 10` /
  `8D 39 A4 10` — first byte `+3` per ordinal, the trailing three
  bytes conserved). The byte-identity between a `$67` use-site and its
  canonical `$25` block is pinned by
  `TestTfwUnitUseTableLtInlandTokenMatchesCanonicalBlock` — and is the
  decisive disproof of "RVA": an address could not be shared verbatim
  across every use-site of the symbol. (Whether procedure / method
  `$67` tokens encode anything address-like is **not** established; no
  consumer reads them.)
* For `$70 <FileName>` it identifies the source file the symbol was
  declared in (`.pas` / `.inc`); the token is decoded into
  `SourceFiles[].LinkToken` but no consumer uses it (proc →
  declaring-unit attribution keys on the name, §4.18).

**Why it is NOT a useful declaration-lookup key (investigated, negative).**
A follow-up explored exposing a `FindClassByLinkToken` / token-keyed
declaration lookup (to give RsmDesk a collision-free "jump to declaration"
and to feed §6.34). A diagnostic over DebugTarget.rsm (382 `$2A` joins,
230 `$25` joins; the diagnostic was converted to the pin below) **refuted
the general primitive**:

* **Types — redundant.** For every `$2A` entry the 4-byte token's **low
  word equals the registry primary id** (token = `primary | (extra << 16)`).
  The primary already identifies the type uniquely, so a token lookup is
  the existing primary-id lookup with 16 redundant bits.
* **Aliases — misleading.** A type-alias *use-site* carries the
  **underlying** type's token, not the alias's own `$2A` token:
  `$66 'DWORD'`/`'HWND'`/`'UINT'`/`'SIZE_T'` all carry `Cardinal`'s token
  (`$281FFF6`), while the `$2A 'DWORD'` registry entry has its own
  distinct token (`$179C0012`, primary `$12`). A name-keyed token lookup
  would therefore navigate `DWORD` to `Cardinal`, not to the `DWORD`
  declaration.
* **Consts — colliding.** Ordinal-0 named consts share a token across
  unrelated families (`varEmpty`, `S_OK`, `CP_ACP` all = `$97E667F1`), so
  the token is not unique for the const/symbol space either.

The **only** clean case is the narrow same-enum identity already pinned
(`$67` element ↔ its own `$25` block, byte-identical, `+3` sibling
stride). Conclusion: no token-keyed reader API was added — RsmDesk
navigation is better served by the existing name + primary-id lookups,
and §6.34's collision problem is not solved by this token (it carries no
field-ownership; §4.15 Pass 3). The token's full internal structure
(type: `primary|extra`; enum: enum-id high bits | ordinal low byte;
const: ordinal-base collisions) is only partially decoded, but has no
consumer, so it is not tracked as an open gap. Pinned by
[Test.DPT.Rsm.Reader.TestLinkTokenSemanticsNotAUsefulLookupKey32](../Test/Test.DPT.Rsm.Reader.pas)
(byte-identity, low-word-equals-primary, alias-resolves-to-underlying,
alias-differs-from-own-`$2A`).

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
+ `LinkToken`), and every `$64` segment carries a `SourceFileIdx`
**foreign key** into that list — so the importing unit's name is
stored **once** (deduplicated by name via the scanner's
`FSourceFileByName` map) rather than copied onto every segment of a
block. `HandleSourceFileIntroRecord` recognises the standalone
introducer (`$70 <…pas> <token:4> $00 $64`), records/dedups the
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
followed by `<token:4> $00 $64` and opens the next unit's import
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
  `$66 'Boolean'` entry at the canonical LinkToken `$62BC8138`, and
  asserts it exceeds the `$10000000` RVA ceiling (the not-an-RVA
  proof).
* [`Test.DPT.Rsm.Scanner.TestUnitUseSymbolReferencePayloadHasPlusThreeStride`](../Test/Test.DPT.Rsm.Scanner.pas)
  — three `System.UITypes` `vk*` siblings carry the `+3` LSB stride
  the encoding interpretation depends on (a uniform `+3` step proves
  the payload is a token, not an address).
* [`Test.DPT.Rsm.Scanner.TestUnitUseFalsePositiveRejection`](../Test/Test.DPT.Rsm.Scanner.pas)
  — leakage guard: a synthetic `$64` lacking the trailing-zero
  anchor must not open a segment.
* [`Test.DPT.Rsm.Taifun.TestTfwUnitUseTableContainsTLandTyp`](../Test/Test.DPT.Rsm.Taifun.pas)
  — TFW.rsm carries ≥60 `$66 'TLandTyp'` entries with the canonical
  LinkToken `$7DA69008`, also asserted above the RVA ceiling.
* [`Test.DPT.Rsm.Taifun.TestTfwUnitUseTableLtInlandTokenMatchesCanonicalBlock`](../Test/Test.DPT.Rsm.Taifun.pas)
  — `$67 'ltInland'` payload `$10A43981` matches the bytes at
  file offset 54816766 + 11 (the canonical `$25 'ltInland'` block's
  body), anchoring the cross-record structural relationship and
  proving the payload is the §4.6.2 token (not an RVA).
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
$70 <NL> <ThisUnit'sSourceFile> <token:4 LE> $00 (\$64|\$65) ; introducer
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
> delta/RLE-encoded in a dedicated section. The file is a TDS-style
> container with a fully-decoded 32-byte header (`+4` dir offset `$420`,
> `+8` header size `$20`, `+C` version 1, `+10` link timestamp, `+18`
> 8-byte image base) → a typed source-search-path table at `$426` → a
> large HEAD of per-module debug sections (each opened by a `$70
> <SourceFile.pas>` introducer, holding `$67`/`$68` import-symbol refs,
> embedded code, and RTTI strings) → a compact TAIL symbol/type index
> (the `$35` module-dependency tree + the per-unit `$70`/`$2A`/`$28`/
> `$25` records §4.1–§4.18 decode). The per-statement line table is
> NOT in these sections (§6.29 round 4: the per-module bytecode is
> TYPE RTTI/layout, not lines); the proc → **declaring-unit** anchor
> this section delivers does not depend on it.

### 4.19 `61 4D <pb> 00` per-unit module record

Every compilation unit (RTL/VCL **and** program) is wrapped in a
fixed-header **module record**, discovered in §6.29 round 4 and pinned
by
[`Test.DPT.Rsm.Scanner.TestModuleRecordChain32`/`…64`](../Test/Test.DPT.Rsm.Scanner.pas):

```
61 4D <pb> 00   <$25>  <size: u16 LE>  <aux: u16 LE>  <link-ts: u32>  <body…>
\__ magic ___/   tag    \_ record span _/  \_ flags _/                 \_ §4.17/§4.18 $70, $35, $25, $2A, $28 + §4.19 payload
```

* **Magic** `61 4D <pb> 00`. `<pb>` is the **platform byte** — `$03`
  on Win32, `$23` on Win64 — the same marker as the container header's
  `+15` (§6.29). So `DebugTarget.rsm` (Win32) has the magic
  `61 4D 03 00`; the Win64 fixture has `61 4D 23 00` and **zero**
  `61 4D 03 00`.
* **Tag** `$25` at `+4`, then **`size`** (u16 LE at `+5..+6`) = the byte
  distance from this magic to the **next** module record's magic, then a
  **u16 aux/flags** field at `+7..+8` (NOT padding: `$0000` for the leaf
  enum units, `$0002` for the program/`.dpr` — meaning UNCERTAIN), then
  the 4-byte link timestamp. Walking `magic + size` lands exactly on the
  next magic — verified for the 4 program-unit records
  (`DebugTarget.EnumAlpha` size `$2E2` → `EnumBeta` `$284` → `EnumGamma`
  `$28B` → `DebugTarget` (`.dpr`) `$E6E`). The `size` is u16, so it only
  spans **adjacent** records;
  where two units are separated by a large non-record region (embedded
  code / string pools), the chain is local, not file-global.
* **Body** holds, in order: the unit's `$70` source-file introducer
  (§4.18) and `$65`/`$64` used-unit list, the `$35` module-dependency
  records (§4.17), the `$25`/`$2A`/`$03` symbol/type/enum definitions,
  then a **type-RTTI / layout bytecode** region (recurring `XX 31`
  anchors + `<even> 05 <small>` triples, terminator `42 00`), a
  **section-name table** (`.text`/`.itext`/`.data`/`.bss`/`.tls` with
  per-section base values), and a trailing **per-segment base-VA table**
  (e.g. `$004DAF84`, `$004E1D68`, … the `.data`/`.bss`/`.tls`
  contributions). The RTTI/layout bytecode is present **even in the
  1-line `EnumAlpha` module** (which has one source line but one enum
  type, `TStatus`) — proving it scales with TYPE complexity, not source
  lines, which is how §6.29 round 4 refuted the round-3 hypothesis that
  this bytecode was the line table.

No current reader path consumes the module-record wrapper, the
RTTI/layout bytecode, or the segment-base table; the scanner walks the
inner `$70`/`$35`/`$2A`/`$28`/`$25` records directly and ignores the
`61 4D` framing. Documented here because it is the structural context
§6.29's line-table search runs inside.

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
`TRsmEnumDef` (its unit name taken from the §4.18 `$70` introducer the
scan is inside, §6.25 R2) — that's the
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
+ the owning-unit `$70`-introducer lookup in
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
* **R2 — mislabeled unit names (CLOSED via the §4.18 `$70` anchor).**
  The synthesized def's `UnitName` now comes from the **§4.18 `$70`
  source-file introducer the scan is inside** (`FCurrentSourceFileIdx`) —
  the same trusted declaring-unit anchor `HandleProcRecord` stamps onto
  procs (each unit's `$2A` block is preceded by its own `$70`
  introducer). This **replaced** the former 1 MB forward "name search"
  that grabbed the unit-init proc's dotted namespace and used a
  `T`/`E`/`I`+Upper naming-convention filter to reject method names —
  a heuristic that (a) overshot into unrelated namespaces (TFW
  `TIdPortList → Winapi.WinSock`, actually `IdGlobal`), (b) could not
  reject the `C`-prefix method names the convention filter didn't cover
  (`TZUGFeRDXMLObjectTyp → CZUGFeRD….GetSequence` instead of
  `Base.Xsd.ZUGFeRD.Types`), and (c) fell back to a nearby method when
  the init proc sat >1 MB out (the original R2 defect, e.g.
  `TNumColorsRange → TPlannerColorArrayList.Add`). The `$70` anchor fixes
  all three **and retires the naming-convention crutch** — the
  "offset-indexed post-process" this entry previously prescribed, now
  realised by reusing the §4.18 `SourceFiles` index. Verified on TFW: all
  55 surviving synth enums have `FCurrentSourceFileIdx ≥ 0`; the rare
  early-RTL `$2A` with no preceding `$70` falls back to the nearby dotted
  proc and is phantom-filtered downstream regardless. Pinned by the
  `TestEnumDefsNotOverCollected32` `StatusUnit` / never-method-like
  assertions (DebugTarget) and `TestTfwSynthEnumPhantomResidual` step (5)
  (TFW: no surviving synth EnumDef has a `TClass.Method` unit). Folded
  into §4.8.
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
([Test.DPT.Rsm.Taifun.TestTfwSynthEnumPhantomResidual](../Test/Test.DPT.Rsm.Taifun.pas))
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
[Test.DPT.Rsm.Taifun.TestTfwSynthEnumPhantomResidual](../Test/Test.DPT.Rsm.Taifun.pas)
(asserts the representative phantoms are gone, that NO surviving synth
def still matches a dropped convention, and the leakage guards above);
`TestTfwRecordFieldEnumNameConventionBindsLand` /
`TestTfwEnumTypedFieldResolvesToPrimary` confirm real enum binding is
unaffected.

**Same-`(name,unit)` synth duplicates — CLOSED (condition (d)).** A
synthesised `EnumDef` that shares its `(TypeName, UnitName)` with a
`$03`-sourced def is the **same Delphi type** (a type name is unit-unique),
so the `$03` is authoritative and the synth is a redundant or
const-polluted view. `FilterPhantomEnumDefs` now drops it (condition (d),
[DPT.Rsm.Reader.pas `FilterPhantomEnumDefs`](DPT.Rsm.Reader.pas)).
Condition (b) missed these: element names shared across sibling enums make
the first-wins element→type map ambiguous, so a synth whose elements all
live in its *own* `$03` can still map an element to a *different* `$03`
enum and fail the "all map to one type" test — e.g. TFW's `TLngTyp`
(28/28 elements in its `$03`, yet not dropped by (b)), and the
zero-overlap pollution cases `TCalFirstWeekMode` (0/5), `TThumbButtonState`
(0/1). A reconnaissance diagnostic found **24** such same-unit synth
duplicates on `C:\MSE\TFW\TFW.rsm`. The drop is **unit-scoped**, so a
genuine cross-unit homonym survives — TFW's `TDataType` (synth in
`VCLTee.TeeSpline`, `$03` in `bmpfilt`, 0/5 overlap) keeps its own
identity, as do DebugTarget's `EnumAlpha`/`Beta`/`Gamma` `TStatus` trio.
This subsumes the former "`TTokenKind` doubles" residual (TTokenKind isn't
doubled on the current TFW build, but the mechanism is identical). Pinned
by
[Test.DPT.Rsm.Taifun.TestTfwEnumDefNoSameUnitSynthDuplicate](../Test/Test.DPT.Rsm.Taifun.pas)
(invariant: no synth survivor shares `(name,unit)` with a `$03` def;
leakage/non-vacuity: a cross-unit homonym still survives).

**Remaining residual (smaller, not high-confidence-droppable).** After
(c)/(d), what still leaks is:
* **`T<Upper>`-named const-borrow phantoms** (`TWMNotifyRE`,
  `TIdPortList`, … carrying named `const`s, with **no** `$03` of their
  own) — name is byte- and convention-indistinguishable from a real enum,
  and §6.26 refuted any per-record byte discriminator, so these are the
  **irreducible** remainder. (d) cannot help: there is no same-name `$03`
  to supersede them. The `skRecord` kind-oracle can't safely prune them
  either (the `TZUGFeRDXMLObjectTyp` counterexample). The multi-family
  subset is still caught by the dup-ordinal guard.
* **`E<Upper>` / `C<Upper>` families** (`EAplError`, `CExcel`, …) —
  droppable by extending the convention list, but **deferred by design**
  (codebase-specific conventions, higher false-positive risk than
  `I*`/`P*`/Windows-struct).
* **R3 (dropped polluted enums)** as before — deferred (lesser-evil
  tradeoff). (**R2 is now CLOSED** — see the R2 bullet above: the
  synthesized def's unit comes from the §4.18 `$70` introducer, retiring
  the name-search + its `T`/`E`/`I` convention crutch.)

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
> `Test.DPT.Rsm.Taifun.TestTfwPerProcRefIsNotUnitUseTableIndex`), leaving
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
> §6.30 (inline-declared `var X := expr;` local BPRel-offset
> mis-decode) is **closed** — the `$66 $00 $01 $04` inline-var form is
> decoded in §4.4, pinned by `TestInlineVarLocalsDecoded32/64` +
> `TestInlineVarWideOffsetMonotonic32/64`. §6.31 (the `'T'`-prefix
> class-name discovery crutch) is **closed** — the candidate gate is now
> convention-free (structural-anchor based) with the trailer-index
> perf restoration in §4.11, pinned by `TestNonTPrefixClassDiscovered32/64`
> + `TestLargeBinaryNonTClassDiscovered`. §6.32 (the dotted-walk
> record-hop priming wrongly firing on a class-instance **local** whose
> per-proc `TypeIdx` aliases to a record) is **closed** — the VMT-priority
> priming-skip now covers BP-relative/global object locals, folded into
> the §4.2 consumer note; verified live (the four `V.FExpected*` fields
> resolve on `Test.Lib`).
> The last number used is **§6.34** (enum/class-typed fields of
> unit-local-colliding convention classes) — now **closed as a design
> limit** after three refuted decode rounds (nearest-LOW-byte, global
> registry uniqueness → garbage, file-offset proximity → matches absent
> or 5–79 MB away): the field-type id in these `marker@+6/+7`,
> `byte+5 != $0C` records is a per-unit local reference, not a `$2A`
> registry primary — the field-side analogue of the §6.27 / §4.2 design
> limit. Folded into **§4.9** (the `§6.34 (closed — design limit)`
> subsection). §6.33 — the large-binary `string`-field
> auto-typing stack — is **closed** (all three defects fixed, folded into
> §4.9). **§6.35** (frameless x86 procs) is now **fully closed** — all
> four halves (frame-base ESP rebase, reg→reg param spill, narrow-memory
> param spill, and register-resident locals via the `$16` LOCAL form →
> `lkRegisterResident`) are decoded and folded into the §4 register-param
> consumer note. **§6.36 manifestation B** (cross-unit record LOCAL
> dotted walk — `AdrLoc.Anschrift.Str`) is now **closed**: it needed
> the §4.4 Shape-C offset decode (the 2-byte cross-unit type alias
> pushes the BPREL offset to `+5`) **plus** two record-context bridges
> in the dotted walk (first-hop `FindRecordsByMemberName` for the
> type-id-less record local, nested-hop `FindRecordBySizeAndMemberName`
> for the inline nested record), folded into §4.4 + the §4.14 consumer
> note and pinned by `TestMcpEvaluateCrossUnitRecordLocalDottedWalk` +
> `TestCrossUnitRecordLocalOffsetAndLayout32`. The same manifestation was
> later extended to a cross-unit **pointer-to-record** local
> (`CxPtr: PComplexRec`) and a deeper aggregate with alignment padding
> (`TComplexRec` embedding three records): three more decode fixes landed
> together — (a) §4.4 Shape A now admits the `$1E` 2-byte-alias Hi byte
> (a pointer local's offset was decoding to a bogus out-of-frame `-504`);
> (b) the §4.14 nested-record bridge tolerates alignment-padding slack
> (`slotSize - nestedExtent < 8`, not exact equality); (c) the first-hop
> bridge derefs a typed-pointer first segment at runtime. Pinned by
> `TestMcpEvaluateComplexRecordAndTypedPointer` +
> `TestComplexRecLayoutAndPointerLocalOffset32`. **§6.36 manifestation A**
> (an *interface*-typed local auto-detecting as an unrelated record) is now
> **closed** — the layout-gated live recovery in `EvaluateVariable` /
> `TryRecoverReferenceType` names the runtime implementing class instead of
> the bogus record (see the §6.36 entry). Its former static-route residual —
> interface support for the no-process RsmDesk viewer — is **also closed**:
> `ScanTypeRegistry` now admits `I<Upper>` `$2A` names and synthesizes
> discovered `skInterface` structs (new `TRsmStructKind` ordinal), pinned by
> `TestInterfaceDiscoveredAsStruct32/64` and written up in §4.8. (The live
> `Lst` one-byte-truncation loss remains a documented design limit in §6.36,
> not an open gap.) **§6.37** (getter-backed / published property
> evaluation) and **§6.39** (`.rsm` proc `SegmentOffset` drift, fixed
> consumer-side via the `.map` fallback) are also **closed**. The only
> still-OPEN tagged §6 entry is **§6.29** (per-statement line table) — and
> that is now a *confident negative* (four rounds: the Athens `-VR` linker
> appears not to emit the table at all; lines live only in the `.map`), with
> its residual closure items explicitly parked as cost/payoff-unfavourable.
> The highest §6 number ever used is **§6.39**; the next gap discovered MUST
> be numbered **§6.40**. Numbers are never reused or recycled — commit
> messages, code comments, and pin docstrings reference closed §6.N entries
> by their original number long after the §6 entry itself is gone, and
> renumbering would silently invalidate those references.

### 6.29 Per-statement address → line table encoding (`GAP`)

[DPT.Rsm.Scanner.pas `HandleSourceFileIntroRecord`](DPT.Rsm.Scanner.pas)
decodes the `$70` source-file records (incl. the proc → declaring-unit
anchor, §4.18) but **not** the per-statement address → line table that
Embarcadero's RSM Debug File docs say the container carries (the
equivalent of the `.map`'s `Line numbers for <Unit>` arrays).

**Container framing — now fully decoded** (a reconnaissance round on
`DebugTarget.rsm`, both platforms; pinned by
[`TestContainerHeaderLayout32`/`…64`](../Test/Test.DPT.Rsm.Scanner.pas)).
The 32-byte header is fixed-field:

```
+0  (4) magic 'CSH7'
+4  (4) directory offset   = $420
+8  (4) header size        = $20
+C  (4) version            = 1
+10 (4) link timestamp     -- Unix-epoch link stamp; CHANGES every
        rebuild (observed $5CC3424A then $5CC372B0), so non-zero only
+14 (4) machine / flags    -- byte +15 = $03 (Win32) / $23 (Win64); the
        only platform-distinguishing header byte, meaning UNCERTAIN
+18 (8) image base         = $00400000 (Win32) / $140000000 (Win64),
        8-byte LE -- matches the base the scanner trusts in §4.5
```

The directory at `$420` is the **source-search-path table**: a chain of
typed `{u16 type, u32 size, payload}` records starting at `$426` (type 4
= search-path list, type 3/2/1 = the program dir + RTL `\lib\...\release\`
paths), ending in a zero-padding run ~`$5FA`. It is NOT the line table.

**Two-region file layout (NEW — the prior round assumed one inline
stream).** `DebugTarget.rsm` (7.7 MB) splits into:

* **HEAD** (~`$700` … ~`$742000`, the bulk): per-module debug sections,
  each opened by a HEAD `$70 <NL> <SourceFile.pas>` introducer
  (`System.pas` @ `$717`, then `SysInit.pas`, `System.Types.pas`, …,
  `System.Classes.pas` @ `$583AEB` — RTL/VCL units only). Within a
  section sit `$65` used-unit lists, `$67`/`$68` imported-symbol
  references carrying 4-byte linker tokens (`$67 <NL> @DelayLoadHelper2
  <token>`, `kernel32.dll` thunks, … — see the §4.17 not-an-RVA
  finding), interleaved with **embedded x86 machine
  code** (e.g. `$6F0000`: `8D 45 F8 8B D3 85 D2 …` = `lea eax,[ebp-8];
  mov edx,ebx; test edx,edx …`, with `E8 00 00 00 00` relocatable
  calls), RTTI **strings** (`$500000`: `"…will dispatch to the client's
  code."`), and 2-byte-id type/field data (`$600000`). Round 1 guessed
  the line table lived here; **round 2 refuted that** (these are RTL/VCL
  units, which carry NO `.map` line numbers — see below).
* **TAIL** (~`$742000` … EOF, ~140 KB): the compact symbol/type INDEX —
  per program-unit `$70`/`$64`/`$35`(module-dependency)/`$25`/`$2A`/
  `$28`/`$2C`/`$0E` records. This is the stream §4.1–§4.18 decode. The
  program's OWN units (`DebugTarget.dpr` @ `$7432CF`, `DebugTarget.EnumAlpha`,
  …) appear ONLY here; their `$28` procs carry an inline address but **no
  line table** (`$28 TargetProcedure 20 00 00 47 00 DB 04 81 03 02 10 11
  2E 00 63` → entry `$DA004`, then immediately the next `$28`).

**Round-2 reframe — which units even HAVE a line table.** `DebugTarget.map`
emits a `Line numbers for <Unit>` block for **only the 4 program units**
compiled with debug info — `DebugTarget.EnumAlpha` / `EnumBeta` /
`EnumGamma` (one entry each, line @ offset 0) and `DebugTarget` (the
`.dpr`, a large `.text` + `.itext` table). **No RTL/VCL unit** (System,
SysUtils, Winapi.*, …) has a `.map` line block at all. The 4 program
units live ONLY in the TAIL; their TAIL regions are `$70`/`$65`/`$66`
(imported type) / `$67` (imported symbol) descriptors + `$2A`/`$28`/`$25`
definitions, with **no dense numeric block** (instrumented around
`$70 …DebugTarget.dpr` @ `$7432CF` and `$28 TargetProcedure` @ `$7560D9`).
So round 1's "the table lives in a HEAD section" was wrong: the HEAD is
all RTL (no line numbers), and the units that DO have line numbers sit in
a TAIL that contains no table.

**Refutations (encodings the line table is NOT)** — whole-buffer search,
finder verified by a control (`47 00 DB 04`, the proc-entry token, IS
found at `$7560ED`). For the `.dpr` offsets line 18 @ `$DA004` (entry),
19 @ `$DA00A`, 20 @ `$DA032`, 21 @ `$DA040`, 25 @ `$DA059`, 26 @ `$DA067`:

| Encoding | proc ENTRY `$DA004` | every mid-statement line |
|---|---|---|
| proc-addr token `(O+$401000) shl 4 or 7` | **present** (its `$28` slot) | absent |
| plain absolute VA `O+$401000` | absent | absent |
| raw u32 segment offset `O` | absent | absent |
| raw 3-byte offset `O` | absent | absent |

Plus: NOT a plain `lines[]` array in source order — neither `0D 00 0E 00
0F 00 10 00` (lines 13–16 u16) nor `E2 02 E3 02 E4 02` (`.itext` lines
738–740) nor the byte/delta runs (`04 07 30 1D 06 28 0E 14 …` advances,
`12 13 14 15 18 19 1A 1C` absolute lines, the interleaved `{addr,line}`
forms) occurs anywhere. ~9 encodings refuted in total.

**Round-2 conclusion (integer-form negative).** Only the proc ENTRY
address is in the file, and only in the `$28` token form; NO
per-statement line address is present in ANY integer-address form
anywhere. This is fully consistent with a **delta/opcode bytecode**
(which never stores absolute addresses) — so it pins "not stored as
integers" but does NOT prove the table is absent. Round 2 leaned toward
"not emitted at all"; **round 3 (below) retracts that lean** after
finding name-free bytecode blobs in the TAIL.

Pinned by
[`Test.DPT.Rsm.Scanner.TestLineStatementAddrAbsentInAllIntegerForms32`](../Test/Test.DPT.Rsm.Scanner.pas)
(the multi-encoding absence + entry-only-in-token control),
[`…TestRsmProcEntryRvaNotInLineTableWireForm32`](../Test/Test.DPT.Rsm.Scanner.pas)
(the narrow original negative), and the container header by
[`…TestContainerHeaderLayout32`/`…64`](../Test/Test.DPT.Rsm.Scanner.pas).

**Round-3 findings — TAIL gap analysis (the "absent" lean is
RETRACTED).** Scanning the TAIL (`$742000`…EOF, 139 439 bytes) for
maximal runs containing no length-≥4 printable identifier surfaced 69
name-free blobs. The TAIL has exactly **4 `$70` introducers** — the only
4 units with `.map` line tables — `DebugTarget.EnumAlpha` @ `$742A76`,
`EnumBeta` @ `$742D58`, `EnumGamma` @ `$742FDC`, and the `.dpr` (full
path, `$65`) @ `$74329B`, the **last** one; so everything past `$7432CF`
is the `.dpr` region. Three blob CLASSES, hand-classified:

* **UTF-16 string pool** (`$75BA0B`, ~12.5 KB): `{?,count,$FFFFFFFF,
  len:u32, UTF-16 chars}` descriptors — `"CompName"`, `"EmptyName"`,
  `"stderr-tag-line"`, … the program's string literals. NOT lines.
* **Uniform field/VMT records** (`$742000`, ~2.5 KB): repeated 11-byte
  `04 26 00 <id> 80 02 00 02 00 <id2> 55` with `<id>` stepping `$10` and
  `<id2>` stepping `$18`. NOT lines.
* **Opcode bytecode** (`$75ED06`, ~19.8 KB, in the `.dpr` region): the
  prime suspect. Recurring 2-byte anchors `XX 31` whose low byte steps
  (`45 31`,`49 31`,`4D 31`,`55 31`,`5D 31`,`61 31`,`7D 31`,`81 31`)
  interleaved with `<even> 05/06 <small>` triples (`0A 05 10`,`10 05 12`,
  `0A 05 14`,`0A 06 0E`,…) whose third byte is line-number-magnitude and
  whose even first byte smells of the §2 LSB-as-continuation address
  advance (`0A`→5, `10`→8). A run terminator `42 00` separates groups.

So the TAIL is NOT fully accounted for by simple records — there is real
bytecode here, which is why "absent" can no longer be the leading
hypothesis. **BUT the `$75ED06` blob is NOT yet validated as the line
table, and there is a strong competing explanation:** it sits
immediately after a uses/dependency list naming `System.Rtti` and
`System.TypInfo`, and its triples do **not** align to the `.dpr`'s known
first entries (`13 @ $D9FAC, 14, 15, 16, 18 …`) — so it may be an **RTTI
/ typeinfo program** rather than a line program. Present-vs-absent for
the line table is therefore **still open**, but narrowed to: "is
`$75ED06` (or a sibling bytecode blob) the line table, or is it RTTI?"

**Round-4 verdict — the `$75ED06` candidate is REFUTED; it is type
RTTI/layout, not lines.** Three findings settle it:

1. **The bytecode is per-unit type data, in a module-record wrapper
   (now decoded as §4.19).** Every unit is wrapped in a `61 4D <pb> 00
   $25 <size:u16> 00 00` record; the 4 program units chain by `size`
   (`EnumAlpha` `$2E2` → `EnumBeta` `$284` → `EnumGamma` `$28B` →
   `.dpr` `$E6E`). The same `XX 31` + `<even> 05 <small>` bytecode, the
   `.text`/`.itext`/`.data`/`.bss`/`.tls` section table, and a segment
   base-VA table sit inside **every** module record — **including the
   1-line `EnumAlpha`** (one source line, but one enum type `TStatus`).
   A line table would scale with source lines; this scales with TYPE
   complexity. So the bytecode is RTTI/layout, not lines.
2. **No address progression matches.** The `.dpr`'s intra-proc
   address-advance run (TargetProcedure: `06 28 0E 14 05 0E 25`) is
   absent in raw, `×2`, `×4`, and `×2+1` scalings everywhere in the
   file; the `$75ED06` advances (mostly `0A`→5, near-uniform) do not
   resemble the `.dpr`'s varied advances. The `XX 31` anchors (`$3145`,
   `$3149`, …) match no `.text`/`.itext` offset.
3. **Size.** `$75ED06` is ~19.8 KB; the `.dpr`'s entire `.map` line
   table is ~280 entries (≈ <1 KB encoded) — orders of magnitude off.

**Re-confirmed conclusion (now a confident negative).** Across four
rounds: no integer-form line address (R2 pin), no delta-advance run in
any scaling (R4), and the only TAIL bytecode positively identified as
**type RTTI/layout** in §4.19 module records (R4) — present even for a
1-line unit. The strong reading is that **this Studio-37 (Athens) `-VR`
linker does not emit a per-statement address→line table at all**; the
line→address mapping lives only in the `.map`. The `.rsm` carries
proc-entry addresses (§4.1), the declaring-unit edge (§4.18), type
RTTI/layout (§4.19) — but not per-statement lines. The DPT debugger's
use of the `.map` for source-line breakpoints is therefore a *correct*
design, not a workaround.

**Residual (what would close §6.29 outright):** (a) a `-GD`/full-TDS
build comparison to confirm no linker switch emits the table into the
`.rsm`; (b) positively decoding the §4.19 RTTI/layout bytecode to prove
it is wholly type data with no line sub-stream. Neither is needed by any
consumer (the `.map` has lines), so the cost/payoff has tilted away from
chasing it further. **Independent side-item — RESOLVED.** The `$70` /
`$66` / `$67` 4-byte fields were mislabelled "RVA" by §4.17/§4.18 and by
the decoder field `Rva`; they are an **opaque linker token** (the §4.6.2
`$25` token family), NOT an in-image RVA — proven two ways: magnitude
(`$66 'Boolean'` = `$62BC8138` ≈ 1.66 GB ≫ image; the HEAD-region RTL
`$67` tokens `$5CBC7F3C` / `$5CBA54BB` are likewise link-stamp-range),
and byte-identity between a `$67` use-site and its canonical `$25` block
(an address could not be shared verbatim across every use-site). The
field is renamed `TRsmUnitUseRef.LinkToken` / `TRsmSourceFile.LinkToken`;
the relabel is documented in §4.17 and pinned by the not-an-RVA ceiling
assertions in `TestUnitUseSegmentSystemIsDecoded` /
`TestTfwUnitUseTableContainsTLandTyp` plus the byte-identity pin
`TestTfwUnitUseTableLtInlandTokenMatchesCanonicalBlock`.

Pinned by
[`…TestModuleRecordChain32`/`…64`](../Test/Test.DPT.Rsm.Scanner.pas)
(the §4.19 module-record framing + chain that carries the RTTI bytecode),
in addition to the integer-form / proc-entry / header pins above.

### 6.36 Cross-unit reference-typed local (interface) mis-typed as an unrelated record (`CLOSED — live recovery names the runtime type`)

[DPT.Debugger.pas AutoDetectFormatterName / TryGetRecordTerminalName /
EvaluateVariable](DPT.Debugger.pas). A stack local whose declared type is a
*reference type from another unit* (an interface, or a cross-unit record) is
auto-detected as an unrelated record, because the one type byte the compact
`.rsm` stack-local form stores does not carry that cross-unit type — it
collides with a same-low-id record in the registry.

**Live repro (current `C:\MSE\TEST\Test.Lib.exe`, Jun-20 08:36).** In
`Test.Base.Collections.TestCLazyUniqueObjectList.AsEnumerable` (source line
213, `.map` RVA `$02186C2C`) the local `Lst: ILazyUniqueList<TObject>`
auto-detects as **`record TICONDIR`** (a Windows icon-directory struct). The
misroute is the **record-terminal fallback** `TryGetRecordTerminalName`
(`FindClassIdxByRsmTypeId(TypeIdx) → skRecord`), NOT `AutoDetectFormatterName`
Path 2 (whose `ClassLookup` only ever returns `'object'`, for `skClass`).
`type=object` → `Object @ <ptr>` (no class — `Lst` is an interface ref so
`[ptr]` is an IMT, not a VMT, and `ReadRuntimeClassName` fails); `type=int` →
the raw ref. Bare `evaluate Lst` is therefore **confidently wrong** — worse than
failing — and `Lst.<Field>` would navigate the bogus record into garbage.
Value-independent: at line 213 `Lst` is still `nil` (slot all-zero) yet the
record label still appears, proving the misroute is purely type-side. (The
gap only became observable after §6.39 — before that the BP mis-mapped to
`TestHtml.WriteLn`. Sibling `InternalList: IList<TObject>` is a separate
closure-capture decode gap, not this one.)

**Mechanism — the stack-local type byte is lossy for cross-unit reference
types (byte-level, this session, live build).** Two local encodings exist and
behave differently:

* **Register-resident** form `16 00 00 <lo> <hi>` carries the **full 2-byte**
  type id and resolves correctly. Verified: the closure body's `Self` and the
  outer `__frame` both decode `$0235` = exactly `AsEnumerable$ActRec`'s `$2A`
  id; `Result` = `$0229`. (The unit assigns its own types a *sequential*
  `$02xx` "page": `Add$ActRec`=`$0225`, `AsEnumerable$ActRec`=`$0235`, … +8.)
* **Stack** form `66 00 00 <type> <offset>` carries only **one type byte**.
  `Lst` = `66 00 00 FA F0`: type `$FA`, offset `$F0`→`−8` — and `−8` is
  correct (live, the `EBP−8` slot holds the real interface ref `$0FF6A0C6`,
  whose `[slot]` is IMT `$021973F8`).

That single byte `$FA` does **not** encode `Lst`'s declared type under any
scheme tested: it is not the type's full id (`ILazyUniqueList\`1`=`$F4B4`,
open spec `{Base.Collections}ILazyUniqueList<T>`=`$18CE`); not the low byte of
either (`$B4`/`$CE` ≠ `$FA`); no `$2A` entry with low byte `$FA` exists in this
unit's block and the `$02xx` page never reaches `$02FA`; the proc header
(`… C1 08 02 98 D4 B2 | 21 04 'Self'`) is a signature descriptor, byte-identical
across procs with different locals, NOT a per-proc local-type table. `$FA`
simply equals `TICONDIR`'s full id `$00FA`, so `FindClassIdxByRsmTypeId($00FA)`
returns `TICONDIR`. **The cross-unit type IS in the `.rsm`** — in the registry
(`ILazyUniqueList\`1`) and in the unit's `$64` import segment (token
`$1807F4B4`) — but the *stack local's one-byte reference to it* is insufficient
to reach it. This is the reduced `.rsm` being lossy for reference-typed stack
locals; the reg-resident and same-unit cases are fine. (Two compounding parser
limits, both real and independently worth fixing, though neither alone fixes
`Lst`: (1) `ScanTypeRegistry` admits only `T`/`P`-prefixed `$2A` names into
`FRsmTypeIdToClassIdx`, dropping every interface; (2) interfaces aren't
discovered as structs — `TRsmStructDiscoverer` knows only the VMT-class trailer
and the `$0E` record sentinel, and `TRsmStructKind` has no `skInterface`.)

**Refuted approach — live-slot validation alone (implemented + reverted this
session).** The obvious tier-1 — at the record-terminal fallback, deref the
slot and (a) re-route to `object` when `ReadRuntimeClassName` resolves a known
class, else (b) suppress the record when the slot "looks like a live
reference" (`P → VMT/IMT → first slot`, three readable pointer hops) — was
built and **reverted as unsafe**. The slot bytes are **byte-identical**
between (i) an interface/object reference mis-typed as a record and (ii) a
*genuine* record whose FIRST field is an object / interface / string
reference. Suppressing (ii) emits a confidently-wrong `object` for a real
record — the very failure class this gap is about — and no leakage guard can
separate the two from the slot alone, so the heuristic merely trades one wrong
answer for another. (The safe direction does hold and was verified live: a
scalar-first record like `GGlobalMixed` keeps `record TMixedRec` under the
heuristic — the hazard is purely the reference-first-field record.)

(§6.39 unblocked observability: before it, the `AsEnumerable` BP mis-mapped to
`TestHtml.WriteLn` so `Lst` wasn't even in scope. DebugTarget can't host a
*pinned* repro — its small registry never produces the id collision — so this
gap needs a live/MCP pin, like §6.38.)

**Working path forward — live recovery, gated by a LAYOUT discriminator (this
supersedes the "gateless / design limit" framing of earlier passes).** The
blind slot-validation above was correctly reverted because the slot bytes alone
can't separate an interface ref from a reference-first record. But we do not
have to look at the slot blind — we have the **resolved record's own member
layout** from `TRsmStructDiscoverer`, and the **live slot** is authoritative and
recoverable (proven: `Lst`→`$0FF6A0C6`, `[Lst]`→IMT `$021973F8`). The gate:

1. When `TryGetRecordTerminalName` resolves a record via a **page-truncated**
   id (`TypeIdx < $0100`, i.e. the high byte was dropped — the low-confidence
   case this gap is about), inspect that record's first members. `TICONDIR`
   begins with `idReserved: Word` / `idType: Word` at offset 0 — non-reference
   scalars. A live `TICONDIR` therefore starts with small word values; it
   **cannot** present as a valid double-indirect pointer.
2. Read the live slot. If it is a valid `P → [P]` double pointer (both into
   image/heap) while the resolved record's leading members are non-reference
   scalars, the record id is a **collision** → suppress it.
3. Recover via the §6.37 live-RTTI mechanism: an interface ref → its
   implementing object → the object's class RTTI + implemented-interfaces table
   → a real, collision-free name.

This is **principled** (resolved layout vs actual bytes), not the refuted blind
guess, and it directly removes the confidently-wrong `record TICONDIR` and the
`Lst.<Field>`-into-garbage hazard. Its honest limit: RTTI names the *runtime*
implementing class / interface, not necessarily the exact declared generic
`ILazyUniqueList<TObject>` — but that is a far better answer than a wrong record,
and a correct one. Needs a live/MCP pin (no standalone `TRsmReader` test).

**Static-route status — the two parser limits are now CLOSED (§4.8).** Both
independently-fixable limits this entry tracked are done: (1) `ScanTypeRegistry`
now admits `I<Upper>` `$2A` names and (2) synthesizes a discovered
`skInterface` `TRsmClassInfo` (the new `TRsmStructKind` ordinal) for each,
appended to `FClasses` / `FClassByName` and mapped into `FRsmTypeIdToClassIdx`
by id. So an interface is now a first-class discovered type (name + id, by-id
round-trip) for the no-process viewer and for class members typed as an
interface — pinned by `Test.DPT.Rsm.Reader.TestInterfaceDiscoveredAsStruct32/64`
(full §4.8 write-up). This does **NOT** fix `Lst`: the deeper blocker for it is the
one-byte stack-local type code itself (see mechanism above) — a reduced-format
loss, NOT proof the type is unrecoverable in principle (Delphi's own debugger
resolves it from richer DCU/TDS info). Leads already checked and refuted so
future passes don't redo them: a per-proc proc-header type table (the header is
a fixed signature descriptor, byte-identical across procs with different
locals); the `$ActRec` field route (the real interface id never sits adjacent
to an `Lst` field, and `$F4B4`/`$18CE` are reused 56/414× — not collision-free);
and a unit-`$02xx`-page alias for `$FA` (none exists; the page never reaches
`$02FA`). The remaining un-refuted static idea is recovering the dropped page
byte from the unit's `$64` import segment by scope (`ILazyUniqueList\`1` is
listed there, token `$1807F4B4`) — speculative, given the `$FA`↔`$B4` low-byte
mismatch, so the live path above is the primary route.

**IMPLEMENTED & live-verified — `evaluate Lst` now NAMES the runtime type.** The
layout-gated guard ships in
[DPT.Debugger.pas `EvaluateVariable` / `TryGetRecordTerminalName` /
`TryRecoverReferenceType` / `StrictObjectClassName`](DPT.Debugger.pas). At the
record-terminal fallback, for a LOCAL whose resolved record's first member is a
sub-pointer scalar (1-2 bytes at offset 0) yet whose live slot is a valid
double-indirect reference (`slot → [slot]`, both readable), the record is
suppressed and the live reference's runtime type recovered:

* `StrictObjectClassName` validates a candidate as a genuine object by the VMT
  self-pointer anchor (`[VMT-88]==VMT` Win32 / `-176` Win64) plus a
  `RsmIsPrintableAscii` name sanity check — strict enough to drive a scan.
* Direct object reference → named from the slot itself.
* Interface reference → the implementing object sits a small **pointer-aligned**
  distance before the (possibly unaligned) interface-field slot. Scan backward
  on the **target** pointer grid (`FTargetIs32Bit` → 4, else 8 — NOT the Win64
  debugger's `SizeOf(Pointer)`; and align first, since the slot can be
  unaligned) for the nearest valid VMT.
* If neither resolves, decline with a precise hint — never the bogus record.

Live-verified on `C:\MSE\TEST\Test.Lib.exe` at
`TestCLazyUniqueObjectList.AsEnumerable` line 222 (`Lst` assigned): bare
`evaluate Lst` now returns
`interface @ <p> (implemented by CLazyUniqueObjectList<System.TObject> @ <base>)`
**instead of** `record TICONDIR`. The interface field was at an *unaligned*
offset 22 inside the object (slot `…A0C6`, base `…A0B0`), which the aligned
target-grid scan handles. Regression-clean: full DptDebugger suite green both
platforms (Win64 232/232; Win32 exit-0, record-terminal and §6.36-B pins
included) — the guard never fires on a legitimate record (a real scalar-first
record can't present a valid double pointer where its leading bytes are a
1-2 byte scalar).

**Honest limit (by design, not a gap).** The recovery names the *runtime
implementing class* (`CLazyUniqueObjectList<System.TObject>`), not the local's
declared *interface* type (`ILazyUniqueList<TObject>`) — that static binding is
the truncated stack-local id, unrecoverable here (above). Naming the declared
interface would need an IMT→interface map from image RTTI; the implementing
class is the collision-free, useful answer and removes the navigation hazard.

> **Manifestation B (cross-unit record LOCAL dotted walk) is closed.**
> `Ad: TAdresse` → `Ad.Anschrift.Str` on Test.Lib (and its controlled
> repro `AdrLoc: DebugTarget.RecTypes.TXAdresse` in
> `RecordLocalNestedProbe`) needed three pieces, all now landed and
> documented elsewhere: (1) the §4.4 **Shape-C** offset decode — a
> cross-unit record local carries a 2-byte type alias (Hi byte not
> `$2E`/`$2F`) that pushes its BPREL offset to `+5`, so the local's
> *address* itself failed to decode (synthesized `-10000` sentinel);
> (2) the first-hop **record-local context bridge**
> (`FindRecordsByMemberName`) and (3) the nested-hop
> **inline-record bridge** (`FindRecordBySizeAndMemberName`), both folded
> into the §4.14 consumer note. Pinned by
> `TestMcpEvaluateCrossUnitRecordLocalDottedWalk` (live walk →
> "Hauptstr.", with `Name`/`Ort` nested-layout guards) +
> `TestCrossUnitRecordLocalOffsetAndLayout32` (the `-153` offset decode +
> record layout). The Test.Lib symptom there ("(enum) 7" auto-detect)
> was the same end-user failure reached by a different internal path —
> the address decoded but the type aliased to an enum; the bridges fix
> the navigation regardless of which way the bad id lands.
>
> One residual, documented as a **design limit in §4.14** (not reopened
> here): the no-`type=` auto-detect still cannot infer the terminal type
> of these record fields (the RSM carries no primitive id for them), so
> bare `evaluate AdrLoc.Anschrift.Str` falls back to `int` and bare
> `evaluate AdrLoc` honestly declines. The contract is to pass
> `type=shortstring` for record-field terminals; the navigation is what
> §6.36-B fixed.

### 6.37 Getter-backed / published property evaluation — CLOSED

Closed: `evaluate Obj.Caption`-style property reads now resolve, including on
large binaries. Mechanism in §4.16 ("Consumer note — the dotted walk
resolves properties two ways"): **live runtime RTTI** (VMT → `vmtTypeInfo`
→ published-property table → `GetProc` decode → field read or getter **call
injection**) is the primary, collision-proof path for published properties,
with the RSM `$31`/`$2C`/`$2E` records as the small-binary fallback for
non-published ones. Proven live on TFW (`Result.Caption` →
`001- Adresse suchen`, the form title the IDE shows) and pinned
deterministically by `TestMcpEvaluatePublishedPropertyViaLiveRtti`,
`TestMcpEvaluateGetterBackedPropertyViaCallInjection`,
`TestMcpEvaluateFieldBackedPropertyResolvesUnderlyingField`, and
`TestPropertyGetterNameRecovered32`.

The three-round consumer-side attribution investigation that preceded the
RTTI solution (member-name-set match; owner-from-getter; file-offset
proximity — all refuted at TFW scale because the RSM Format-A 2-byte id
cross-references collide in a 30k-class binary) is preserved in git history;
its conclusion (why RTTI, not RSM, is authoritative for published properties
at scale) is summarised in §4.16. The narrow residual once noted here — a
NON-published `public` getter property that the published-only walk missed —
was closed by §6.38 (the extended-RTTI `PropDataEx` walk added to
`TryResolveRttiProperty`); see §4.16 path 1. The only remaining limit is a
property emitted on NEITHER RTTI table (e.g. a `strict private` getter the
default `{$RTTI}` omits) on a huge binary, a narrow design limit documented in
§4.16, not a tracked gap.

### 6.39 `.rsm` proc `SegmentOffset` drifts below the true code RVA — CLOSED (consumer-side)

**Closed (live-verified).** The consumer `.map` PC→proc fallback
([DPT.Debugger.pas `MapCorrectedProcIdx`](DPT.Debugger.pas), wired into
`GetLocals`/`GetCurrentProcedureName`/`GetLocalAddress`) resolves the defect.
Verified live on `C:\MSE\TEST\Test.Lib.exe` (a build with a matching detailed
`.map`): at a BP in `TestCLazyUniqueObjectList.AsEnumerable`, `get_locals`
reports `procedure = TestCLazyUniqueObjectList.AsEnumerable` with its real
locals — **including `Lst`** (`evaluate Lst type=object` → `Object @ 101D67D6`,
matching the slot). The earlier session observed the opposite on the
then-current Test.Lib build — the same AsEnumerable PC resolved to
`TestHtml.WriteLn` with only `Self`/`msg`, `Lst` "Unknown variable" — which is
the mis-decode this fix targets. (Caveat: that was a different Test.Lib build,
so this is not a controlled A/B on one binary; the proof here is that the
consumer path is correct-by-construction — prefer the authoritative `.map` on
disagreement — and produces the right result live.) Regression-clean (full DPT
suite green; `TestBreakpointInTarget32` still passes — the override never fires
on correctly-decoded binaries). The mechanism is summarised in the §4.16-style
consumer note below; the underlying reader-level `.rsm` proc-address mis-decode
is NOT repaired (the address is simply absent from the container — see the
investigation log retained below) but is rendered harmless by the `.map`
fallback. Two operational notes surfaced while verifying: the target needs a
detailed `.map` next to it (no `.map` ⇒ `FMapScanner` nil ⇒ no fallback and no
line breakpoints — rebuild with `/p:DCC_MapFile=3`), and a raw-offset gap
sentinel was abandoned because Test.Lib rebuilds shift the layout. This also
unblocks the §6.36 observability (a `public`/interface local is now reachable
to inspect).

--- investigation log (retained) ---

[DPT.Rsm.Scanner.pas `HandleProcRecord` / `DecodeProcAddrPayload`](DPT.Rsm.Scanner.pas)
→ [DPT.Rsm.Reader.pas `FindProcContaining`](DPT.Rsm.Reader.pas) →
[DPT.Debugger.pas `GetLocals`](DPT.Debugger.pas). On `Test.Lib` (the §6.31/§6.36
corpus, committed under `RsmTaifunData`) the proc-address decode records a
`SegmentOffset` that sits **below** the proc's true code RVA, by a
**piecewise-constant** delta that steps up by region (see round-2 root cause
below) — so the live PC→proc lookup
(`SegmentOff := RIP − base − $1000`, fed to `FindProcContaining`) lands in the
wrong, lower-drifted **neighbouring** proc. This is what surfaced as the §6.36
observability blocker: a BP in `TestCLazyUniqueObjectList.AsEnumerable` makes
`get_locals` report `TestHtml.WriteLn`, hiding the in-scope locals.

Reproduced offline against the then-committed Test.Lib fixture (no live session)
for `TestCLazyUniqueObjectList.AsEnumerable` — a standalone `TRsmReader` probe
(`Test.DPT.Rsm.Taifun.TRsmTestLibTests`) showed `FindProcContaining` at the
proc's true `.map` offset returning the drifted neighbour. (The probe is not
kept as a committed test: it pinned raw offsets, which the later Test.Lib
rebuild invalidated — see the verification note at the end of this entry.)
Figures from that build:

| proc | `.map` RVA | `.rsm` SegmentOffset | delta |
|---|---|---|---|
| `TestCLazyUniqueObjectList.AsEnumerable` | `$021960AC` | `$2182648` (size `$3F8`) | **~$13A64** |
| `TestHtml.WriteLn` | `$021A78DC` | `$219304C` (size `$321C`) | **~$14890** |

`FindProcContaining($21950AC)` — the live offset for AsEnumerable
(`.map RVA $021960AC − $1000`) — returns **`TestHtml.WriteLn`**, not
AsEnumerable, exactly reproducing the live mismap. Note the `.rsm` table is
internally **consistent** (each proc round-trips on its own recorded
`SegmentOffset`), and the address did **not** fail to decode (it is non-zero) —
this is distinct from the closed §6.17 (`Size = 0` name-only entries). The two
deltas differ ($13A64 vs $14890), so it is **not** a constant skew: the decode
under-counts. Most procs/binaries are unaffected (TFW's locals + §6.38 worked).

**Root-caused (round 2) — the `$A0` form's fixed +7 address slot is wrong for
extended/forward `$A0` records.** Test.Lib is **Win32** (the `$28` records use
the `$A0` sub-tag; the `.pdata` segment has length 0 — the earlier "Win64"
label in §6.36 was wrong). Ground truth is the **EXE itself**: at the `.map`
RVA `$021970AC` (`= $1000 + segrel $021960AC`) the bytes are
`55 8B EC 33 C9 51 51 51 51 53 …` — a textbook Win32 Delphi prologue, i.e. the
real AsEnumerable. At the `.rsm`-decoded RVA `$2183648` the bytes are RTTI/data,
not code. So **the `.map` is correct and the `.rsm` decode is wrong**.

`DecodeProcAddrPayload`'s `$A0` branch reads the address DWORD at a **fixed
payload+7** (`TryWin32(AStartOff+7)` on Win32). Comparing two records (bytes
from the `$28` tag, payload begins after `$28 NL <name>`):

```
TSingleRec.InternalGetWords (CORRECT, segOff $982C):
  payload: A0 00 00 | 3A A4 7A 0C | C7 82 0A 04 | ...     ; +7 = C7 82 0A 04
           $040A82C7 >>4 - $401000 = $982C   ✓ address IS at +7
TestCLazyUniqueObjectList.AsEnumerable (WRONG, true segOff $021960AC):
  payload: A0 00 00 | 2B 67 6B 3A | 87 64 83 25 | C1 08 02 98 | D4 B2 21 04 | 'Self'…
           +7 = 87 64 83 25 -> $2583648 - $401000 = $2182648  (valid-looking but WRONG)
```

The true address DWORD (`(VA<<4)|7` for VA `$25970AC` = `C7 0A 97 25`) is
**absent** — not just from AsEnumerable's record but from the **entire 240 MB
`.rsm`** (round-3 full-buffer scan below). It is a closure-bearing method
(locals `Self/Enum/Item/Lst/__frame`, plus a separate
`…AsEnumerable$ActRec.$0$Body` proc), so its `$A0` record carries extended
metadata where +7 lands on a non-address field. The piecewise-constant
**plateaus** in the `.rsm`-vs-`.map` delta (0 at the start, then `$37420` over
a VCL run, `$dc6c` over a long ImageEn run, `$13094` over FMX, `$13a64` at the
Test region — identical across dozens of unrelated units, so not join noise)
are the signature of this: within a region the extended-`$A0` records share a
shape, so the +7 mis-read is consistently off; across regions the shape (and
the offset) changes.

**Round 3 — the address is NOT in the `.rsm` at all (decisive).** A survey of
every `$A0` proc record joined against the `.map` (unique `Class.Method` keys)
showed the +7 decode matches the true `.map` offset for **only 73 of 69,262**
procs — it is wrong for **~99.9%**, not a rare edge case. For every wrong proc
the true address is absent from a 64-byte payload window in all three
encodings (`(VA<<4)|7`, raw VA, raw segrel). A full-buffer scan of the 240 MB
`.rsm` for AsEnumerable's address (`C7 0A 97 25` and raw `AC 70 59 02`) returned
**0 hits**, while a control scan for a known-present address
(`TSingleRec.InternalGetWords` = `C7 82 0A 04`) returned **1 hit** — so the
scanner works and the address is genuinely **not stored**. Conclusion: for the
extended `$A0` form, `DecodeProcAddrPayload` reads a metadata DWORD at +7 that
*looks* like a valid VA (passes the `&$0F=7` + range checks) but isn't; the real
code address is simply not present in the container in any DWORD form. The 73
"correct" procs are the simple `$A0` records that genuinely carry the address at
+7.

**Fix — consumer-side `.map` fallback, NOT a decode tweak (IMPLEMENTED;
live-verification pending).** Since the address isn't in the `.rsm`, no decode
change can recover it. The debugger already loads the `.map` (`FMapScanner`) and
uses it authoritatively for breakpoints (`GetAddressFromUnitLine`). Implemented
in [DPT.Debugger.pas `MapCorrectedProcIdx`](DPT.Debugger.pas): after
`FindProcContaining`, resolve the proc the `.map` says is at the PC via
`FMapScanner.ProcNameFromAddr(SegOff)` (ProcNames[].VA and the line table share
one VA space, and the BP resolver uses `base + $1000 + LineInfo.VA`, so
`SegOff = RIP - base - $1000` queries it in the right space by construction);
keep `FindProcContaining` when its name already suffix-matches the `.map` name
(good binaries — no-op, no overload risk), otherwise bridge the `.map`
`Unit.Class.Method` to the `.rsm` `Class.Method` by stripping leading dotted
segments until `FindProcByName` hits, and read that proc's locals (whose
`BpOffset`s are independent of the broken code-address decode). The helper also
returns the **`.map`-corrected proc-start offset** (`SegOff − ProcNameFromAddr`'s
offset out-param) so the x86 prologue reads (`ProcUsesEbpFrame` /
`TryFindRegParamSpillDisp` in `GetLocals`/`GetLocalAddress`) hit real code, not
the drifted address. Wired into all three consumers. Regression-clean: the full
DPT suite stays green (DebugTarget's `.rsm` decodes fine, so the override never
fires there).

**Live verification still pending.** It is consumer-only and live-only, so it
needs a live session at a mis-decoded proc. This was **not** confirmable this
turn: the `C:\MSE\TEST` Test.Lib was rebuilt mid-investigation (the prior build
was TestInsight-conditional), and on the rebuilt binary **no breakpoint in
`Test.Base.Collections` fires** at all — `AsEnumerable` lines 220/222/227/228
AND the always-executed standalone `CountItems` (151/154) resolve to `active`
addresses yet never trigger, though the test runs and passes. (BPs fire normally
on DebugTarget — `TestBreakpointInTarget32` passes — so this is a property of
the rebuilt Test.Lib, not the debugger.) A prior raw-offset gap sentinel was
removed: the rebuild shifted the layout, so its hardcoded `$21950AC` started
resolving to an unrelated proc — exactly the raw-offset fragility the skill
warns against. Closure needs a stable Test.Lib build where line breakpoints fire
(matching `-V -VR` `.rsm` + detailed `.map`, non-TestInsight); then confirm
`get_locals`/`evaluate Lst` at `AsEnumerable` resolve correctly.

### 6.41 Pointer-to-cross-unit-record `var`-block LOCAL: alias TypeIdx unbound to the record layout — CLOSED (consumer-side `T<localName>` bridge)

[DPT.Debugger.pas `EvaluateVariable`](DPT.Debugger.pas) (dotted-walk
first-hop priming). Surfaced
by the live realtest after §6.40 closed: in TFW's `CKonsMisDict.DataLoad`
the local `KonsMis: PKonsMis` (`PKonsMis = ^TKonsMis`, `TKonsMis` a
packed record in the sibling unit `Tfw.Kons.Mis.Typ`) now decodes to the
**correct** base pointer — `get_locals` → `bp_offset -12`,
`evaluate KonsMis (int)` → `0xF2498660` (the live record), ground-truth
`Name` at `+0x49` = the ShortString `Volltextsuche` (verified by
`read_memory`). **But before the fix below, every field hop failed**:
`evaluate KonsMis.Name` and even `evaluate KonsMis.Typ` both returned
`Could not navigate`, and `evaluate KonsMis (object)` returned the
type-less `Object @ F2498660` (no class/record name).

**Mechanism (confirmed via the standalone-reader disambiguator,
`TestKonsMisLocalRecordBindingGapPinned`):** the record itself is **not**
the problem — `FindClassByName('TKonsMis')` resolves a **unique**
`skRecord` with the full layout (`Typ` at `0x44` size 1, `Name` at `0x49`
size 41, 29 members). The break is purely the local→record **binding**:

* The scanner records `KonsMis.TypeIdx = $02E5` — the per-proc 2-byte
  alias (§4.4 Shape C), **not** the registry primary (the §4.2 design
  limit). `FindClassIdxByRsmTypeId($02E5)` resolves to the **unrelated**
  record `TLogSharing` (a §6.18-style alias collision), and
  `FindStructByTypeIdx($02E5)` / `IsRecordTypeIdx($02E5)` find nothing.
  So the dotted-walk first-hop priming
  ([DPT.Debugger.pas `EvaluateVariable`](DPT.Debugger.pas)) primes the
  **wrong** record and the `Name`/`Typ` member lookup fails.
* The §6.32 VMT-priority override (which rescues *class*-typed locals
  whose alias collides) cannot help here: a **record** local has no VMT
  to read a runtime type name from, so `SkipRecordPriming` stays False
  and the bad alias resolution stands.
* The §6.36 field-name fallback (`FindRecordsByMemberName(Segments[1])`,
  accept iff exactly one record matches) is defeated by ambiguity:
  **321** records carry `Name`, **190** carry `Typ`, **29** carry both —
  never a unique match. And the `P<X>` type name is itself absent
  (`FindClassByName('PKonsMis') = -1`).

This is the **record-typed-`var`-block-local manifestation of the §4.2
per-proc-ref design limit**: the per-proc alias is structurally
unresolvable to the registry primary, so the local→record binding has to
come from a different signal.

**Closure — the consumer-side `T<localName>` record bridge.** The reader
side is left as-is (the §4.2 alias remains a per-proc ref; this is a
design limit, not a decodable gap). The dotted-walk first-hop priming
([DPT.Debugger.pas `EvaluateVariable`](DPT.Debugger.pas)) now recovers the
record by the `P<X>`/`T<X>` naming convention — a local named `X` of type
`PX` (`= ^TX`) binds to the record `TX` — the same family as the §6.19
field bridge and the §6.24 enum bridge. Two guarded steps:

1. **Discard a mis-resolved alias.** After `FindClassIdxByRsmTypeId`, if
   the result is an `skRecord` that does **not** declare the accessed
   field `Segments[1]`, it is the §6.18 collision (`$02E5 → TLogSharing`,
   which has no `Name`) — reset `GStructIdx := -1` so the recovery routes
   below run. (Without this the bad alias short-circuited every fallback.)
2. **`T<localName>` bridge**, tried before the §6.36 field-name fallback
   because it is far less ambiguous (one `TKonsMis` vs. 321 records
   carrying `Name`): bind to `FindClassByName('T' + Segments[0])` iff it
   is a discovered `skRecord` that declares `Segments[1]`. The existing
   live-pointer-deref probe (the slot must point at a region the whole
   record reads back from) is the structural cross-check; class-typed
   locals never reach here (the §6.32 VMT override set
   `SkipRecordPriming`).

Verified live against `C:\MSE-26.05-Mongo\TFW\TFW.exe` under the MCP
debugger: `evaluate KonsMis.Name` (`type=shortstring`) → **`Volltextsuche`**
and `evaluate KonsMis.Typ` → **`14`**, where both returned
`Could not navigate` before. Pinned (reader-side preconditions the bridge
relies on) by
[`Test.DPT.Rsm.Taifun.TRsmTfwTests.TestKonsMisLocalRecordBindingGapPinned`](../Test/Test.DPT.Rsm.Taifun.pas);
the dotted-walk path itself is exercised live (no DebugTarget repro — the
small fixture's field names are unambiguous, so its §6.36 fallback already
binds and never reaches the bridge). **Residual:** bare
`evaluate KonsMis.Name` (no `type=`) auto-types the `S_40` ShortString
member as `int` (`0x6C6F560D` = the `0D 56 6F 6C` length+chars) because the
record member carries no `PrimitiveTypeId` — the navigation is correct,
only the auto-formatter guess is off. That is the §6.24-family
record-member-primitive-type binding, tracked separately; `type=shortstring`
is the workaround.

### 6.42 Pointer-to-record `var`-block LOCAL: GLOBAL-republished alias re-collides without the §6.41 discard — CLOSED (consumer-side)

[DPT.Debugger.Evaluate.pas `Evaluate`](DPT.Debugger.Evaluate.pas)
(dotted-walk first-hop priming). Surfaced by the realtest on
`C:\MSE-26.05-Mongo\TFW\TFW.exe`: `CComputerDict.DataLoad` has the local
`Computer: PComputer` (`PComputer = ^TComputer`), and **even the first
hop** `evaluate Computer.Name` / `Computer.OS` returned `Could not
navigate`, although the §6.41 `T<localName>` bridge should bind
`Computer → TComputer` exactly as it binds `KonsMis → TKonsMis`.

**Mechanism (confirmed via `TestComputerLocalRecordBridgePreconditions`).**
The §6.41 closure applied its "does the resolved record declare the
accessed field?" discard guard only to the **`FirstLocalTypeIdx`**
resolution. But the dotted-walk recovery has a *second* registry
resolution right after it: a `$20` stack-local is also republished into
the global map (§4.4), so the recovery calls
`FindGlobalTypeIdx(Segments[0]) → FindClassIdxByRsmTypeId(...)`. For
`Computer` this returns a **non-zero** id (`$F822`) that
`FindClassIdxByRsmTypeId` maps to the unrelated record `TKlkKons` (which
declares neither `OS` nor `Name`) — a §6.18-style alias collision, but on
the GLOBAL id rather than the local one. With no discard guard there, the
colliding record (`GStructIdx ≥ 0`) **pre-empted both** the
`FindBestRecordForGlobalAndField` proximity/`T<global>` recovery (which
resolves the correct `TComputer`) AND the §6.41 `T<localName>` bridge
(both gated on `GStructIdx < 0`), so the walk primed `TKlkKons` and every
field hop failed. `KonsMis` escaped the bug only by luck: its
republished id resolves to nothing (`-1`), so its recovery proceeded.

**Closure.** Re-apply the §6.41 discard to the global-resolved alias: a
record alias that does not declare `Segments[1]` is dropped
(`GStructIdx := -1`) so the recoveries run. Verified live:
`evaluate Computer.Name (shortstring)` → `NB0374`,
`evaluate Computer.OS.MajorVersion (int)` → `10`,
`evaluate Computer.CPU.Name (shortstring)` → the CPU model — all
previously `Could not navigate`. Pinned (reader-level preconditions) by
[`Test.DPT.Rsm.Taifun.TRsmTfwTests.TestComputerLocalRecordBridgePreconditions`](../Test/Test.DPT.Rsm.Taifun.pas).

### 6.43 Nested-record member re-prime fails on ambiguous leaf names — CLOSED (consumer-side `T<outerStem><member>` convention)

[DPT.Debugger.Evaluate.pas `Evaluate`](DPT.Debugger.Evaluate.pas)
(dotted-walk §6.36 nested-record bridge). After §6.42 unblocked the first
hop, `evaluate Computer.OS.Name` (`type=shortstring`) **still** returned
`Could not navigate` while `evaluate Computer.OS.MajorVersion` succeeded —
two leaves of the *same* `TComputerOS` sub-record, differing only in the
leaf name.

**Mechanism (confirmed via the same pin's §6.43 assertions).** A
record-typed member of a record (`TComputer.OS : TComputerOS`) carries
`TypeIdx = 0` (§4.14 record-field-id-is-zero), and its only other type id
(`PrimitiveTypeId = $C94E`) keys **neither** the registry
(`FindClassIdxByRsmTypeId = -1`) **nor** a struct file-offset
(`FindStructByTypeIdx = -1`) — the §4.2 design-limit family. So the
nested re-prime cannot resolve the sub-record by id and falls to the
§6.36 `FindRecordBySizeAndMemberName(Member.Size, Segments[I+1])` bridge,
which disambiguates same-size records **by the next segment's field
name**. That is unique for a rare leaf (`MajorVersion` → `TComputerOS`)
but **ambiguous for a common one** — 321 TFW records declare `Name`, so
`FindRecordBySizeAndMemberName(314, 'Name') = -1`, the re-prime fails, and
the leaf can't be reached. (`Computer.CPU.Name` worked only because CPU's
size+`Name` happened to be unique; `Computer.GPU.Name` failed for the
same reason as OS.)

**Closure.** When the size+next-field bridge is ambiguous (`-1`),
disambiguate by the nested-record NAME convention: a member `<M>` on
record `T<Stem>` whose type is the sibling record `T<Stem><M>`
(`TComputer.OS : TComputerOS`, `.CPU : TComputerCPU`, `.GPU :
TComputerGPU`). Strictly guarded — the outer name must start with `T`,
and the candidate must be a discovered `skRecord` whose layout extent
fits the member's parent slot (the same `<8`-byte alignment tolerance
`FindRecordBySizeAndMemberName` uses) AND declares the next segment — so a
convention miss can never mis-prime. Verified live:
`evaluate Computer.OS.Name` → **`Windows 11`**,
`Computer.GPU.Name` → `Intel(R) UHD Graphics`,
`Computer.OS.BuildNumber` → `26200`, plus `DB`/`Memory`/`CPU` leaves —
all cross-checked against `read_memory`. **Residual:** bare
`evaluate Computer.OS.Name` (no `type=`) still fails because the record
member carries no `PrimitiveTypeId` for the auto-formatter — the same
§6.24-family record-member-primitive-type-binding residual noted under
§6.41; `type=shortstring` is the workaround. Pinned by the §6.43
assertions in
[`Test.DPT.Rsm.Taifun.TRsmTfwTests.TestComputerLocalRecordBridgePreconditions`](../Test/Test.DPT.Rsm.Taifun.pas).

### 6.44 Nested SHARED-record member unreachable by `T<outerStem><member>` — CLOSED (consumer-side `T<member>` convention)

[DPT.Debugger.Evaluate.pas `Evaluate`](DPT.Debugger.Evaluate.pas)
(dotted-walk §6.36 nested-record bridge). The §6.43 convention closes the
case where a nested sub-record is *namespaced under its owner*
(`TComputer.OS : TComputerOS`). But a record just as often embeds a
**shared** record whose name is **not** built from the owner — the
canonical case is a record header: `TUser.RecHeader : TRecHeader`,
`TUser.RecChanges : TRecChanges` (the same `TRecHeader` is reused by
`TComputer`, `TKonsMis`, hundreds of records). Surfaced by the realtest:
`evaluate User.RecHeader.Version` (`User: PUser`, BP
`Base.User.Dict:488`) returned `Could not navigate`, even though every
direct `User.<field>` resolved (§6.42 first hop) and the ground-truth
`Version` (`0x018D7DD0`) sat at the record base.

**Mechanism.** Same root as §6.43 — `RecHeader` carries `TypeIdx = 0`
(§4.14) and an unresolvable type id, so the nested re-prime falls to the
§6.36 size+next-field bridge, which is ambiguous for the common leaf
`Version`. The §6.43 `T<outerStem><member>` candidate misses too:
`'T' + 'User' + 'RecHeader' = 'TUserRecHeader'` does **not** exist
(the type is the shared `TRecHeader`, not a `TUser`-namespaced sibling).

**Closure.** Add a SECOND name-convention candidate to the same bridge:
`T<member>` — a member `<M>` whose type is the record `T<M>`
(`RecHeader → TRecHeader`, `RecChanges → TRecChanges`), the same `T<X>`
family as §6.19/§6.41 applied to a nested member. Both candidates are
tried in order of specificity (`T<outerStem><member>` first, so a
namespaced sub-record still wins when both resolve), each under the
identical guard: a discovered `skRecord` whose layout extent fits the
member's parent slot (`<8`-byte alignment tolerance) AND declares the
next segment — so a convention miss can never mis-prime. Verified live:
`evaluate User.RecHeader.Version (int)` → **`26050000 (0x018D7DD0)`**,
byte-matching `read_memory` at the record base. Reproduced and pinned in
the controlled fixture (a SHARED-header record reached by `T<member>`,
with a same-extent decoy that also declares `Version` to force the
size+name bridge ambiguous, in its OWN compiland so the §6.37
getter-name proximity window stays stable) by
[`Test.DPT.MCP.Server.TMcpServerTests.TestMcpEvaluateSharedHeaderRecordViaTMemberConvention`](../Test/Test.DPT.MCP.Server.pas)
(RED before this change with the exact `Could not navigate` string, GREEN
after). The §6.43 residual applies unchanged: bare `evaluate` (no
`type=`) of a record member still hits the §6.24-family auto-formatter
gap; `type=int` is the workaround.

### 6.45 The `.rsm` cannot carry the cross-unit nested-record member→type edge — it lives in TD32/`.tds`, read by `DPT.Td32.Reader` (`DESIGN LIMIT` + external resolution)

The §6.36/§6.41/§6.43/§6.44 closures are all NAME conventions
(`T<localName>`, `T<outerStem><member>`, `T<member>`) plus the §6.36
size+next-field bridge. They take `.rsm`-only nested-record resolution to
its **ceiling**, but they are heuristics that fail on any project whose
types are not named by the `T<X>` convention. A multi-round byte-level
investigation established **why** no convention-free `.rsm` decode exists
and **where** the authoritative edge actually lives.

**The `.rsm` (`$2C`) genuinely does not encode the cross-unit member→type
edge.** The discriminator is the sub-record's `$2A` body flag (§4.8):
* **Same-compilation records (`$20`/`$00`):** the `$2C` field-id at
  `After+3..+4` **equals** the sub-record's `$2A` registry primary, so the
  existing `LinkFieldsFromFormatA` → `FindClassIdxForRawId` path resolves
  `Member.TypeIdx` directly (DebugTarget `TWithHeader.WhdrHeader` → `$2E85`
  = `TWhdrHeader`, verified live).
* **Cross-unit / precompiled-DCU records (`$A8`/`$88`):** the `$2C`
  field-id is a **parent-relative POSITIONAL value**, not the registry
  primary, so `Member.TypeIdx` stays 0. TFW byte-proof: `TComputer.OS`
  field-id `$C94E` ≠ `TComputerOS` primary `$AE30`; the SAME target type
  `TRecHeader` yields a *different* field-id per parent
  (`TComputer.RecHeader=$2A`, `TUser.RecHeader=$3C`) — a position, not an
  identity. `FindStructByTypeIdx(0)` then mis-lands on whatever record
  carries `TypeIdx=0`.

**Leads refuted (do NOT re-walk — all byte-checked on TFW.rsm):**
* global low-byte + nearest-`$2A`-offset decode: **0/18** (the `+3` low
  byte is a positional field index; the post-`$9C01` 2-byte value is a
  per-unit record counter `base+4·index`; neither is a type id);
* the parent record's own `$2A` body does **not** enumerate its members;
* the `$66` import-ref 4-byte LinkToken (§4.17) is name-keyed and
  **absent** from the member's `$2C` body;
* unit-scoping narrows but does not resolve: same-unit
  declaration-adjacency + slot-size is only 4/6 (intra-unit size
  collisions); cross-unit `$66` membership confirms reachability but
  cannot pick *which* member binds to a type;
* there is **no per-unit numeric id** (the `$70` 4-byte token is the
  build-wide link timestamp `$5CD96D42`, identical across units) and **no
  `$35` import-by-id table** — the uses-block is purely the §4.17
  `$64`/`$66`/`$67` name-keyed import list.

Consistent with the §4.2 per-proc-ref limit and §6.34: the `.rsm` is a
**reduced** sidecar. The Delphi IDE resolves these member types from its
**local DCU/type database** (it compiled the project) and uses the `.rsm`
only for the address↔symbol mapping of the remote process — context a
`.rsm`+`.exe`-only tool like DPT does not have.

**Where the edge lives + the resolution.** The authoritative member→type
graph is the **TD32/CodeView** debug info (`LF_STRUCTURE`/`LF_CLASS`/
`LF_FIELDLIST`/`LF_MEMBER`, each member carrying a real 16-bit
type-index). dcc32 emits it embedded in the EXE (`-V`) **or** as a
detachable `<project>.tds` sidecar (`-VT` — a Delphi output, despite
docwiki labelling `-VT` "C++ only"); `-VR` (the `.rsm`) is independent and
coexists. [`DPT.Td32.Reader.TTd32Reader`](DPT.Td32.Reader.pas) (recovered
from history; the TD32 peer of `TRsmReader`, same
`FindClassByName`/`FindClassMember`/`FindStructByTypeIdx` API) parses both
the embedded stream (`LoadFromFile`, FB09 scan from offset 256) and a
standalone `.tds` (`LoadFromTdsFile`, scan from 0) and resolves the
cross-unit edge the `.rsm` cannot. Proven on DebugTarget AND on the real
**357 MB `TFW.tds`** (`-VT` build): `TComputer.OS → TComputerOS`,
`TUser.RecHeader → TRecHeader`, load ~0.6 s on Win64 (no OOM); the `.tds`
(357 MB) is smaller than the `.rsm` (811 MB) and ~38× faster to load.
Pinned by [`Test.DPT.Td32.Reader`](../Test/Test.DPT.Td32.Reader.pas)
(embedded + standalone `.tds`, RSM head-to-head) and
[`Test.DPT.Td32.Tfw`](../Test/Test.DPT.Td32.Tfw.pas) (the TFW-scale gate).

**Status:** the §6.43/§6.44 name conventions remain the best-effort
**fallback** for `.rsm`-only targets. For builds that emit TD32 (`-V` or
`-VT`), `DPT.Td32.Reader` is the convention-free, project-independent
source. Open follow-up (NOT a `.rsm` gap): wire `DPT.Td32.Reader` into the
live evaluate path as the preferred reader, `.rsm` as fallback.

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
   named after a VMT `skClass`, following a non-enum naming convention,
   sharing a `$03` def's `(name, unit)`, or fully covered by a single
   authoritative `$03` enum — §6.25. Runs **first** because the enum bridges below
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
| `SourceFiles`                        | `HandleSourceFileIntroRecord` (§4.17)| (index, deduped by unit name)        | `TRsmSourceFile { SourceFile, UnitName, StartOffset, LinkToken }` — the importing unit a segment's `SourceFileIdx` points at |

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
