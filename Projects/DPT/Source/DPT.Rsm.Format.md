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
  collection. See [DPT.Rsm.Model.pas:200](DPT.Rsm.Model.pas#L200) and
  [DPT.Rsm.Scanner.pas:328](DPT.Rsm.Scanner.pas#L328).
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
`TRsmCrossUnitParentResolver.Run` → `TRsmScopeLocalEnumBridge.Run`).

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
[DPT.Rsm.Scanner.pas:560-566](DPT.Rsm.Scanner.pas#L560-L566) for
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
  ([DPT.Rsm.Scanner.pas:651-665](DPT.Rsm.Scanner.pas#L651-L665))
* `TRsmScanner.HandleEnumConstantRecord` sparse-ordinal form
  ([DPT.Rsm.Scanner.pas:885-896](DPT.Rsm.Scanner.pas#L885-L896))

---

## 3. Record taxonomy

Tag constants live in `TRsmTag` in
[DPT.Rsm.Model.pas:159-201](DPT.Rsm.Model.pas#L159-L201).

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
| `$63`  | `SCOPE_END`         | Closes the current proc scope (only effective after at least one local-shaped record has been seen)     |

`$28 PROC_TAG`, `$25`, `$03`, `$2A` and `$2C` are valid both inside and
outside a proc scope. The `$22`, `$21`, `$20` (local-form), and
`$27` paths gate on the scanner's `FScanInProc` state.

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
([DPT.Rsm.Scanner.pas:342-456](DPT.Rsm.Scanner.pas#L342-L456)). Dispatch
happens on the byte at `name+0`:

| Sub-tag | Decoder        | Notes                                                                       |
|---------|----------------|-----------------------------------------------------------------------------|
| `$20`   | `TryWin32(name+3)` then `TryWin64(name+3)` | Simple inline form                                                          |
| `$A0`   | `TryWin32(name+7)` only                    | Extended form with type-ref / timestamp metadata                            |
| `$41`   | `TryWin32(name+4)` only                    | Method-record extended form found in large binaries (`41 02 10 00` header) |
| `$80`, `$00`, ... | None              | Forward declaration / cross-reference, no embedded address                  |

**Win32 address encoding** (`TryWin32`, [line 375-397](DPT.Rsm.Scanner.pas#L375-L397)):

```
4 bytes: DWORD = (VA shl 4) or $07
```

The low nibble of byte 0 must be `$07`. The recovered RVA is
`(DWORD >> 4) - $401000` (`$400000` image base + `$1000` `.text` RVA).
Sanity range: `(0, $10000000)` — i.e. up to 256 MB of code.

**Win64 address encoding** (`TryWin64`, [line 399-425](DPT.Rsm.Scanner.pas#L399-L425)):

```
byte 0: (byte0 and $7F) must be $03         // encoding-kind tag
        byte0 bit 7 = VA bit 4
byte 1: VA bits 5..12  (full 8 bits)
byte 2: VA bits 13..20 (full 8 bits)
byte 3,4: encoded proc-size / local-layout info (not used for address)
```

A trailer `04 10 ?? 2E 00` must follow within the next 1-2 bytes, where
`??` is a per-binary counter that varies build-to-build (linker
allocates it from the overall RSM layout, not from the proc itself).

The recovered VA is masked to 21 bits (`$1FFFFF`), so the encoding caps
out at ~2 MB of code per binary for the bits-0..2 alone. **UNCERTAIN /
GAP**: larger binaries would have to draw additional high VA bits from
bytes 3/4, which the current decoder ignores. The comment at
[line 363-373](DPT.Rsm.Scanner.pas#L363-L373) acknowledges this.

**Duplicate-name handling**: the first `$28` for a name creates the proc
with whatever `Decoded` address (possibly 0). A later `$28` with the
same name and `Decoded > 0` patches the existing entry. This handles
the forward-declaration-then-definition pattern.

**Scope side-effect**: a successful `$28` flips `FScanInProc := True`,
resets `FScanLocalIdx` and `FScanRegParam` to 0, and clears
`FScanSeenLocalSinceProc`.

### 4.2 `$22` PARAM_TAG — stack / open-array parameter

```
$22  <NL: u8>  <Name>  $62 $00 $00  <typeId-lo: u8>  <typeId-hi: u8>
```

`NL` ∈ `[1, 64]`. The 3-byte anchor `$62 $00 $00` confirms the shape;
without it the record is rejected.

Type id decode: if `Hi == $2E` or `Hi == $2F`, read as a 2-byte LE id;
otherwise read the single byte at +3 as a primitive id.

Open-array parameters use **two register slots** (pointer + high-index).
After consuming the param body the scanner peeks the next 2 bytes; if
they start with `$20 $21` (a hidden high-index sub-record), it
increments `FScanRegParam` once more so subsequent parameters retain
correct register indices. See
[DPT.Rsm.Scanner.pas:578-583](DPT.Rsm.Scanner.pas#L578-L583).

Stored as `TRsmLocal` with `Kind = lkRegister`, `RegParamIdx =
FScanRegParam`.

### 4.3 `$21` REGVAR_TAG — register-passed variable

```
$21  <NL: u8>  <Name>  $66 $00 $00  <typeId-lo>  <typeId-hi>
```

Identical layout to `$22` except the anchor is `$66 $00 $00`. Used for
method-call `Self` / `AOwner` / etc. that ride EAX/EDX/ECX (x86) or
RCX/RDX/R8/R9 (Win64).

### 4.4 `$20` LOCAL_TAG — stack local OR module-level global

Two forms sharing the same tag byte:

#### Stack-local form (when `FScanInProc` is True)

```
$20  <NL: u8>  <Name>  <typeinfo + BPRel-offset payload>
```

Decoded by `HandleLocalRecord`
([DPT.Rsm.Scanner.pas:627-713](DPT.Rsm.Scanner.pas#L627-L713)). The
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
([Scanner.pas:703-710](DPT.Rsm.Scanner.pas#L703-L710)), because the
`FScanInProc` gate cannot reliably distinguish a stack local from a
module-level variable in every code path.

#### Module-global form (when `FScanInProc` is False)

```
$20  <NL: u8>  <Name>  $66 $00 $00  <id-lo>  <id-hi>  <VA: 4 bytes>
```

Decoded by `HandleModuleGlobalLocalTagRecord`
([Scanner.pas:731-770](DPT.Rsm.Scanner.pas#L731-L770)). `NL` ∈ `[1, 40]`.
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
[Scanner.pas:719-748](DPT.Rsm.Scanner.pas#L719-L748).

### 4.5 `$27` GLOBAL_PRIM_TAG — top-level primitive global

```
$27  <NL: u8>  <Name>  $66 $00 $00  <id-lo>  [<id-hi>]  <VA: 4 bytes>
```

Decoded by `HandleGlobalPrimRecord`
([Scanner.pas:792-846](DPT.Rsm.Scanner.pas#L792-L846)). `NL` ∈ `[1, 40]`.
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
comment at [Scanner.pas:746-756](DPT.Rsm.Scanner.pas#L746-L756) explains
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
$8A $00 $00  <RVA:4>  <typeId-lo>  <typeId-hi != $00>  $00 $00  <2*ordinal: u8>
```

Anchor: `$8A $00 $00` opens the body, `typeId-hi` is non-zero (real
cross-unit primary, e.g. `$04` for `TThreadPriority` → id `$0441`).

The 4-byte RVA at offset +3..+6 is captured but currently unused
(`RecordCrossUnitRtlConstant` stores `ARecordPos` only — see
[EnumDecoder.pas:196-204](DPT.Rsm.EnumDecoder.pas#L196-L204)).

Ordinal is `body[11] >> 1`. Reaches `RecordCrossUnitRtlConstant`.

#### 4.6.3 Same-compilation cross-unit, single-byte ordinal (11-byte body)

```
$25  <NL>  <Name>
$8A $00 $00  <RVA:4>  <secId-lo>  $00 $00 $00  <2*ordinal: u8>
```

`typeId-hi == 0` AND `body[10]` LSB == 0. Ordinal = `body[10] >> 1`.

The secondary id is `body[7]` (single byte); the `$00`s at +8, +9, +10
form the padding before the ordinal.

Reaches `RecordCrossUnitSameCompConstant(secId, ordinal, name, pos)` —
constants are **buffered** until the matching `$2A` registry entry
arrives (because the secondary collides across sibling-unit enums).

#### 4.6.4 Same-compilation cross-unit, sparse / 2-byte ordinal (12-byte body)

```
$25  <NL>  <Name>
$8A $00 $00  <RVA:4>  <secId-lo>  $00 $00 $00  <ord-byte-lo>  <ord-byte-hi>
```

`typeId-hi == 0` AND `body[10]` LSB == 1.

The ordinal uses Delphi's LSB-as-continuation form:
`W = body[10] | (body[11] << 8); ordinal = (W - 1) >> 2`.

Also reaches `RecordCrossUnitSameCompConstant`. Used for enums with
explicit ordinal values like `(a = 1, b = 100)` or sparse `(a = 128, ...)`.

### 4.7 `$03` ENUM_DEF_TAG — enum type definition

```
$03  <NL: u8>  <TypeName>
$01 $00 $00 $00 $00  <MaxOrd: u8>
$00 $00 $00 $00 $00 $00 $00         (12-byte header total)
( <ElemLen: u8>  <ElemName> ) * (MaxOrd + 1)
<UnitLen: u8>  <UnitName>
```

`NL` ∈ `[2, 40]`. The 12-byte header following the name is anchored on
the `$01` at +0 and the run of zero bytes at +1..+4 and +6..+12; byte +5
is the max-ordinal value.

Element count is capped at 512 to defend against coincidental `$03`
byte hits. Each element name is a `[1, 64]` length-prefixed identifier.
The unit name follows immediately after the last element.

**The dispatcher does NOT advance `P` past the body** (see comment at
[Scanner.pas:920-928](DPT.Rsm.Scanner.pas#L920-L928)). The single-byte
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
`(typeId, ordinal)` via `TryGetEnumConstantName` keep working. See
§6.1 for the residual implication.

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
* **`$20` — wide body**: at +5 begins a non-zero 4-byte block (LE
  word at +5/+6 + LE word at +7/+8) whose meaning is only partially
  understood. All program-local classes
  (`TInner`/`TDerived`/`TDeepDerived`/`TClassFieldHost`/...) and a
  subset of records (`TPoint2D`, `TRect2D`, `TPair`, `TMixedRec`,
  `TWhdrHeader`, `TWithHeader`, `TPrimitives`, ...) use this form.
  The word at +7/+8 is what the scanner reads as `SecCandidate`; for
  the wide form it carries a legitimate value (often `$04DB` in
  DebugTarget), but only enum-bridge consumers gated by
  `FCrossUnitEnumIds` end up using it (see §6.6 for the remaining
  uncertainty).
* **`$08`, `$80`, `$88`, `$98`, `$A8`, `$B8`** — observed on cross-unit
  RTL entries (`TObject`, `TGUID`, `TVisibilityClass`, `TThreadID`,
  ...); body shape is undocumented. See §6.6.

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
pending): the scanner walks up to 1024 bytes forward looking for a
`PROC_TAG` record whose name contains a dot
(`'DebugTarget.EnumAlpha'`). That dotted proc name is the unit-init
proc; its identifier IS the owning unit's name. This is the only known
way to recover the `(unit, type)` pair for synthesized `EnumDef`s in
the same-comp case. See
[Scanner.pas:1033-1066](DPT.Rsm.Scanner.pas#L1033-L1066).

The dispatcher does not advance `P` past the body (single-byte fallback
re-walks; harmless under tight shape checks).

`TRsmEnumDecoder.RecordTypeRegistry` then runs three jobs:

1. Bridge `primary → secondary` alias list — but only when the
   secondary candidate has been seen in a prior cross-unit `$25` record
   (filter via `FCrossUnitEnumIds`).
2. Flush every pending same-comp $25 constant under the primary id, so
   `FEnumConstNames[primary:ordinal] := name`.
3. Synthesise an `EnumDef` from the pending buffer when the forward-scan
   recovered an owning unit name.

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
  [DPT.Rsm.FormatALinker.pas:243-429](DPT.Rsm.FormatALinker.pas#L243-L429).
  This bridge is critical for the TFW corpus where unit-local id `$44`
  is `TUserKonsOutlook` in one unit and an unrelated class in another.

#### Body shapes — field type information

`BodyLen = EndOff - After` (i.e. distance from the anchor to the
terminator). The linker discriminates four body shapes
([FormatALinker.pas:648-704](DPT.Rsm.FormatALinker.pas#L648-L704)):

| Body shape           | Description                                                                          |
|----------------------|--------------------------------------------------------------------------------------|
| `BodyLen == 14`      | Numeric primitive (Integer, Word, Byte, Int64, UnicodeString, Single, Double, Extended). `PrimitiveTypeId` = 2 bytes at `EndOff - 5`. |
| `BodyLen == 15`      | Numeric primitive with extra leading byte (Boolean, Currency, ...). Same recovery rule. |
| `BodyLen == 9` + `$9C $01` at `After+5..+6` | Managed reference primitive (AnsiString, WideString, ShortString). `PrimitiveTypeId` = 2 bytes at `After+3..+4`. |
| `BodyLen >= 10` + `$9C $01` at `After+6..+7` | Enum-typed field. `PrimitiveTypeId` = 2 bytes at `After+3..+4`. |
| `BodyLen >= 11` + `$9C $01` at `After+7..+8` | Enum-typed field at parent offset ≥ 256 (two-byte separator). Same recovery rule. |

When the `FieldId` resolves to a known class/record (via
`FindClassIdxForRawId`), `Member.TypeIdx` is set to the discovered
class's `TypeIdx`. Otherwise `Member.PrimitiveTypeId` is populated
through one of the body-shape rules above. **Members not confirmed by
any Format-A record are pruned by `PruneSpuriousMembers`** to remove
Format-B over-collection from the backward window scan.

### 4.10 `$63` SCOPE_END

A single byte. Closes the active proc scope **only when**
`FScanSeenLocalSinceProc` is True
([Scanner.pas:1136-1142](DPT.Rsm.Scanner.pas#L1136-L1142)). The guard
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
to `FindClassTrailerWithin` which scans up to 8 KB forward for the
trailer pattern. The 4-byte zero prefix before `NameEnd+1..+4` selects
the type-record region (vs. the method-record region whose 4-byte
prefix is non-zero garbage).

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
[StructDiscoverer.pas:399-413](DPT.Rsm.StructDiscoverer.pas#L399-L413).

Field 0's offset is hard-coded to 0; subsequent offsets come from each
prior field's next-offset DWORD.

**First-field locator**: the bytes between the record-name and the
first field tag have a variable-length header (size DWORD, count,
flags, padding) whose shape is **not fully understood** — the walker
scans up to 4 KB forward for the first `$02` byte that satisfies
`IsValidFieldTypeinfoPrefix`. The 4 KB window covers TFW's `TAppCaps`,
which has ~500 bytes of variant-record case-list / nested sub-record
header between the name and the first field.

### 4.14 Format-B field record (for classes, between fields and class trailer)

For **classes**, the field walker is **backward**, not forward —
`TRsmStructDiscoverer.ScanFieldsBackwardFromClassName`. Each field
record has the simpler shape:

```
<DWORD field-offset>  <NL: u8>  <field-name starting with 'F'>
```

The DWORD must satisfy `Off <= $FFFF` (high two bytes zero). The
backward window is bounded by `ScanWindow = 64 KB` upward but capped at
the previous class's anchor (`AMinStartOff`) to prevent cross-class
leakage.

**Heuristic guard**: the field name's first character **must be `F`**
([StructDiscoverer.pas:202-210](DPT.Rsm.StructDiscoverer.pas#L202-L210)).
This rules out non-field length-prefixed strings within the scan window
but means **non-F-prefixed class fields are dropped here** (TFW's
`TAppCaps.DbKindName` etc. are records, which use the structural-anchor
forward walker without this rule). Some classes in real-world binaries
that use non-F prefixes for fields will get their fields missed —
documented as a known limitation.

The backward walker tends to **over-collect** (the window is wide and
matches every `<DWORD-off> <namelen> <F-name>` triple), so the Format-A
linker's `PruneSpuriousMembers` runs as a post-process to drop members
that the `$2C` records never confirmed (see §4.9 and
[FormatALinker.pas:709-743](DPT.Rsm.FormatALinker.pas#L709-L743)).

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

Each item here is anchored to the code location that flags it.

### 6.1 Sparse / explicit-value enums absent from `EnumDefs` (`unused`)

[Scanner.pas:968-975](DPT.Rsm.Scanner.pas#L968-L975) and §4.7 above —
the "different `$03` emission shape" hypothesis is **refuted**: the
linker emits NO `$03 ENUM_DEF` record at all for sparse / explicit-
value enums like `TSparseEnum = (seAlpha = 1, seBeta = 5, seGamma =
11)` (pinned by
[Test.DPT.Rsm.Scanner.TestSparseEnumResolvesViaEnumConstNames32](../Test/Test.DPT.Rsm.Scanner.pas)).
The `$25` channel carries each element with its explicit ordinal so
`EnumConstNames["<typeId>:<ord>"] -> name` works correctly, but
`TRsmReader.EnumDefs` will not list sparse enums. Consumers that
walk the element list (e.g. to enumerate every value of an enum)
silently miss sparse types. A synthetic `EnumDef` could be built from
the buffered `$25` records the way the same-comp cross-unit case does
(see §5.1), keyed by the `$2A` registry name; the scanner already has
all the pieces, only the synthesis step is missing.

### 6.2 Win64 proc-address VAs above 2 MB (`UNCERTAIN`)

[Scanner.pas:363-373](DPT.Rsm.Scanner.pas#L363-L373) — the current
Win64 decoder only uses bytes 0..2 for the VA bits, capping recoverable
RVAs at 21 bits (`$1FFFFF` ≈ 2 MB). Larger binaries presumably carry
higher VA bits in bytes 3/4, but the encoding isn't reverse-engineered
yet. Symptoms when wrong: `SegmentOffset = 0` on procs whose RVA falls
outside the decoded window.

### 6.3 Non-F-prefixed class fields (`GAP`)

[StructDiscoverer.pas:202-210](DPT.Rsm.StructDiscoverer.pas#L202-L210)
— the **class** field walker (backward window) requires the field name
to start with `F`. Records use the structural-anchor forward walker and
are unaffected, but classes whose source convention uses different
prefixes will have their fields missed. The structural anchor used by
records (`$02 $00 <last-flag> $00 $00 $00`) presumably applies to
class fields too, but the surrounding byte stream is different enough
that the existing forward walker isn't a drop-in replacement.

### 6.4 Variable-length header before record's first field (`UNCERTAIN`)

[StructDiscoverer.pas:324-343](DPT.Rsm.StructDiscoverer.pas#L324-L343)
— the header between the record-name's size DWORD and the first
`$02` field tag has a variable layout (count, flags, padding) that
isn't fully understood. The walker scans up to 4 KB forward for the
first `$02` byte satisfying the structural anchor; the 4 KB window has
been sufficient in practice but isn't grounded in a decoded shape.

### 6.5 `$25` cross-unit RTL form's 4-byte RVA (`UNCERTAIN / unused`)

The 4-byte RVA at offset +3..+6 of the cross-unit RTL `$25` body is
captured but never consumed. The `RecordCrossUnitRtlConstant` method
stores `ARecordPos` (the byte offset of the record) but does not
expose the RVA to any resolver. Whether the RVA serves a meaningful
purpose (linker fixup target?) is unknown.

### 6.6 `$2A` type-registry body-flag remaining unknowns (`UNCERTAIN`)

[Scanner.pas:1016-1025](DPT.Rsm.Scanner.pas#L1016-L1025) — the byte
at body offset +0 was previously called `KindFlag` and hypothesised to
discriminate class / record / enum / primitive. That hypothesis is
**refuted**: in DebugTarget's program-local cluster classes and
records share `$20`, while enums and records share `$00` (see §4.8
for the full taxonomy and
[Test.DPT.Rsm.Scanner.Test2ATypeRegistryFlagIsBodyShapeNotKind32](../Test/Test.DPT.Rsm.Scanner.pas)
for the pinning test). What remains open:

1. **Wide-body payload meaning (`$20`)**: the 4 bytes at +5..+8 are a
   non-zero block that climbs monotonically across consecutive entries
   in DebugTarget — `$87 $2F $DB $04` for `TInner`, `$C7 $40 $DB $04`
   for `TDerived`, ... The trailing `$04 $DB` suggests a segmented
   reference (segment `$04DB`, offset varying); we have no decoder
   that consumes any of this yet beyond the enum-bridge use of the
   word at +7/+8.
2. **Which records get `$20` vs. `$00`**: both `TPoint2D` and
   `TPoint3D` are integer-only records yet pick different flags
   (`$20` and `$00` respectively). The discriminator is not obviously
   "has managed fields" or "has variant cases" — likely a Delphi RTTI
   emission decision driven by class membership / `{$M+}` scope, but
   the linker rule isn't pinned down.
3. **Cross-unit RTL values (`$08`, `$80`, `$88`, `$98`, `$A8`, `$B8`)**:
   observed on `TObject`, `TGUID`, `TVisibilityClass`, etc.; body
   shapes are not characterised and no concrete sample has been
   broken open.

### 6.7 `$28` `$80` / `$00` sub-tags (`GAP`)

[Scanner.pas:444-455](DPT.Rsm.Scanner.pas#L444-L455) — the proc-record
sub-tag byte at `name+0` is dispatched for `$20`, `$A0`, `$41`.
Other observed values (`$80`, `$00`, ...) are treated as
forward-declaration / cross-reference records with no embedded address.
There may be address payloads in those forms that the current decoder
discards.

### 6.9 FieldId → Enum binding for `$2C` enum-typed fields (`GAP`, under investigation)

[Test.DPT.Rsm.Reader.pas:231-486](../Test/Test.DPT.Rsm.Reader.pas#L231-L486)
— the `DiagnoseTfwFieldIdBindings` test scans `TFW.rsm` for `$03
ENUM_DEF` records of `TUserKonsOutlookDirection` and
`TUserKonsOutlookSyncStatus`, then searches for the FieldIds observed
on the corresponding `$2C` field records (`$0D2A` for
`SyncDirection`, `$1D2C` for `SyncStatus`). The investigation is open:
the current Format-A linker uses the `$9C $01` body-shape rules to
recover a `PrimitiveTypeId` for enum-typed fields, but pairing the
field's id with the `EnumDef` it points at (so the resolver knows
**which** enum to format the ordinal against) is not yet wired up for
the wide cross-unit case in TFW.

### 6.10 `$2C` parent id narrow encoding scope (`UNCERTAIN`)

[FormatALinker.pas:614-631](DPT.Rsm.FormatALinker.pas#L614-L631) —
when `parent-hi == $FF`, the parent id is treated as a unit-local byte
and resolved via the block-owner index. This is sufficient for the
TFW `UserKonsOutlook*` corpus but may miss edge cases (block-owner
pairing breaks down when a unit's `$0E` record markers don't appear in
the same order as the `$2C` field blocks). The pairing is heuristic.

### 6.11 `TRsmEnumDecoder.FLastSecondary` (`unused`)

[EnumDecoder.pas:71-78](DPT.Rsm.EnumDecoder.pas#L71-L78) — the most
recently scanned `$25` secondary id and its file position are recorded
on every cross-unit constant but never consumed. Comment notes "set-only;
reserved for future use by resolvers that want to reason about
per-$25-region locality". Not a gap so much as latent state.

### 6.12 `TStrings → TPersistent → TObject` inheritance chain (`GAP`)

[Test.DPT.Rsm.LocalsReader.pas:686-738](../Test/Test.DPT.Rsm.LocalsReader.pas#L686-L738)
— `DoTestNonComponentRtlInheritance` documents that `TStringList.FCount`
resolves (own field) but `TStringList.FUpdateCount via TStrings` does
NOT, because the RSM emits `TStrings`'s field records in a region that
sits BEFORE the previous discovered class's `AMinStartOff` cap. The
backward-scan-with-cap is conservatively safe (prevents cross-class
leakage) but loses cross-anchor inherited fields for non-TComponent
RTL hierarchies.

### 6.13 Field byte width for terminal fields (`UNCERTAIN`)

[StructDiscoverer.pas:432-435](DPT.Rsm.StructDiscoverer.pas#L432-L435)
and [256-259](DPT.Rsm.StructDiscoverer.pas#L256-L259) — the last
member of a record / class has `Size := 0` because its byte width
cannot be derived from a successor offset. The evaluator falls back
to the user-requested type's width in that case; whether the byte
width is recoverable from elsewhere in the field's typeinfo prefix is
not known.

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
5. Runs the four post-process passes:
   `TRsmFormatALinker.Run` →
   `TRsmClassParentDeriver.Run` →
   `TRsmCrossUnitParentResolver.Run` →
   `TRsmScopeLocalEnumBridge.Run`.
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
