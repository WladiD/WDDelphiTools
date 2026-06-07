// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.BufferIO;

// Stateless byte-level helpers for the RSM (CSH7) buffer. Lives in
// its own unit so TRsmScanner and TRsmStructDiscoverer can share one
// implementation of "read N bytes / parse length-prefixed identifier
// / validate a field-typeinfo prefix" without either class taking a
// dependency on the other.
//
// All functions accept the raw <c>(ABuf, ASz)</c> pair so the helpers
// never need to know which class owns the memory; the inline ByteAt
// and DwordAt thin-wrap raw pointer access for readability at call
// sites.

interface

/// <summary>
///   Single byte at the given offset within the supplied buffer. No
///   bounds check (callers verify offset).
/// </summary>
function RsmByteAt(ABuf: PByte; AOffset: NativeInt): Byte; inline;

/// <summary>
///   Little-endian DWORD at the given offset within the supplied
///   buffer. No bounds check.
/// </summary>
function RsmDwordAt(ABuf: PByte; AOffset: NativeInt): UInt32; inline;

/// <summary>
///   True when the 4 bytes at <c>AOffset</c> equal <c>AB0..AB3</c> in
///   stream order. Faster than four <c>RsmByteAt</c> compares (one
///   unaligned 32-bit load + one compare against the byte values folded
///   into an LE DWORD), yet the call site stays self-documenting — the
///   expected anchor bytes are spelled out instead of hidden in a hex
///   literal. No bounds check (callers verify offset + 3).
/// </summary>
function RsmDwordAtEquals(ABuf: PByte; AOffset: NativeInt;
  AB0, AB1, AB2, AB3: Byte): Boolean; inline;

/// <summary>
///   True for bytes that may appear in an RSM identifier (digits,
///   ASCII letters, plus the qualified-name / generic / linker-alias
///   punctuation: '.', '_', '$', '@', '&lt;', '&gt;', ',').
/// </summary>
function RsmIsPrintableAscii(AB: Byte): Boolean; inline;

/// <summary>
///   Length-prefixed identifier reader. Returns False when the length
///   is implausible for a Delphi identifier or any byte in the run is
///   not an identifier character.
/// </summary>
function RsmReadIdentifier(ABuf: PByte; ASz: NativeInt; AOffset: NativeInt;
  out AName: String): Boolean;

/// <summary>
///   Validates the 6 bytes that follow a record-field's name. Anchors
///   the field-walker on structural shape (<c>$02 $00 [last-flag] $00
///   $00 $00</c>) rather than the field name's first character, so
///   records whose fields don't use the F-prefix convention parse
///   correctly.
/// </summary>
function RsmIsValidFieldTypeinfoPrefix(ABuf: PByte; ASz: NativeInt;
  AOffset: NativeInt): Boolean;

implementation

uses

  System.SysUtils;

function RsmByteAt(ABuf: PByte; AOffset: NativeInt): Byte;
begin
  Result := (ABuf + AOffset)^;
end;

function RsmDwordAt(ABuf: PByte; AOffset: NativeInt): UInt32;
begin
  Result := PUInt32(ABuf + AOffset)^;
end;

function RsmDwordAtEquals(ABuf: PByte; AOffset: NativeInt;
  AB0, AB1, AB2, AB3: Byte): Boolean;
begin
  Result := PUInt32(ABuf + AOffset)^ =
    (UInt32(AB0) or (UInt32(AB1) shl 8) or
     (UInt32(AB2) shl 16) or (UInt32(AB3) shl 24));
end;

function RsmIsPrintableAscii(AB: Byte): Boolean;
begin
  // Identifier bytes accepted inside RSM length-prefixed names.
  // The CSH7 emitter writes names verbatim from Delphi source, but
  // a "name" can be qualified or compiler-decorated:
  //   - 'TFormMain.Create'                  -- class-method PROC records
  //   - 'TFormMain.Create$ActRec'           -- closure / nested-func record
  //   - 'TList<TFoo>.Add'                   -- generic instantiation
  //   - '@MyName'                           -- linker-emitted alias
  //   - 'TMap<TKey,TValue>.Get'             -- multi-arg generics (comma)
  // Keeping the check tight (no whitespace, no control bytes) limits
  // false-positive PROC records when a random byte run happens to
  // pass the length-prefix shape, while still letting real qualified
  // names through.
  Result := ((AB >= Ord('A')) and (AB <= Ord('Z'))) or
            ((AB >= Ord('a')) and (AB <= Ord('z'))) or
            ((AB >= Ord('0')) and (AB <= Ord('9'))) or
            (AB = Ord('_')) or
            (AB = Ord('.')) or
            (AB = Ord('$')) or
            (AB = Ord('<')) or
            (AB = Ord('>')) or
            (AB = Ord(',')) or
            (AB = Ord('@'));
end;

function RsmReadIdentifier(ABuf: PByte; ASz: NativeInt; AOffset: NativeInt;
  out AName: String): Boolean;
var
  L, I: Integer;
  Buf2: TBytes;
begin
  Result := False;
  AName := '';
  if (AOffset < 0) or (AOffset >= ASz) then Exit;
  L := RsmByteAt(ABuf, AOffset);
  if (L < 1) or (L > 64) then Exit;
  if AOffset + 1 + L > ASz then Exit;
  for I := 0 to L - 1 do
    if not RsmIsPrintableAscii(RsmByteAt(ABuf, AOffset + 1 + I)) then Exit;
  SetLength(Buf2, L);
  Move((ABuf + AOffset + 1)^, Buf2[0], L);
  AName := TEncoding.ANSI.GetString(Buf2);
  Result := True;
end;

function RsmIsValidFieldTypeinfoPrefix(ABuf: PByte; ASz: NativeInt;
  AOffset: NativeInt): Boolean;
// Validates the 6 bytes that follow a record-field's name. The
// RSM emits every field with the exact prefix
//   $02 $00 <last-flag> $00 $00 $00       (last-flag: $00 or $02)
// where last-flag = $02 marks the record's terminal field. This
// structural anchor is far more selective than any first-character
// heuristic on the field name (e.g. requiring 'F' as the Delphi
// house-style prefix would drop every TFW record whose source uses
// a different convention -- TAppCaps.DbKindName, TMdt.Id, ...).
begin
  Result := False;
  if AOffset + 5 >= ASz then Exit;
  if RsmByteAt(ABuf, AOffset)     <> $02 then Exit;
  if RsmByteAt(ABuf, AOffset + 1) <> $00 then Exit;
  if (RsmByteAt(ABuf, AOffset + 2) <> $00) and
     (RsmByteAt(ABuf, AOffset + 2) <> $02) then Exit;
  if RsmByteAt(ABuf, AOffset + 3) <> $00 then Exit;
  if RsmByteAt(ABuf, AOffset + 4) <> $00 then Exit;
  if RsmByteAt(ABuf, AOffset + 5) <> $00 then Exit;
  Result := True;
end;

end.
