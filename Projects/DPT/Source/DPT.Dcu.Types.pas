// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Types;

interface

uses

  System.SysUtils,

  mormot.core.collections;

type

  /// <summary>
  ///   Distinguishes whether an imported unit was referenced from the
  ///   unit's interface section ($63 $64 marker in the DCU) or from the
  ///   implementation section ($63 $65 marker).
  /// </summary>
  TDcuUsesScope = (dusUnknown, dusInterface, dusImplementation);

  /// <summary>
  ///   Classifies a symbol entry found in the DCU. The mapping comes
  ///   from the modern-DCU tag byte that precedes each length-prefixed
  ///   symbol name. Imported and exported entries share the same two
  ///   broad classes (type vs. routine) but use different tag bytes:
  ///   imports use <c>$66</c>/<c>$67</c>, exports use <c>$2A</c>/<c>$28</c>.
  /// </summary>
  TDcuSymbolKind = (dskType, dskMethod);

  /// <summary>
  ///   Whether the symbol is referenced from another unit
  ///   (<c>dsoImported</c>) or declared in this unit
  ///   (<c>dsoExported</c>). The two classes are extracted by separate
  ///   scanner passes; consumers can filter by Origin to ask "what does
  ///   this DCU export?" vs. "what does it consume?".
  /// </summary>
  TDcuSymbolOrigin = (dsoImported, dsoExported);

  /// <summary>
  ///   A single symbol entry from the DCU. Fixed-layout: tag byte,
  ///   1-byte length, ASCII name, 4-byte stored hash/CRC. The exact
  ///   tag determines the (Kind, Origin) combination.
  /// </summary>
  TDcuSymbolRef = record
    Kind  : TDcuSymbolKind;
    Origin: TDcuSymbolOrigin;
    Name  : string;
    Hash  : UInt32;
    Offset: Integer;
    constructor Create(AKind: TDcuSymbolKind; AOrigin: TDcuSymbolOrigin;
      const AName: string; AHash: UInt32; AOffset: Integer);
  end;

  /// <summary>
  ///   Represents an entry in the DCU's uses table. Each entry carries
  ///   the imported unit's name, the scope (interface/implementation)
  ///   the compiler tagged it with, and the byte offset at which the
  ///   entry was found - useful for diagnostics and round-trip dumps.
  ///   <c>ResolvedPath</c> is filled in by the optional uses-resolver
  ///   pass with the absolute path to the located DCU on disk; an
  ///   empty value means either the resolver did not run or no DCU
  ///   was found.
  /// </summary>
  TDcuUsesEntry = record
    UnitName    : string;
    Scope       : TDcuUsesScope;
    Offset      : Integer;
    ResolvedPath: string;
    constructor Create(const AUnitName: string; AScope: TDcuUsesScope;
      AOffset: Integer);
  end;

  /// <summary>
  ///   A length-prefixed string entry found inside the DCU header region,
  ///   typically a reference to a source file (.pas/.dpr/.dpk/.inc) used
  ///   while compiling the unit. The trailing 4 bytes are the source's
  ///   stored timestamp.
  /// </summary>
  TDcuSourceRef = record
    FileName : string;
    Timestamp: UInt32;
    Offset   : Integer;
    constructor Create(const AFileName: string; ATimestamp: UInt32; AOffset: Integer);
  end;

  /// <summary>
  ///   Identifies the assumed Delphi compiler that produced a DCU. The
  ///   mapping from the raw magic/version bytes to this enum is empirical
  ///   and is filled in by the multi-version test corpus.
  /// </summary>
  TDcuKnownCompiler = (
    dccUnknown,
    dccDelphi2007,
    dccDelphi10_1,
    dccDelphi10_3,
    dccDelphi11,
    dccDelphi12,
    dccDelphi13);

  /// <summary>
  ///   Identifies the target platform encoded in the DCU header. The
  ///   mapping is derived from byte 1 of the magic-byte sequence; only
  ///   values that have been observed on a real DCU are returned.
  /// </summary>
  TDcuPlatform = (dpUnknown, dpWin32, dpWin64);

  /// <summary>
  ///   Aggregated header information that the iteration-1 analyzer is
  ///   confident to extract: raw magic bytes, the unit's primary source
  ///   file name, the derived unit name and any embedded source includes.
  /// </summary>
  TDcuHeaderInfo = record
    MagicBytes      : array[0..3] of Byte;
    MagicHex        : string;
    DetectedCompiler: TDcuKnownCompiler;
    DetectedPlatform: TDcuPlatform;
    UnitName        : string;
    PrimarySource   : TDcuSourceRef;
    IncludeSources  : IList<TDcuSourceRef>;
  end;

  /// <summary>
  ///   The complete result of an iteration-1 DCU analysis. Future
  ///   iterations will add Symbols and Sections inventories.
  /// </summary>
  TDcuAnalysisResult = record
    FilePath          : string;
    FileSize          : Int64;
    IsDcu             : Boolean;
    Header            : TDcuHeaderInfo;
    InterfaceUses     : IList<TDcuUsesEntry>;
    ImplementationUses: IList<TDcuUsesEntry>;
    UsesParsed        : Boolean;
    Symbols           : IList<TDcuSymbolRef>;
    SymbolsParsed     : Boolean;
    Diagnostics       : IList<string>;
    FirstBytesPreview : string;
  end;

const
  /// <summary>
  ///   The magic byte that has been observed on every modern Delphi DCU
  ///   sampled so far (D11 Win64, D12 Win64). Older or alternate-platform
  ///   DCUs may use a different value; the analyzer must not reject a
  ///   file purely because the byte does not match.
  /// </summary>
  DcuMagicByte_Modern = $4D;

  /// <summary>
  ///   Marker bytes that prefix every uses-table entry in the modern DCU
  ///   layout. The 2-byte pair always starts with $63 ('c'); the second
  ///   byte distinguishes the scope: $64 ('d') = interface uses, $65
  ///   ('e') = implementation uses. Followed by a length-prefixed name
  ///   and three zero trailer bytes.
  /// </summary>
  DcuUsesTag_Marker         = $63;
  DcuUsesTag_InterfaceScope = $64;
  DcuUsesTag_ImplScope      = $65;

  /// <summary>Maximum length we will accept for a uses-entry name.</summary>
  DcuUsesEntryMaxLen = 200;

  /// <summary>
  ///   Tag bytes that prefix imported-symbol cross-reference entries in
  ///   the modern DCU layout. Each entry is followed by a 1-byte length,
  ///   the ASCII name, and a 4-byte hash/CRC trailer.
  /// </summary>
  DcuImportTag_Type   = $66;
  DcuImportTag_Method = $67;

  /// <summary>
  ///   Tag bytes that prefix exported-symbol declarations (the unit's
  ///   own types and routines). Same fixed-layout (tag, length, name,
  ///   4-byte hash) as the import variants. <c>$26</c> is a dot-prefixed
  ///   anchor that the compiler emits before <c>$2A</c> for the same
  ///   type and is intentionally skipped during extraction - it would
  ///   only produce duplicate entries with a leading dot.
  /// </summary>
  DcuExportTag_TypeAnchor = $26;
  DcuExportTag_Routine    = $28;
  DcuExportTag_Type       = $2A;

  /// <summary>Maximum length we will accept for a symbol-ref name.</summary>
  DcuSymbolRefMaxLen = 200;

  // Backwards-compatibility aliases (older callers still in flight).
  DcuSymbolTag_Type   = DcuImportTag_Type;
  DcuSymbolTag_Method = DcuImportTag_Method;

  DcuSymbolKindName: array[TDcuSymbolKind] of string = (
    { dskType   } 'Type',
    { dskMethod } 'Method'
  );

  DcuSymbolOriginName: array[TDcuSymbolOrigin] of string = (
    { dsoImported } 'Imported',
    { dsoExported } 'Exported'
  );

  /// <summary>
  ///   Identifiers that look like valid Pascal unit names but are in
  ///   fact built-in types or compiler intrinsics. They never refer to
  ///   a real unit, so any candidate uses-table entry whose name appears
  ///   here is a coincidental byte alignment in the symbol section and
  ///   must be discarded. Comparison is case-insensitive.
  /// </summary>
  DcuBuiltinNonUnitNames: array[0..47] of string = (
    // Integer types
    'Integer', 'Cardinal', 'Word', 'Byte', 'ShortInt', 'SmallInt',
    'LongInt', 'LongWord', 'Int8', 'Int16', 'Int32', 'Int64',
    'UInt8', 'UInt16', 'UInt32', 'UInt64',
    'NativeInt', 'NativeUInt', 'FixedInt', 'FixedUInt',
    // Floating point
    'Single', 'Double', 'Extended', 'Currency', 'Comp', 'Real', 'Real48',
    // Boolean
    'Boolean', 'ByteBool', 'WordBool', 'LongBool',
    // Character & string types
    'Char', 'AnsiChar', 'WideChar',
    'string', 'AnsiString', 'UnicodeString', 'RawByteString',
    'WideString', 'ShortString', 'UTF8String',
    // Pointer types
    'Pointer', 'PChar', 'PAnsiChar', 'PWideChar',
    // Misc intrinsics
    'Variant', 'OleVariant', 'TDateTime'
  );

  /// <summary>
  ///   File extensions accepted as primary source / include-source
  ///   references inside the header region.
  /// </summary>
  DcuSourceExtensions: array[0..3] of string = ('.pas', '.dpr', '.dpk', '.inc');

  DcuKnownCompilerName: array[TDcuKnownCompiler] of string = (
    { dccUnknown    } 'Unknown',
    { dccDelphi2007 } 'Delphi 2007',
    { dccDelphi10_1 } 'Delphi 10.1 Berlin',
    { dccDelphi10_3 } 'Delphi 10.3 Rio',
    { dccDelphi11   } 'Delphi 11 Alexandria',
    { dccDelphi12   } 'Delphi 12 Athens',
    { dccDelphi13   } 'Delphi 13'
  );

  DcuPlatformName: array[TDcuPlatform] of string = (
    { dpUnknown } 'Unknown',
    { dpWin32   } 'Win32',
    { dpWin64   } 'Win64'
  );

  /// <summary>
  ///   Empirically observed mapping byte-3 -> compiler. Each entry must
  ///   be backed by a real DCU sample produced by the corresponding
  ///   compiler version, captured in the multi-version test corpus.
  /// </summary>
  DcuVersionByte_Delphi11 = $23;
  DcuVersionByte_Delphi12 = $24;
  DcuVersionByte_Delphi13 = $25;

  /// <summary>Empirically observed mapping byte-1 -> platform.</summary>
  DcuPlatformByte_Win32 = $03;
  DcuPlatformByte_Win64 = $23;

implementation

{ TDcuUsesEntry }

constructor TDcuUsesEntry.Create(const AUnitName: string; AScope: TDcuUsesScope;
  AOffset: Integer);
begin
  UnitName := AUnitName;
  Scope := AScope;
  Offset := AOffset;
  ResolvedPath := '';
end;

{ TDcuSymbolRef }

constructor TDcuSymbolRef.Create(AKind: TDcuSymbolKind;
  AOrigin: TDcuSymbolOrigin; const AName: string; AHash: UInt32;
  AOffset: Integer);
begin
  Kind := AKind;
  Origin := AOrigin;
  Name := AName;
  Hash := AHash;
  Offset := AOffset;
end;

{ TDcuSourceRef }

constructor TDcuSourceRef.Create(const AFileName: string; ATimestamp: UInt32;
  AOffset: Integer);
begin
  FileName := AFileName;
  Timestamp := ATimestamp;
  Offset := AOffset;
end;

end.
