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
  ///   Represents an entry in the DCU's interface or implementation uses
  ///   table. Each entry carries the imported unit's name and the CRC the
  ///   compiler stored to detect "must rebuild" situations.
  /// </summary>
  TDcuUsesEntry = record
    UnitName: string;
    CRC     : UInt32;
    constructor Create(const AUnitName: string; ACRC: UInt32);
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
  ///   Aggregated header information that the iteration-1 analyzer is
  ///   confident to extract: raw magic bytes, the unit's primary source
  ///   file name, the derived unit name and any embedded source includes.
  /// </summary>
  TDcuHeaderInfo = record
    MagicBytes      : array[0..3] of Byte;
    MagicHex        : string;
    DetectedCompiler: TDcuKnownCompiler;
    UnitName        : string;
    PrimarySource   : TDcuSourceRef;
    IncludeSources  : IList<TDcuSourceRef>;
  end;

  /// <summary>
  ///   The complete result of an iteration-1 DCU analysis. Future
  ///   iterations will add Symbols and Sections inventories.
  /// </summary>
  TDcuAnalysisResult = record
    FilePath        : string;
    FileSize        : Int64;
    IsDcu           : Boolean;
    Header          : TDcuHeaderInfo;
    InterfaceUses   : IList<TDcuUsesEntry>;
    UsesParsed      : Boolean;
    UsesParseStopAt : Integer;
    Diagnostics     : IList<string>;
    FirstBytesPreview: string;
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
  ///   Tag byte sequence that has been observed immediately before the
  ///   interface uses list on the modern DCU layout. Used as a heuristic
  ///   anchor point when searching for the uses table.
  /// </summary>
  DcuUsesAnchor_Modern: array[0..1] of Byte = ($00, $64);

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

implementation

{ TDcuUsesEntry }

constructor TDcuUsesEntry.Create(const AUnitName: string; ACRC: UInt32);
begin
  UnitName := AUnitName;
  CRC := ACRC;
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
