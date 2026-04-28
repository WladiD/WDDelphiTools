// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Exe.Types;

interface

uses

  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Types;

type

  /// <summary>
  ///   Describes which detection method produced the unit list. The
  ///   analyzer tries strategies in descending order of confidence and
  ///   records the first one that succeeded.
  /// </summary>
  TExeDetectionMethod = (
    edmNone,    // No method succeeded - units list is empty.
    edmMap      // Sibling .map file found and parsed.
  );

  /// <summary>One section of the PE image (.text, .data, .rsrc, ...).</summary>
  TExePeSection = record
    Name        : string;
    VirtualSize : UInt64;
    RawSize     : UInt64;
    constructor Create(const AName: string; AVirtualSize, ARawSize: UInt64);
  end;

  /// <summary>One linked unit found inside the EXE.</summary>
  TExeUnitEntry = record
    Name       : string;
    /// <summary>
    ///   Virtual address of this unit's first byte of code. Zero when
    ///   the segment information was not available.
    /// </summary>
    StartVA    : UInt64;
    /// <summary>
    ///   Approximate code size in bytes derived from the .map segment
    ///   that owns this unit. Zero when the segment information was
    ///   not available.
    /// </summary>
    CodeSize   : UInt64;
    /// <summary>
    ///   When the analyzer was given a DcuIndex, this is the disk path
    ///   of the DCU that contributed the unit. Empty when no index was
    ///   provided or no matching DCU was found.
    /// </summary>
    ResolvedDcu: string;
    constructor Create(const AName: string; AStartVA, ACodeSize: UInt64);
  end;

  /// <summary>Aggregated analysis result of a Delphi-built EXE.</summary>
  TExeAnalysisResult = record
    FilePath        : string;
    FileSize        : Int64;
    IsPE            : Boolean;

    // PE header
    Platform        : TDcuPlatform;        // dpWin32 / dpWin64
    SubsystemName   : string;              // "GUI" / "Console" / ...
    Compiled        : TDateTime;
    Sections        : IList<TExePeSection>;

    // Detection
    Detection       : TExeDetectionMethod;
    DetectionDetail : string;              // human-readable label
    MapFilePath     : string;              // empty when no .map was found

    // Result
    Units           : IList<TExeUnitEntry>;
    Diagnostics     : IList<string>;
  end;

const
  TExeDetectionMethodName: array[TExeDetectionMethod] of string = (
    { edmNone } 'none',
    { edmMap  } 'map'
  );

implementation

{ TExePeSection }

constructor TExePeSection.Create(const AName: string;
  AVirtualSize, ARawSize: UInt64);
begin
  Name := AName;
  VirtualSize := AVirtualSize;
  RawSize := ARawSize;
end;

{ TExeUnitEntry }

constructor TExeUnitEntry.Create(const AName: string;
  AStartVA, ACodeSize: UInt64);
begin
  Name := AName;
  StartVA := AStartVA;
  CodeSize := ACodeSize;
  ResolvedDcu := '';
end;

end.
