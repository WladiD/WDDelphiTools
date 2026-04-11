// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.MapFileParser;

interface

uses
  System.SysUtils,
  System.Classes,

  mormot.core.collections;

type

  TMapLineNumber = record
    LineNumber: Integer;
    Segment: Word;
    UnitName: string;
    VA: UInt64;
  end;

  TMapProcName = record
    Name: string;
    Segment: Word;
    VA: UInt64;
  end;

  TMapSegment = record
    EndVA: UInt64;
    Segment: Word;
    StartVA: UInt64;
    UnitName: string;
  end;

  TMapSegmentClass = record
    IsTLS: Boolean;
    Len: UInt64;
    Segment: Word;
    Start: UInt64;
    VA: UInt64;
  end;

  /// <summary>Parses Delphi MAP files to extract symbol information for debugging.
  /// All address lookups use Virtual Addresses (VA), which are relative offsets
  /// from the code segment start. The debugger converts absolute process addresses
  /// to VAs by subtracting the module base address and the section offset ($1000).
  /// Supports both 32-bit and 64-bit MAP files natively using UInt64 addresses.</summary>
  TMapFileParser = class
  private
    FLastSegmentIndex: Integer;
    FLineNumbers     : IList<TMapLineNumber>;
    FProcNames       : IList<TMapProcName>;
    FSegmentClasses  : IList<TMapSegmentClass>;
    FSegments        : IList<TMapSegment>;
    function GetLineNumberByIndex(Index: Integer): TMapLineNumber;
    function GetLineNumbersCnt: Integer;
    function IndexOfSegment(Addr: UInt64): Integer;
    function MAPAddrToVA(Addr: UInt64): UInt64;
    function ModuleStartFromAddr(Addr: UInt64): UInt64;
    procedure Parse(const AContent: RawByteString);
    procedure ParseClassTable(var P: PAnsiChar; EndP: PAnsiChar);
    procedure ParseDetailedSegments(var P: PAnsiChar; EndP: PAnsiChar);
    procedure ParseLineNumbers(var P: PAnsiChar; EndP: PAnsiChar);
    procedure ParsePublicsByValue(var P: PAnsiChar; EndP: PAnsiChar);
    procedure SortArrays;
  public
    constructor Create(const AMapFileName: string);
    function LineNumberFromAddr(Addr: UInt64): Integer; overload;
    function LineNumberFromAddr(Addr: UInt64; out Offset: Integer): Integer; overload;
    function ModuleNameFromAddr(Addr: UInt64): string;
    function ProcNameFromAddr(Addr: UInt64): string; overload;
    function ProcNameFromAddr(Addr: UInt64; out Offset: Integer): string; overload;
    function VAFromUnitAndProcName(const AUnitName, AProcName: string): UInt64;
    property LineNumberByIndex[Index: Integer]: TMapLineNumber read GetLineNumberByIndex;
    property LineNumbersCnt: Integer read GetLineNumbersCnt;
  end;

implementation

// === Parsing helpers ===

function IsHexChar(C: AnsiChar): Boolean; inline;
begin
  Result := (C >= '0') and (C <= '9') or
            (C >= 'A') and (C <= 'F') or
            (C >= 'a') and (C <= 'f');
end;

function ReadHexValue(var P: PAnsiChar; EndP: PAnsiChar): UInt64;
var
  C: AnsiChar;
begin
  Result := 0;
  while (P < EndP) do
  begin
    C := P^;
    case C of
      '0'..'9': Result := (Result shl 4) or UInt64(Ord(C) - Ord('0'));
      'A'..'F': Result := (Result shl 4) or UInt64(Ord(C) - Ord('A') + 10);
      'a'..'f': Result := (Result shl 4) or UInt64(Ord(C) - Ord('a') + 10);
    else
      Break;
    end;
    Inc(P);
  end;
  // Skip trailing 'H' suffix if present
  if (P < EndP) and (P^ = 'H') then
    Inc(P);
end;

procedure ReadAddress(var P: PAnsiChar; EndP: PAnsiChar; out Segment: Word; out Offset: UInt64);
begin
  Segment := Word(ReadHexValue(P, EndP));
  if (P < EndP) and (P^ = ':') then
    Inc(P);
  Offset := ReadHexValue(P, EndP);
end;

procedure SkipSpaces(var P: PAnsiChar; EndP: PAnsiChar); inline;
begin
  while (P < EndP) and (P^ = ' ') do
    Inc(P);
end;

procedure SkipToNextLine(var P: PAnsiChar; EndP: PAnsiChar); inline;
begin
  while (P < EndP) and (P^ <> #10) do
    Inc(P);
  if (P < EndP) then
    Inc(P);
end;

function ReadToken(var P: PAnsiChar; EndP: PAnsiChar): string;
var
  Start: PAnsiChar;
begin
  Start := P;
  while (P < EndP) and (P^ > ' ') do
    Inc(P);
  SetString(Result, Start, P - Start);
end;

function ReadRestOfLine(var P: PAnsiChar; EndP: PAnsiChar): string;
var
  Start, E: PAnsiChar;
begin
  Start := P;
  while (P < EndP) and (P^ <> #13) and (P^ <> #10) do
    Inc(P);
  E := P;
  // Trim trailing spaces
  while (E > Start) and ((E - 1)^ = ' ') do
    Dec(E);
  SetString(Result, Start, E - Start);
end;

function SyncToHeader(var P: PAnsiChar; EndP: PAnsiChar; const AHeader: RawByteString): Boolean;
var
  HeaderLen: Integer;
  LineStart, LineEnd, SearchPos: PAnsiChar;
begin
  HeaderLen := Length(AHeader);
  while P + HeaderLen <= EndP do
  begin
    LineStart := P;
    LineEnd := P;
    while (LineEnd < EndP) and (LineEnd^ <> #10) do
      Inc(LineEnd);
    // Search for AHeader anywhere in the current line
    SearchPos := LineStart;
    while SearchPos + HeaderLen <= LineEnd do
    begin
      if CompareMem(SearchPos, Pointer(AHeader), HeaderLen) then
      begin
        P := LineEnd;
        if P < EndP then Inc(P); // skip #10
        Exit(True);
      end;
      Inc(SearchPos);
    end;
    P := LineEnd;
    if P < EndP then Inc(P);
  end;
  Result := False;
end;

function SyncToPrefix(var P: PAnsiChar; EndP: PAnsiChar; const APrefix: RawByteString): Boolean;
var
  PrefixLen: Integer;
begin
  PrefixLen := Length(APrefix);
  while P + PrefixLen <= EndP do
  begin
    SkipSpaces(P, EndP);
    if CompareMem(P, Pointer(APrefix), PrefixLen) then
      Exit(True);
    SkipToNextLine(P, EndP);
  end;
  Result := False;
end;

function ExtractModuleName(const AQualifiedName: string): string;
var
  DotPos: Integer;
begin
  DotPos := Pos('.', AQualifiedName);
  if DotPos > 0 then
    Result := Copy(AQualifiedName, 1, DotPos - 1)
  else
    Result := AQualifiedName;
end;

{ TMapFileParser }

constructor TMapFileParser.Create(const AMapFileName: string);
var
  Content: RawByteString;
  Stream: TFileStream;
begin
  inherited Create;
  FSegmentClasses := Collections.NewPlainList<TMapSegmentClass>;
  FSegments := Collections.NewPlainList<TMapSegment>;
  FProcNames := Collections.NewPlainList<TMapProcName>;
  FLineNumbers := Collections.NewPlainList<TMapLineNumber>;
  if not FileExists(AMapFileName) then
    Exit;
  Stream := TFileStream.Create(AMapFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Content, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Content[1], Stream.Size);
  finally
    Stream.Free;
  end;
  Parse(Content);
end;

procedure TMapFileParser.Parse(const AContent: RawByteString);
var
  P, EndP, SaveP: PAnsiChar;
begin
  if Length(AContent) = 0 then
    Exit;

  P := PAnsiChar(AContent);
  EndP := P + Length(AContent);

  // 1. Class table (segment headers)
  SaveP := P;
  ParseClassTable(P, EndP);

  // 2. Detailed segments (module/unit mapping)
  P := SaveP;
  ParseDetailedSegments(P, EndP);

  // 3. Publics by Value (procedure names)
  P := SaveP;
  ParsePublicsByValue(P, EndP);

  // 4. Line numbers
  P := SaveP;
  ParseLineNumbers(P, EndP);

  SortArrays;
end;

procedure TMapFileParser.ParseClassTable(var P: PAnsiChar; EndP: PAnsiChar);
var
  Seg: Word;
  Offset, Len: UInt64;
  SectionName, ClassName: string;
  C: Integer;
begin
  // Sync to "Start" header line
  if not SyncToHeader(P, EndP, ' Start') then
    Exit;

  // Skip empty lines
  while (P < EndP) and ((P^ = #13) or (P^ = #10) or (P^ = ' ')) do
  begin
    if (P^ = #10) then
    begin
      // Check if next line has content
      var NextP := P + 1;
      while (NextP < EndP) and (NextP^ = ' ') do Inc(NextP);
      if (NextP >= EndP) or (NextP^ = #13) or (NextP^ = #10) then
        Break;
    end;
    Inc(P);
  end;

  while (P < EndP) do
  begin
    SkipSpaces(P, EndP);
    if (P >= EndP) or not IsHexChar(P^) then
      Break;

    ReadAddress(P, EndP, Seg, Offset);
    SkipSpaces(P, EndP);
    Len := ReadHexValue(P, EndP);
    SkipSpaces(P, EndP);
    SectionName := ReadToken(P, EndP);
    SkipSpaces(P, EndP);
    ClassName := ReadToken(P, EndP);
    SkipToNextLine(P, EndP);

    var SC: TMapSegmentClass;
    SC.Segment := Seg;
    SC.Start := Offset;
    SC.Len := Len;
    SC.IsTLS := SameText(Copy(ClassName, 1, 3), 'TLS');
    if SC.IsTLS then
      SC.VA := Offset
    else
      SC.VA := MAPAddrToVA(Offset);
    FSegmentClasses.Add(SC);
  end;
end;

procedure TMapFileParser.ParseDetailedSegments(var P: PAnsiChar; EndP: PAnsiChar);
var
  Seg: Word;
  Offset, Len, VA: UInt64;
  ModuleName: string;
  SegIndex: Integer;
begin
  if not SyncToHeader(P, EndP, 'Detailed map of segments') then
    Exit;

  while (P < EndP) do
  begin
    SkipSpaces(P, EndP);
    // Skip blank lines
    if (P < EndP) and ((P^ = #13) or (P^ = #10)) then
    begin
      SkipToNextLine(P, EndP);
      Continue;
    end;
    if (P >= EndP) or not IsHexChar(P^) then
      Break;

    ReadAddress(P, EndP, Seg, Offset);
    SkipSpaces(P, EndP);
    Len := ReadHexValue(P, EndP);

    // Read rest of line to find M= parameter
    ModuleName := '';
    while (P < EndP) and (P^ <> #13) and (P^ <> #10) do
    begin
      if (P^ = 'M') and ((P + 1) < EndP) and ((P + 1)^ = '=') then
      begin
        Inc(P, 2);
        ModuleName := ReadToken(P, EndP);
      end
      else
        Inc(P);
    end;
    SkipToNextLine(P, EndP);

    if ModuleName = '' then
      Continue;

    // Find matching segment class
    SegIndex := -1;
    for var I := 0 to FSegmentClasses.Count - 1 do
      if (FSegmentClasses[I].Segment = Seg) and (Offset < FSegmentClasses[I].Len) then
      begin
        SegIndex := I;
        Break;
      end;
    if SegIndex < 0 then
      Continue;

    if FSegmentClasses[SegIndex].IsTLS then
      VA := Offset
    else
      VA := MAPAddrToVA(Offset + FSegmentClasses[SegIndex].Start);

    var Segment: TMapSegment;
    Segment.Segment := Seg;
    Segment.StartVA := VA;
    Segment.EndVA := VA + Len;
    Segment.UnitName := ModuleName;
    FSegments.Add(Segment);
  end;
end;

procedure TMapFileParser.ParsePublicsByValue(var P: PAnsiChar; EndP: PAnsiChar);
const
  Header: RawByteString = 'Publics by Value';
var
  Seg: Word;
  Offset, VA: UInt64;
  Name: string;
  SegIndex: Integer;
begin
  // Find a line containing "Publics by Value"
  if not SyncToHeader(P, EndP, Header) then
    Exit;

  // Skip empty lines after header
  while (P < EndP) and ((P^ = #13) or (P^ = #10)) do
    Inc(P);

  while (P < EndP) do
  begin
    SkipSpaces(P, EndP);
    if (P >= EndP) or not IsHexChar(P^) then
      Break;

    ReadAddress(P, EndP, Seg, Offset);
    SkipSpaces(P, EndP);
    Name := ReadRestOfLine(P, EndP);
    SkipToNextLine(P, EndP);

    if Name = '' then
      Continue;

    // Find matching segment class
    SegIndex := -1;
    for var I := 0 to FSegmentClasses.Count - 1 do
      if (FSegmentClasses[I].Segment = Seg) and (Offset < FSegmentClasses[I].Len) then
      begin
        SegIndex := I;
        Break;
      end;
    if SegIndex < 0 then
      Continue;

    if FSegmentClasses[SegIndex].IsTLS then
      VA := Offset
    else
      VA := MAPAddrToVA(Offset + FSegmentClasses[SegIndex].Start);

    var Proc: TMapProcName;
    Proc.Segment := Seg;
    Proc.VA := VA;
    Proc.Name := Name;
    FProcNames.Add(Proc);
  end;
end;

procedure TMapFileParser.ParseLineNumbers(var P: PAnsiChar; EndP: PAnsiChar);
const
  LineNumberPrefix: RawByteString = 'Line numbers for ';
var
  Seg: Word;
  Offset, VA: UInt64;
  LineNum: Integer;
  UnitName: string;
  SegIndex: Integer;
  DotPos: Integer;
begin
  while SyncToPrefix(P, EndP, LineNumberPrefix) do
  begin
    Inc(P, Length(LineNumberPrefix));
    // Read unit name until '('
    var Start := P;
    while (P < EndP) and (P^ <> '(') and (P^ <> #13) and (P^ <> #10) do
      Inc(P);
    SetString(UnitName, Start, P - Start);
    // Remove path prefix if present (extract last component before dot)
    DotPos := Pos('.', UnitName);
    if DotPos > 0 then
      UnitName := Copy(UnitName, 1, DotPos - 1);
    SkipToNextLine(P, EndP);

    // Skip empty lines
    while (P < EndP) and ((P^ = #13) or (P^ = #10)) do
      Inc(P);

    // Read line number entries: "lineNum SSSS:OOOOOOOO" (multiple per line)
    while (P < EndP) do
    begin
      // Skip whitespace including newlines between line number rows
      while (P < EndP) and ((P^ = ' ') or (P^ = #13) or (P^ = #10) or (P^ = #9)) do
        Inc(P);
      if (P >= EndP) or ((P^ < '0') or (P^ > '9')) then
        Break;

      // Read line number (decimal)
      LineNum := 0;
      while (P < EndP) and (P^ >= '0') and (P^ <= '9') do
      begin
        LineNum := LineNum * 10 + Ord(P^) - Ord('0');
        Inc(P);
      end;

      SkipSpaces(P, EndP);
      if (P >= EndP) or not IsHexChar(P^) then
        Break;

      ReadAddress(P, EndP, Seg, Offset);

      // Find matching segment class
      SegIndex := -1;
      for var I := 0 to FSegmentClasses.Count - 1 do
        if (FSegmentClasses[I].Segment = Seg) and (Offset < FSegmentClasses[I].Len) then
        begin
          SegIndex := I;
          Break;
        end;
      if SegIndex < 0 then
        Continue;

      if FSegmentClasses[SegIndex].IsTLS then
        VA := Offset
      else
        VA := MAPAddrToVA(Offset + FSegmentClasses[SegIndex].Start);

      if VA = 0 then
        Continue;

      var LN: TMapLineNumber;
      LN.Segment := Seg;
      LN.VA := VA;
      LN.LineNumber := LineNum;
      LN.UnitName := UnitName;
      FLineNumbers.Add(LN);
    end;
  end;
end;

function TMapFileParser.MAPAddrToVA(Addr: UInt64): UInt64;
begin
  if (FSegmentClasses.Count > 0) and (FSegmentClasses[0].Start > 0) and
     (Addr >= FSegmentClasses[0].Start) then
    Result := Addr - FSegmentClasses[0].Start
  else
    Result := Addr;
end;

function CompareLineNumbers(const A, B): Integer;
begin
  if TMapLineNumber(A).VA < TMapLineNumber(B).VA then Result := -1
  else if TMapLineNumber(A).VA > TMapLineNumber(B).VA then Result := 1
  else Result := 0;
end;

function CompareProcNames(const A, B): Integer;
begin
  if TMapProcName(A).VA < TMapProcName(B).VA then Result := -1
  else if TMapProcName(A).VA > TMapProcName(B).VA then Result := 1
  else Result := 0;
end;

function CompareSegments(const A, B): Integer;
begin
  if TMapSegment(A).EndVA < TMapSegment(B).EndVA then Result := -1
  else if TMapSegment(A).EndVA > TMapSegment(B).EndVA then Result := 1
  else Result := 0;
end;

procedure TMapFileParser.SortArrays;
begin
  FLineNumbers.Sort(CompareLineNumbers);
  FProcNames.Sort(CompareProcNames);
  FSegments.Sort(CompareSegments);
end;

function TMapFileParser.GetLineNumberByIndex(Index: Integer): TMapLineNumber;
begin
  Result := FLineNumbers[Index];
end;

function TMapFileParser.GetLineNumbersCnt: Integer;
begin
  Result := FLineNumbers.Count;
end;

function TMapFileParser.IndexOfSegment(Addr: UInt64): Integer;
var
  L, R: Integer;
begin
  R := FSegments.Count - 1;
  Result := FLastSegmentIndex;
  if (Result >= 0) and (Result <= R) then
    if (FSegments[Result].StartVA <= Addr) and (Addr < FSegments[Result].EndVA) then
      Exit;

  L := 0;
  while L <= R do
  begin
    Result := L + (R - L) div 2;
    if Addr >= FSegments[Result].EndVA then
      L := Result + 1
    else
    begin
      R := Result - 1;
      if (FSegments[Result].StartVA <= Addr) and (Addr < FSegments[Result].EndVA) then
      begin
        FLastSegmentIndex := Result;
        Exit;
      end;
    end;
  end;
  Result := -1;
end;

function TMapFileParser.ModuleStartFromAddr(Addr: UInt64): UInt64;
var
  I: Integer;
begin
  I := IndexOfSegment(Addr);
  if I <> -1 then
    Result := FSegments[I].StartVA
  else
    Result := UInt64(-1);
end;

function TMapFileParser.ModuleNameFromAddr(Addr: UInt64): string;
var
  I: Integer;
begin
  I := IndexOfSegment(Addr);
  if I <> -1 then
    Result := FSegments[I].UnitName
  else
    Result := '';
end;

function TMapFileParser.ProcNameFromAddr(Addr: UInt64): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(Addr, Dummy);
end;

function TMapFileParser.ProcNameFromAddr(Addr: UInt64; out Offset: Integer): string;
var
  L, R, M: Integer;
  ModStart: UInt64;
begin
  Result := '';
  Offset := 0;
  ModStart := ModuleStartFromAddr(Addr);

  // Binary search: find largest VA <= Addr
  L := 0;
  R := FProcNames.Count - 1;
  M := -1;
  while L <= R do
  begin
    var Mid := L + (R - L) div 2;
    if FProcNames[Mid].VA <= Addr then
    begin
      M := Mid;
      L := Mid + 1;
    end
    else
      R := Mid - 1;
  end;

  if (M >= 0) and (FProcNames[M].VA >= ModStart) then
  begin
    Result := FProcNames[M].Name;
    Offset := Integer(Addr - FProcNames[M].VA);
  end;
end;

function TMapFileParser.LineNumberFromAddr(Addr: UInt64): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(Addr, Dummy);
end;

function TMapFileParser.LineNumberFromAddr(Addr: UInt64; out Offset: Integer): Integer;
var
  L, R, M: Integer;
  ModStart: UInt64;
begin
  Result := 0;
  Offset := 0;
  ModStart := ModuleStartFromAddr(Addr);

  // Binary search: find largest VA <= Addr
  L := 0;
  R := FLineNumbers.Count - 1;
  M := -1;
  while L <= R do
  begin
    var Mid := L + (R - L) div 2;
    if FLineNumbers[Mid].VA <= Addr then
    begin
      M := Mid;
      L := Mid + 1;
    end
    else
      R := Mid - 1;
  end;

  if (M >= 0) and (FLineNumbers[M].VA >= ModStart) then
  begin
    Result := FLineNumbers[M].LineNumber;
    Offset := Integer(Addr - FLineNumbers[M].VA);
  end;
end;

function TMapFileParser.VAFromUnitAndProcName(const AUnitName, AProcName: string): UInt64;
var
  QualifiedName: string;
begin
  Result := 0;
  QualifiedName := AUnitName + '.' + AProcName;
  for var I := 0 to FProcNames.Count - 1 do
    if SameText(FProcNames[I].Name, QualifiedName) then
      Exit(FProcNames[I].VA);
end;

end.
