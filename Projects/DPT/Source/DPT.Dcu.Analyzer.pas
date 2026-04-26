// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Analyzer;

interface

uses

  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Reader,
  DPT.Dcu.Types;

type

  /// <summary>
  ///   Iteration-1 DCU analyzer. Decodes only the parts of a DCU file that
  ///   the project has empirically validated across multiple Delphi
  ///   versions: magic bytes, the embedded source file references and
  ///   the interface uses table. Anything beyond that is intentionally
  ///   left for later iterations because the DCU format is undocumented
  ///   and version-specific.
  /// </summary>
  TDcuAnalyzer = class
  public
    const
      /// <summary>Number of header bytes the source-reference scan inspects.</summary>
      SourceScanWindow = 1024;

      /// <summary>Number of bytes returned in <c>FirstBytesPreview</c>.</summary>
      PreviewByteCount = 64;

      /// <summary>
      ///   Hard cap on how many uses entries to extract before bailing out;
      ///   acts as a runaway-loop guard. Real units rarely exceed a few
      ///   dozen; the cap is intentionally generous.
      ///   </summary>
      MaxUsesEntries = 1024;

      /// <summary>Hard cap on how many symbol references to record.</summary>
      MaxSymbolRefs = 65536;
  private
    class function ExtractUnitName(const ASourceFileName: string): string;
    class function HasKnownSourceExtension(const AFileName: string): Boolean;
    class function MagicHex(const ABytes: array of Byte): string;
    class function CompilerFromMagic(const ABytes: array of Byte): TDcuKnownCompiler;
    class function PlatformFromMagic(const ABytes: array of Byte): TDcuPlatform;
    class function IsValidUnitNameChar(AByte: Byte): Boolean;
    class function IsBuiltinNonUnitName(const AName: string): Boolean;
    class function LooksLikeUnitName(const AName: string): Boolean;
    class procedure ScanSourceReferences(AReader: TDcuReader;
      var AHeader: TDcuHeaderInfo; ADiagnostics: IList<string>);
    class procedure ScanUsesTable(AReader: TDcuReader;
      var AResult: TDcuAnalysisResult);
    class function LooksLikeSymbolName(const AName: string;
      AKind: TDcuSymbolKind): Boolean;
    class procedure ScanSymbolRefs(AReader: TDcuReader;
      var AResult: TDcuAnalysisResult);
  public
    class function Analyze(const AFilePath: string): TDcuAnalysisResult; overload;
    class function Analyze(const AContent: TBytes;
      const AFilePath: string = ''): TDcuAnalysisResult; overload;
  end;

implementation

{ TDcuAnalyzer }

class function TDcuAnalyzer.HasKnownSourceExtension(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));
  for var Known in DcuSourceExtensions do
    if Ext = Known then
      Exit(True);
  Result := False;
end;

class function TDcuAnalyzer.ExtractUnitName(const ASourceFileName: string): string;
begin
  // The compiler may store either a bare filename ("DPT.Detection.pas") or
  // an absolute path ("C:\...\DPT.Detection.pas") depending on how dcc was
  // invoked. Strip directory + extension to recover the unit name.
  Result := ChangeFileExt(ExtractFileName(ASourceFileName), '');
end;

class function TDcuAnalyzer.MagicHex(const ABytes: array of Byte): string;
begin
  Result := Format('%.2X %.2X %.2X %.2X', [ABytes[0], ABytes[1], ABytes[2], ABytes[3]]);
end;

class function TDcuAnalyzer.CompilerFromMagic(const ABytes: array of Byte): TDcuKnownCompiler;
begin
  // Empirical: byte 3 increments per compiler version. Only values that
  // have actually been observed in the multi-version test corpus are
  // mapped here; everything else stays dccUnknown so a future compiler's
  // DCU is honestly reported as unidentified instead of misclassified.
  if ABytes[0] <> DcuMagicByte_Modern then
    Exit(dccUnknown);
  case ABytes[3] of
    DcuVersionByte_Delphi11: Result := dccDelphi11;
    DcuVersionByte_Delphi12: Result := dccDelphi12;
    DcuVersionByte_Delphi13: Result := dccDelphi13;
  else
    Result := dccUnknown;
  end;
end;

class function TDcuAnalyzer.PlatformFromMagic(const ABytes: array of Byte): TDcuPlatform;
begin
  if ABytes[0] <> DcuMagicByte_Modern then
    Exit(dpUnknown);
  case ABytes[1] of
    DcuPlatformByte_Win32: Result := dpWin32;
    DcuPlatformByte_Win64: Result := dpWin64;
  else
    Result := dpUnknown;
  end;
end;

class procedure TDcuAnalyzer.ScanSourceReferences(AReader: TDcuReader;
  var AHeader: TDcuHeaderInfo; ADiagnostics: IList<string>);
var
  CandidateName : string;
  Consumed      : Integer;
  EndScan       : Integer;
  LastSourceOfs : Integer;
  Offset        : Integer;
  Timestamp     : UInt32;
  UsesAnchorOfs : Integer;
begin
  AHeader.IncludeSources := Collections.NewPlainList<TDcuSourceRef>;
  EndScan := AReader.Size;
  if EndScan > SourceScanWindow then
    EndScan := SourceScanWindow;

  // Stop the source-reference scan at the first $63 $64 / $63 $65 marker
  // (uses-table start) so its byte stream does not bleed into source
  // references.
  UsesAnchorOfs := AReader.IndexOf([DcuUsesTag_Marker, DcuUsesTag_InterfaceScope], 0);
  if (UsesAnchorOfs <= 0) or (UsesAnchorOfs >= EndScan) then
    UsesAnchorOfs := AReader.IndexOf([DcuUsesTag_Marker, DcuUsesTag_ImplScope], 0);
  if (UsesAnchorOfs > 0) and (UsesAnchorOfs < EndScan) then
    EndScan := UsesAnchorOfs;

  LastSourceOfs := -1;
  Offset := 0;
  while Offset < EndScan do
  begin
    if not AReader.TryReadShortStringAt(Offset, CandidateName, Consumed) then
    begin
      Inc(Offset);
      Continue;
    end;
    if HasKnownSourceExtension(CandidateName) then
    begin
      // 4-byte timestamp follows the string in the modern layout
      if Offset + Consumed + 4 <= AReader.Size then
      begin
        Timestamp :=
          UInt32(AReader.Bytes[Offset + Consumed])              or
          (UInt32(AReader.Bytes[Offset + Consumed + 1]) shl  8) or
          (UInt32(AReader.Bytes[Offset + Consumed + 2]) shl 16) or
          (UInt32(AReader.Bytes[Offset + Consumed + 3]) shl 24);
      end
      else
        Timestamp := 0;
      AHeader.IncludeSources.Add(TDcuSourceRef.Create(CandidateName, Timestamp, Offset));
      LastSourceOfs := AHeader.IncludeSources.Count - 1;
      Inc(Offset, Consumed + 4);
      Continue;
    end;
    Inc(Offset);
  end;

  if LastSourceOfs >= 0 then
  begin
    AHeader.PrimarySource := AHeader.IncludeSources[LastSourceOfs];
    AHeader.IncludeSources.Delete(LastSourceOfs);
    AHeader.UnitName := ExtractUnitName(AHeader.PrimarySource.FileName);
  end
  else
  begin
    ADiagnostics.Add('No source file reference found in the first '
      + IntToStr(EndScan) + ' bytes of the DCU header');
    AHeader.PrimarySource := TDcuSourceRef.Create('', 0, -1);
    AHeader.UnitName := '';
  end;
end;

class function TDcuAnalyzer.IsValidUnitNameChar(AByte: Byte): Boolean;
begin
  // Pascal qualified identifiers: A..Z, a..z, 0..9, '_', '.'.
  // No spaces, no quotes, no operators.
  Result :=
    ((AByte >= Ord('A')) and (AByte <= Ord('Z'))) or
    ((AByte >= Ord('a')) and (AByte <= Ord('z'))) or
    ((AByte >= Ord('0')) and (AByte <= Ord('9'))) or
    (AByte = Ord('_')) or
    (AByte = Ord('.'));
end;

class function TDcuAnalyzer.IsBuiltinNonUnitName(const AName: string): Boolean;
var
  Builtin: string;
begin
  Result := False;
  for Builtin in DcuBuiltinNonUnitNames do
    if SameText(Builtin, AName) then
      Exit(True);
end;

class function TDcuAnalyzer.LooksLikeUnitName(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Length(AName) = 0) or (Length(AName) > DcuUsesEntryMaxLen) then
    Exit;
  // Must start with a letter or underscore (not a digit, not a dot).
  case AName[1] of
    'A'..'Z', 'a'..'z', '_': ;
  else
    Exit;
  end;
  // Every character must be a valid identifier character; no two adjacent
  // dots; must not end on a dot.
  for I := 1 to Length(AName) do
    if not IsValidUnitNameChar(Ord(AName[I])) then
      Exit;
  if AName[Length(AName)] = '.' then
    Exit;
  for I := 1 to Length(AName) - 1 do
    if (AName[I] = '.') and (AName[I + 1] = '.') then
      Exit;
  // Reject built-in type names that pass all syntactic checks but never
  // refer to a real unit. These are the typical false-positive matches
  // when bytes happen to align in the symbol section.
  if IsBuiltinNonUnitName(AName) then
    Exit;
  Result := True;
end;

class procedure TDcuAnalyzer.ScanUsesTable(AReader: TDcuReader;
  var AResult: TDcuAnalysisResult);
var
  Consumed   : Integer;
  Name       : string;
  Offset     : Integer;
  Scope      : TDcuUsesScope;
  ScopeByte  : Byte;
  TrailerOk  : Boolean;
  TotalParsed: Integer;
begin
  AResult.UsesParsed := False;
  TotalParsed := 0;
  Offset := 0;

  while (Offset < AReader.Size - 3) and (TotalParsed < MaxUsesEntries) do
  begin
    if AReader.Bytes[Offset] <> DcuUsesTag_Marker then
    begin
      Inc(Offset);
      Continue;
    end;

    ScopeByte := AReader.Bytes[Offset + 1];
    case ScopeByte of
      DcuUsesTag_InterfaceScope: Scope := dusInterface;
      DcuUsesTag_ImplScope     : Scope := dusImplementation;
    else
      Inc(Offset);
      Continue;
    end;

    if not AReader.TryReadShortStringAt(Offset + 2, Name, Consumed) then
    begin
      Inc(Offset);
      Continue;
    end;
    if not LooksLikeUnitName(Name) then
    begin
      Inc(Offset);
      Continue;
    end;

    // The 3 bytes immediately after the name must all be zero in the
    // observed layout. This is the strongest filter against false
    // positives where a $63 byte happens to appear inside a longer
    // textual symbol name.
    if Offset + 2 + Consumed + 3 > AReader.Size then
    begin
      Inc(Offset);
      Continue;
    end;
    TrailerOk :=
      (AReader.Bytes[Offset + 2 + Consumed]     = 0) and
      (AReader.Bytes[Offset + 2 + Consumed + 1] = 0) and
      (AReader.Bytes[Offset + 2 + Consumed + 2] = 0);
    if not TrailerOk then
    begin
      Inc(Offset);
      Continue;
    end;

    case Scope of
      dusInterface     : AResult.InterfaceUses.Add(
        TDcuUsesEntry.Create(Name, Scope, Offset));
      dusImplementation: AResult.ImplementationUses.Add(
        TDcuUsesEntry.Create(Name, Scope, Offset));
    end;
    Inc(TotalParsed);
    Inc(Offset, 2 + Consumed + 3);
  end;

  AResult.UsesParsed :=
    (AResult.InterfaceUses.Count > 0) or (AResult.ImplementationUses.Count > 0);
end;

class function TDcuAnalyzer.LooksLikeSymbolName(const AName: string;
  AKind: TDcuSymbolKind): Boolean;
var
  I    : Integer;
  Start: Integer;
begin
  Result := False;
  if (Length(AName) = 0) or (Length(AName) > DcuSymbolRefMaxLen) then
    Exit;

  // Method names occasionally appear with a leading dot (anchored to
  // the previously emitted owning type, e.g. ".TDptBuildTask"). Type
  // refs never start with a dot.
  Start := 1;
  if (AKind = dskMethod) and (AName[1] = '.') then
  begin
    if Length(AName) = 1 then
      Exit;
    Start := 2;
  end;

  // First non-dot character must be a letter or underscore.
  case AName[Start] of
    'A'..'Z', 'a'..'z', '_': ;
  else
    Exit;
  end;

  // Every character must belong to a Pascal qualified identifier.
  for I := Start to Length(AName) do
    if not IsValidUnitNameChar(Ord(AName[I])) then
      Exit;
  // No double dots and no trailing dot.
  if AName[Length(AName)] = '.' then
    Exit;
  for I := Start to Length(AName) - 1 do
    if (AName[I] = '.') and (AName[I + 1] = '.') then
      Exit;

  Result := True;
end;

class procedure TDcuAnalyzer.ScanSymbolRefs(AReader: TDcuReader;
  var AResult: TDcuAnalysisResult);
var
  Consumed : Integer;
  Hash     : UInt32;
  Kind     : TDcuSymbolKind;
  Name     : string;
  Offset   : Integer;
  Seen     : IKeyValue<string, Boolean>;
  TagByte  : Byte;
  TrailerOk: Boolean;

  function MatchUsesEntryAt(AOffset: Integer; out AEntryLen: Integer): Boolean;
  var
    UCons: Integer;
    UName: string;
  begin
    AEntryLen := 0;
    Result := False;
    if (AOffset + 2 >= AReader.Size) then Exit;
    if AReader.Bytes[AOffset] <> DcuUsesTag_Marker then Exit;
    if not (AReader.Bytes[AOffset + 1] in
      [DcuUsesTag_InterfaceScope, DcuUsesTag_ImplScope]) then Exit;
    if not AReader.TryReadShortStringAt(AOffset + 2, UName, UCons) then Exit;
    if not LooksLikeUnitName(UName) then Exit;
    if AOffset + 2 + UCons + 3 > AReader.Size then Exit;
    if (AReader.Bytes[AOffset + 2 + UCons]     <> 0) or
       (AReader.Bytes[AOffset + 2 + UCons + 1] <> 0) or
       (AReader.Bytes[AOffset + 2 + UCons + 2] <> 0) then Exit;
    AEntryLen := 2 + UCons + 3;
    Result := True;
  end;

var
  UsesLen: Integer;
begin
  AResult.SymbolsParsed := False;
  Seen := Collections.NewPlainKeyValue<string, Boolean>;
  Offset := 0;

  while (Offset < AReader.Size - 1) and (AResult.Symbols.Count < MaxSymbolRefs) do
  begin
    // Walk past valid uses entries verbatim so a $66/$67 byte that
    // happens to live inside a uses entry's hash trailer does not get
    // misread as a symbol-ref tag.
    if MatchUsesEntryAt(Offset, UsesLen) then
    begin
      Inc(Offset, UsesLen);
      Continue;
    end;

    TagByte := AReader.Bytes[Offset];
    if TagByte = DcuSymbolTag_Type then
      Kind := dskType
    else if TagByte = DcuSymbolTag_Method then
      Kind := dskMethod
    else
    begin
      Inc(Offset);
      Continue;
    end;

    if not AReader.TryReadShortStringAt(Offset + 1, Name, Consumed) then
    begin
      Inc(Offset);
      Continue;
    end;
    if not LooksLikeSymbolName(Name, Kind) then
    begin
      Inc(Offset);
      Continue;
    end;
    // 4-byte hash/CRC trailer is unconstrained but must fit in the file.
    TrailerOk := Offset + 1 + Consumed + 4 <= AReader.Size;
    if not TrailerOk then
    begin
      Inc(Offset);
      Continue;
    end;
    Hash :=
      UInt32(AReader.Bytes[Offset + 1 + Consumed])              or
      (UInt32(AReader.Bytes[Offset + 1 + Consumed + 1]) shl  8) or
      (UInt32(AReader.Bytes[Offset + 1 + Consumed + 2]) shl 16) or
      (UInt32(AReader.Bytes[Offset + 1 + Consumed + 3]) shl 24);

    // First-encounter dedup: a single type/method may be referenced
    // many times in the file - we record only the earliest occurrence.
    var Key := DcuSymbolKindName[Kind] + ':' + Name;
    if not Seen.ContainsKey(Key) then
    begin
      Seen.Add(Key, True);
      AResult.Symbols.Add(TDcuSymbolRef.Create(Kind, Name, Hash, Offset));
    end;
    Inc(Offset, 1 + Consumed + 4);
  end;

  AResult.SymbolsParsed := AResult.Symbols.Count > 0;
end;

class function TDcuAnalyzer.Analyze(const AFilePath: string): TDcuAnalysisResult;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(AFilePath);
  try
    Result := Analyze(Reader.Content, AFilePath);
  finally
    Reader.Free;
  end;
end;

class function TDcuAnalyzer.Analyze(const AContent: TBytes;
  const AFilePath: string): TDcuAnalysisResult;
var
  Reader: TDcuReader;
begin
  Result := Default(TDcuAnalysisResult);
  Result.FilePath := AFilePath;
  Result.FileSize := Length(AContent);
  Result.InterfaceUses := Collections.NewPlainList<TDcuUsesEntry>;
  Result.ImplementationUses := Collections.NewPlainList<TDcuUsesEntry>;
  Result.Symbols := Collections.NewPlainList<TDcuSymbolRef>;
  Result.Diagnostics := Collections.NewList<string>;
  Result.Header.IncludeSources := Collections.NewPlainList<TDcuSourceRef>;

  if Length(AContent) < 16 then
  begin
    Result.IsDcu := False;
    Result.Diagnostics.Add('File is too small to be a DCU (' + IntToStr(Length(AContent))
      + ' bytes)');
    Exit;
  end;

  Reader := TDcuReader.Create(AContent);
  try
    Result.Header.MagicBytes[0] := Reader.Bytes[0];
    Result.Header.MagicBytes[1] := Reader.Bytes[1];
    Result.Header.MagicBytes[2] := Reader.Bytes[2];
    Result.Header.MagicBytes[3] := Reader.Bytes[3];
    Result.Header.MagicHex := MagicHex(Result.Header.MagicBytes);
    Result.Header.DetectedCompiler := CompilerFromMagic(Result.Header.MagicBytes);
    Result.Header.DetectedPlatform := PlatformFromMagic(Result.Header.MagicBytes);

    Result.IsDcu := Result.Header.MagicBytes[0] = DcuMagicByte_Modern;
    if not Result.IsDcu then
      Result.Diagnostics.Add(Format('Magic byte $%.2X is not a known DCU marker',
        [Result.Header.MagicBytes[0]]));

    Result.FirstBytesPreview := Reader.HexPreview(0, PreviewByteCount);

    ScanSourceReferences(Reader, Result.Header, Result.Diagnostics);
    ScanUsesTable(Reader, Result);
    ScanSymbolRefs(Reader, Result);
  finally
    Reader.Free;
  end;
end;

end.
