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
      ///   The "00 64" block in modern DCUs has been observed to contain a
      ///   single implicit System reference followed by the symbol section.
      ///   Reading more entries here would simply mis-classify type symbols
      ///   ("string", "Integer", ...) as units. Iteration 1 therefore caps
      ///   the extraction at one entry and treats this list as the implicit
      ///   System reference rather than the full interface uses table; the
      ///   real uses-with-source information is exposed via the source file
      ///   references in the Header section.
      /// </summary>
      MaxUsesEntries = 1;
  private
    class function ExtractUnitName(const ASourceFileName: string): string;
    class function HasKnownSourceExtension(const AFileName: string): Boolean;
    class function MagicHex(const ABytes: array of Byte): string;
    class function CompilerFromMagic(const ABytes: array of Byte): TDcuKnownCompiler;
    class procedure ScanSourceReferences(AReader: TDcuReader;
      var AHeader: TDcuHeaderInfo; ADiagnostics: IList<string>);
    class procedure ScanInterfaceUses(AReader: TDcuReader;
      AStartOffset: Integer; var AResult: TDcuAnalysisResult);
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
  // The empirical mapping is intentionally narrow in iteration 1.
  // The multi-Delphi-version test suite is responsible for filling in
  // additional rows here as more compiler versions get observed.
  if ABytes[0] = DcuMagicByte_Modern then
    Result := dccUnknown // recognised as DCU but compiler not yet pinned
  else
    Result := dccUnknown;
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

  // Stop the scan at the first uses-anchor occurrence so includes that
  // happen to embed a printable byte stream do not bleed into uses data.
  UsesAnchorOfs := AReader.IndexOf(DcuUsesAnchor_Modern, 0);
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

class procedure TDcuAnalyzer.ScanInterfaceUses(AReader: TDcuReader;
  AStartOffset: Integer; var AResult: TDcuAnalysisResult);
var
  Consumed: Integer;
  CRC     : UInt32;
  Name    : string;
  Offset  : Integer;
begin
  AResult.UsesParsed := False;
  AResult.UsesParseStopAt := AStartOffset;
  if AStartOffset < 0 then
  begin
    AResult.Diagnostics.Add('Interface uses anchor not found; uses table not parsed');
    Exit;
  end;

  // Anchor sequence is "00 64"; entries start right after.
  Offset := AStartOffset + Length(DcuUsesAnchor_Modern);
  while (Offset < AReader.Size) and (AResult.InterfaceUses.Count < MaxUsesEntries) do
  begin
    if not AReader.TryReadShortStringAt(Offset, Name, Consumed) then
      Break;
    if Offset + Consumed + 4 > AReader.Size then
      Break;
    CRC :=
      UInt32(AReader.Bytes[Offset + Consumed])              or
      (UInt32(AReader.Bytes[Offset + Consumed + 1]) shl  8) or
      (UInt32(AReader.Bytes[Offset + Consumed + 2]) shl 16) or
      (UInt32(AReader.Bytes[Offset + Consumed + 3]) shl 24);
    AResult.InterfaceUses.Add(TDcuUsesEntry.Create(Name, CRC));
    Inc(Offset, Consumed + 4);
  end;

  AResult.UsesParseStopAt := Offset;
  AResult.UsesParsed := AResult.InterfaceUses.Count > 0;
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
  Reader        : TDcuReader;
  UsesAnchorOfs : Integer;
begin
  Result := Default(TDcuAnalysisResult);
  Result.FilePath := AFilePath;
  Result.FileSize := Length(AContent);
  Result.InterfaceUses := Collections.NewPlainList<TDcuUsesEntry>;
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

    Result.IsDcu := Result.Header.MagicBytes[0] = DcuMagicByte_Modern;
    if not Result.IsDcu then
      Result.Diagnostics.Add(Format('Magic byte $%.2X is not a known DCU marker',
        [Result.Header.MagicBytes[0]]));

    Result.FirstBytesPreview := Reader.HexPreview(0, PreviewByteCount);

    ScanSourceReferences(Reader, Result.Header, Result.Diagnostics);

    UsesAnchorOfs := Reader.IndexOf(DcuUsesAnchor_Modern, 0);
    ScanInterfaceUses(Reader, UsesAnchorOfs, Result);
  finally
    Reader.Free;
  end;
end;

end.
