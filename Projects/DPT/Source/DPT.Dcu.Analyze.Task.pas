// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Analyze.Task;

interface

uses

  System.Classes,

  DPT.Dcu.Types,
  DPT.Task,
  DPT.Types;

type

  TDcuAnalyzeFormat = (dafText, dafJson);

  TDcuAnalyzeSection = (dasHeader, dasUses, dasSymbols, dasSections);
  TDcuAnalyzeSections = set of TDcuAnalyzeSection;

  TDptDcuAnalyzeTask = class(TDptTaskBase)
  private
    FDcuFile    : string;
    FFormat     : TDcuAnalyzeFormat;
    FResolve    : Boolean;
    FSearchPaths: TArray<string>;
    FSections   : TDcuAnalyzeSections;
    FVerbose    : Boolean;
    function  AutoSearchPathsFor(const AResult: TDcuAnalysisResult): TArray<string>;
    procedure RenderJson(const AResult: TDcuAnalysisResult; ALines: TStrings);
    procedure RenderText(const AResult: TDcuAnalysisResult; ALines: TStrings);
  public
    constructor Create; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
    property DcuFile: string read FDcuFile write FDcuFile;
    property Format: TDcuAnalyzeFormat read FFormat write FFormat;
    property Resolve: Boolean read FResolve write FResolve;
    property SearchPaths: TArray<string> read FSearchPaths write FSearchPaths;
    property Sections: TDcuAnalyzeSections read FSections write FSections;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

uses

  System.StrUtils,
  System.SysUtils,
  System.Types,

  JclIDEUtils,

  DPT.Dcu.Analyzer;

const
  AllSections: TDcuAnalyzeSections = [dasHeader, dasUses, dasSymbols, dasSections];

function JsonEscape(const S: string): string;
var
  Sb: TStringBuilder;
  Ch: Char;
begin
  Sb := TStringBuilder.Create(Length(S) + 2);
  try
    for Ch in S do
      case Ch of
        '"' : Sb.Append('\"');
        '\' : Sb.Append('\\');
        #8  : Sb.Append('\b');
        #9  : Sb.Append('\t');
        #10 : Sb.Append('\n');
        #12 : Sb.Append('\f');
        #13 : Sb.Append('\r');
      else
        if Ord(Ch) < 32 then
          Sb.Append(System.SysUtils.Format('\u%.4x', [Ord(Ch)]))
        else
          Sb.Append(Ch);
      end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function JsonStr(const S: string): string;
begin
  Result := '"' + JsonEscape(S) + '"';
end;

function CompilerToIdeVersion(ACompiler: TDcuKnownCompiler): Integer;
begin
  case ACompiler of
    dccDelphi11: Result := 22;
    dccDelphi12: Result := 23;
    dccDelphi13: Result := 37;
  else
    Result := 0;
  end;
end;

function PlatformToJclPlatform(APlatform: TDcuPlatform;
  out AJclPlatform: TJclBDSPlatform): Boolean;
begin
  Result := True;
  case APlatform of
    dpWin32: AJclPlatform := bpWin32;
    dpWin64: AJclPlatform := bpWin64;
  else
    Result := False;
  end;
end;

{ TDptDcuAnalyzeTask }

function TDptDcuAnalyzeTask.AutoSearchPathsFor(
  const AResult: TDcuAnalysisResult): TArray<string>;
var
  IdeVer       : Integer;
  Installation : TJclBorRADToolInstallation;
  Installations: TJclBorRADToolInstallations;
  JclPlatform  : TJclBDSPlatform;
  Parts        : TStringDynArray;
  RawPath      : string;
  S            : string;
begin
  // Auto-derive the Delphi RTL search path from the magic-byte-detected
  // (compiler, platform) tuple. Anything we cannot identify (unknown
  // compiler, unknown platform, version not installed on this machine)
  // returns an empty array so the resolver falls back to the explicit
  // --SearchPath list and the DCU's own directory only.
  Result := nil;

  IdeVer := CompilerToIdeVersion(AResult.Header.DetectedCompiler);
  if IdeVer = 0 then
    Exit;
  if not PlatformToJclPlatform(AResult.Header.DetectedPlatform, JclPlatform) then
    Exit;

  Installations := TJclBorRADToolInstallations.Create;
  try
    if not Installations.DelphiVersionInstalled[IdeVer] then
      Exit;
    Installation := Installations.DelphiInstallationFromVersion[IdeVer];
    RawPath := Installation.LibrarySearchPath[JclPlatform];
    if RawPath = '' then
      Exit;
    Parts := SplitString(RawPath, ';');
    for S in Parts do
      if Trim(S) <> '' then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := ExcludeTrailingPathDelimiter(Trim(S));
      end;
  finally
    Installations.Free;
  end;
end;

constructor TDptDcuAnalyzeTask.Create;
begin
  inherited;
  FFormat := dafText;
  FSections := AllSections;
  FVerbose := False;
  FResolve := False;
  FSearchPaths := nil;
end;

procedure TDptDcuAnalyzeTask.Parse(CmdLine: TCmdLineConsumer);
var
  ExplicitSections: Boolean;
  Param           : string;
  ValueLower      : string;
begin
  FDcuFile := '';
  FFormat := dafText;
  FSections := [];
  ExplicitSections := False;

  while CmdLine.HasParameter do
  begin
    Param := CmdLine.CheckParameter('Option/DcuFile');

    if SameText(Param, '--Header') then
    begin
      FSections := FSections + [dasHeader];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Uses') then
    begin
      FSections := FSections + [dasUses];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Symbols') then
    begin
      FSections := FSections + [dasSymbols];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Sections') then
    begin
      FSections := FSections + [dasSections];
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--All') then
    begin
      FSections := AllSections;
      ExplicitSections := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--Format=', True) then
    begin
      ValueLower := LowerCase(Param.Substring(9).DeQuotedString('"'));
      if ValueLower = 'text' then
        FFormat := dafText
      else if ValueLower = 'json' then
        FFormat := dafJson
      else
        CmdLine.InvalidParameter('Unknown --Format value: ' + ValueLower);
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Verbose') then
    begin
      FVerbose := True;
      CmdLine.ConsumeParameter;
    end
    else if SameText(Param, '--Resolve') then
    begin
      FResolve := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--SearchPath=', True) then
    begin
      var RawValue := Param.Substring(13).DeQuotedString('"');
      var Parts := SplitString(RawValue, ';');
      for var P in Parts do
        if Trim(P) <> '' then
        begin
          SetLength(FSearchPaths, Length(FSearchPaths) + 1);
          FSearchPaths[High(FSearchPaths)] :=
            ExcludeTrailingPathDelimiter(ExpandFileName(Trim(P)));
        end;
      // Setting a search path implies resolution; the user only mentions
      // them when they want lookups to actually happen.
      FResolve := True;
      CmdLine.ConsumeParameter;
    end
    else if Param.StartsWith('--') then
    begin
      CmdLine.InvalidParameter('Unknown option: ' + Param);
    end
    else
    begin
      if FDcuFile = '' then
      begin
        FDcuFile := ExpandFileName(Param);
        CmdLine.ConsumeParameter;
      end
      else
        CmdLine.InvalidParameter('Unexpected positional argument: ' + Param);
    end;
  end;

  if not ExplicitSections then
    FSections := AllSections;

  if FDcuFile = '' then
    raise EInvalidParameter.Create('DcuAnalyze requires <DcuFile> argument');
end;

procedure TDptDcuAnalyzeTask.RenderText(const AResult: TDcuAnalysisResult;
  ALines: TStrings);
var
  IncludeSrc: TDcuSourceRef;
  UsesEntry : TDcuUsesEntry;
begin
  ALines.Add('File:      ' + AResult.FilePath);
  ALines.Add('Size:      ' + IntToStr(AResult.FileSize) + ' bytes');
  if AResult.IsDcu then
    ALines.Add('IsDcu:     yes')
  else
    ALines.Add('IsDcu:     no');

  if dasHeader in FSections then
  begin
    ALines.Add('');
    ALines.Add('[Header]');
    ALines.Add('  Magic:    ' + AResult.Header.MagicHex);
    ALines.Add('  Compiler: ' + DcuKnownCompilerName[AResult.Header.DetectedCompiler]);
    ALines.Add('  Platform: ' + DcuPlatformName[AResult.Header.DetectedPlatform]);
    if AResult.Header.UnitName <> '' then
      ALines.Add('  Unit:     ' + AResult.Header.UnitName);
    if AResult.Header.PrimarySource.FileName <> '' then
      ALines.Add('  Source:   ' + AResult.Header.PrimarySource.FileName);
    if AResult.Header.IncludeSources.Count > 0 then
    begin
      ALines.Add('  Includes (' + IntToStr(AResult.Header.IncludeSources.Count) + '):');
      for IncludeSrc in AResult.Header.IncludeSources do
        ALines.Add('    - ' + IncludeSrc.FileName);
    end;
    if FVerbose then
      ALines.Add('  Preview:  ' + AResult.FirstBytesPreview);
  end;

  if dasUses in FSections then
  begin
    ALines.Add('');
    ALines.Add('[Interface uses] (' + IntToStr(AResult.InterfaceUses.Count) + ')');
    if AResult.InterfaceUses.Count = 0 then
      ALines.Add('  (none detected)')
    else
      for UsesEntry in AResult.InterfaceUses do
      begin
        if FResolve and (UsesEntry.ResolvedPath <> '') then
          ALines.Add(System.SysUtils.Format('  %-30s -> %s',
            [UsesEntry.UnitName, UsesEntry.ResolvedPath]))
        else if FResolve then
          ALines.Add(System.SysUtils.Format('  %-30s -> (unresolved)',
            [UsesEntry.UnitName]))
        else
          ALines.Add('  ' + UsesEntry.UnitName);
      end;

    ALines.Add('');
    ALines.Add('[Implementation uses] (' + IntToStr(AResult.ImplementationUses.Count) + ')');
    if AResult.ImplementationUses.Count = 0 then
      ALines.Add('  (none detected)')
    else
      for UsesEntry in AResult.ImplementationUses do
      begin
        if FResolve and (UsesEntry.ResolvedPath <> '') then
          ALines.Add(System.SysUtils.Format('  %-30s -> %s',
            [UsesEntry.UnitName, UsesEntry.ResolvedPath]))
        else if FResolve then
          ALines.Add(System.SysUtils.Format('  %-30s -> (unresolved)',
            [UsesEntry.UnitName]))
        else
          ALines.Add('  ' + UsesEntry.UnitName);
      end;
  end;

  if dasSymbols in FSections then
  begin
    ALines.Add('');
    var ImpTypeCount := 0;
    var ImpMethodCount := 0;
    var ExpTypeCount := 0;
    var ExpRoutineCount := 0;
    for var Sym in AResult.Symbols do
      case Sym.Origin of
        dsoImported:
          case Sym.Kind of
            dskType  : Inc(ImpTypeCount);
            dskMethod: Inc(ImpMethodCount);
          end;
        dsoExported:
          case Sym.Kind of
            dskType  : Inc(ExpTypeCount);
            dskMethod: Inc(ExpRoutineCount);
          end;
      end;

    ALines.Add(System.SysUtils.Format(
      '[Symbols] imported %d types + %d methods, exported %d types + %d routines',
      [ImpTypeCount, ImpMethodCount, ExpTypeCount, ExpRoutineCount]));
    if AResult.Symbols.Count = 0 then
      ALines.Add('  (none detected)')
    else
    begin
      if ExpTypeCount > 0 then
      begin
        ALines.Add('  Exported types (declared by this unit):');
        for var Sym in AResult.Symbols do
          if (Sym.Origin = dsoExported) and (Sym.Kind = dskType) then
            ALines.Add('    ' + Sym.Name);
      end;
      if ExpRoutineCount > 0 then
      begin
        ALines.Add('  Exported routines (functions / methods declared here):');
        for var Sym in AResult.Symbols do
          if (Sym.Origin = dsoExported) and (Sym.Kind = dskMethod) then
            ALines.Add('    ' + Sym.Name);
      end;
      if ImpTypeCount > 0 then
      begin
        ALines.Add('  Imported types (referenced from other units):');
        for var Sym in AResult.Symbols do
          if (Sym.Origin = dsoImported) and (Sym.Kind = dskType) then
            ALines.Add('    ' + Sym.Name);
      end;
      if ImpMethodCount > 0 then
      begin
        ALines.Add('  Imported methods/values:');
        for var Sym in AResult.Symbols do
          if (Sym.Origin = dsoImported) and (Sym.Kind = dskMethod) then
            ALines.Add('    ' + Sym.Name);
      end;
    end;
  end;

  if dasSections in FSections then
  begin
    ALines.Add('');
    ALines.Add('[Sections]');
    ALines.Add('  (reserved for a later iteration)');
  end;

  if (AResult.Diagnostics.Count > 0) and FVerbose then
  begin
    ALines.Add('');
    ALines.Add('[Diagnostics]');
    for var Diag in AResult.Diagnostics do
      ALines.Add('  ' + Diag);
  end;
end;

procedure TDptDcuAnalyzeTask.RenderJson(const AResult: TDcuAnalysisResult;
  ALines: TStrings);
var
  First     : Boolean;
  IncludeSrc: TDcuSourceRef;
  Sb        : TStringBuilder;
  UsesEntry : TDcuUsesEntry;

  procedure Add(const S: string);
  begin
    Sb.AppendLine(S);
  end;

begin
  Sb := TStringBuilder.Create;
  try
    Add('{');
    Add('  "file": ' + JsonStr(AResult.FilePath) + ',');
    Add('  "size": ' + IntToStr(AResult.FileSize) + ',');
    Add('  "isDcu": ' + IfThen(AResult.IsDcu, 'true', 'false') + ',');

    if dasHeader in FSections then
    begin
      Add('  "header": {');
      Add('    "magic": ' + JsonStr(AResult.Header.MagicHex) + ',');
      Add('    "compiler": ' + JsonStr(DcuKnownCompilerName[AResult.Header.DetectedCompiler]) + ',');
      Add('    "platform": ' + JsonStr(DcuPlatformName[AResult.Header.DetectedPlatform]) + ',');
      Add('    "unit": ' + JsonStr(AResult.Header.UnitName) + ',');
      Add('    "source": ' + JsonStr(AResult.Header.PrimarySource.FileName) + ',');
      Sb.Append('    "includes": [');
      First := True;
      for IncludeSrc in AResult.Header.IncludeSources do
      begin
        if not First then Sb.Append(', ');
        Sb.Append(JsonStr(IncludeSrc.FileName));
        First := False;
      end;
      Add(']');
      Add('  },');
    end;

    if dasUses in FSections then
    begin
      Sb.Append('  "interfaceUses": [');
      First := True;
      for UsesEntry in AResult.InterfaceUses do
      begin
        if not First then Sb.Append(', ');
        if FResolve then
          Sb.Append(System.SysUtils.Format(
            '{"unit": %s, "resolvedPath": %s}',
            [JsonStr(UsesEntry.UnitName),
             IfThen(UsesEntry.ResolvedPath = '', 'null',
               JsonStr(UsesEntry.ResolvedPath))]))
        else
          Sb.Append(JsonStr(UsesEntry.UnitName));
        First := False;
      end;
      Add('],');

      Sb.Append('  "implementationUses": [');
      First := True;
      for UsesEntry in AResult.ImplementationUses do
      begin
        if not First then Sb.Append(', ');
        if FResolve then
          Sb.Append(System.SysUtils.Format(
            '{"unit": %s, "resolvedPath": %s}',
            [JsonStr(UsesEntry.UnitName),
             IfThen(UsesEntry.ResolvedPath = '', 'null',
               JsonStr(UsesEntry.ResolvedPath))]))
        else
          Sb.Append(JsonStr(UsesEntry.UnitName));
        First := False;
      end;
      Add('],');
    end;

    if dasSymbols in FSections then
    begin
      Sb.Append('  "symbols": {');
      // Imported references
      Sb.Append(' "imported": { "types": [');
      First := True;
      for var Sym in AResult.Symbols do
        if (Sym.Origin = dsoImported) and (Sym.Kind = dskType) then
        begin
          if not First then Sb.Append(', ');
          Sb.Append(JsonStr(Sym.Name));
          First := False;
        end;
      Sb.Append('], "methods": [');
      First := True;
      for var Sym in AResult.Symbols do
        if (Sym.Origin = dsoImported) and (Sym.Kind = dskMethod) then
        begin
          if not First then Sb.Append(', ');
          Sb.Append(JsonStr(Sym.Name));
          First := False;
        end;
      Sb.Append('] },');
      // Exported declarations
      Sb.Append(' "exported": { "types": [');
      First := True;
      for var Sym in AResult.Symbols do
        if (Sym.Origin = dsoExported) and (Sym.Kind = dskType) then
        begin
          if not First then Sb.Append(', ');
          Sb.Append(JsonStr(Sym.Name));
          First := False;
        end;
      Sb.Append('], "routines": [');
      First := True;
      for var Sym in AResult.Symbols do
        if (Sym.Origin = dsoExported) and (Sym.Kind = dskMethod) then
        begin
          if not First then Sb.Append(', ');
          Sb.Append(JsonStr(Sym.Name));
          First := False;
        end;
      Sb.Append('] }');
      Add(' },');
    end;

    if dasSections in FSections then
      Add('  "sections": null,');

    Sb.Append('  "diagnostics": [');
    First := True;
    for var Diag in AResult.Diagnostics do
    begin
      if not First then Sb.Append(', ');
      Sb.Append(JsonStr(Diag));
      First := False;
    end;
    Add(']');

    Add('}');

    ALines.Text := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

procedure TDptDcuAnalyzeTask.Execute;
var
  Lines : TStringList;
  Result: TDcuAnalysisResult;
begin
  if not FileExists(FDcuFile) then
    raise EDptRuntimeError.Create('DCU file not found: ' + FDcuFile);

  Result := TDcuAnalyzer.Analyze(FDcuFile);

  // Optional resolver pass: combine the user's --SearchPath list with the
  // Delphi RTL search path inferred from the magic-byte detected
  // (compiler, platform) tuple. The DCU's own directory is added by the
  // resolver itself, so peer DCUs always resolve cheaply.
  if FResolve then
  begin
    var EffectivePaths := FSearchPaths;
    var AutoPaths := AutoSearchPathsFor(Result);
    for var P in AutoPaths do
    begin
      SetLength(EffectivePaths, Length(EffectivePaths) + 1);
      EffectivePaths[High(EffectivePaths)] := P;
    end;
    TDcuAnalyzer.ResolveUses(Result, EffectivePaths);
  end;

  Lines := TStringList.Create;
  try
    case FFormat of
      dafText: RenderText(Result, Lines);
      dafJson: RenderJson(Result, Lines);
    end;
    for var L in Lines do
      Writeln(L);
  finally
    Lines.Free;
  end;
end;

end.
