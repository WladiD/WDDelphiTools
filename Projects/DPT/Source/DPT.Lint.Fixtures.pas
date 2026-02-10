// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Fixtures;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Generics.Collections,
  System.IOUtils,
  Slim.Fixture,
  DPT.Lint.Context;

type

  [SlimFixture('DptLintUnitContextFixture')]
  TDptLintUnitContextFixture = class(TSlimFixture)
  private
    FTargetFile: string;
    FLines: TStringList;
    FSingleSeparatorLine: string;
    FDoubleSeparatorLine: string;
  public
    constructor Create(const ATargetFile: string);
    destructor Destroy; override;

    procedure DefineSingleSeparatorLine(const AValue: string);
    procedure DefineDoubleSeparatorLine(const AValue: string);
    function GetFixture: TSlimFixture;

    // File property guards (ensure properties, fix if needed)
    procedure EnsureUtf8Bom;
    procedure EnsureCrlf;

    property TargetFile: string read FTargetFile;
    property Lines: TStringList read FLines;
    property SingleSeparatorLine: string read FSingleSeparatorLine;
    property DoubleSeparatorLine: string read FDoubleSeparatorLine;
  end;

  TDptLintBaseFixture = class(TSlimFixture)
  protected
    FContext: TDptLintUnitContextFixture;
    procedure ReportViolation(ALine: Integer; const AMsg: string);
  public
    procedure SetContext(AContext: TSlimFixture);
  end;

  [SlimFixture('DptLintFixture')]
  TDptLintFixture = class(TDptLintBaseFixture)
  private
    function GetPart(const AStartKey, AEndKey1, AEndKey2, AEndKey3: string; AIncludeStart: Boolean): string;
    function GetPartOffset(const AStartKey: string; AIncludeStart: Boolean): Integer;
  public
    class procedure ReportError(const AFile: string; ALine: Integer; const AMsg: string);

    // General Purpose Primitives
    function LineStartsWith(ALine: string; const AText: string): Boolean;
    function FileMatchesRegex(const APattern: string): Boolean;
    function FileContainsInclude(const AIncludeNames: string): Boolean;
    function UnitNameMatchesFileName: Boolean;
    function KeywordIsSurroundedBy(const AKeyword, ASeparator: string): Boolean;

    // Unit Part Extractors for further analysis
    function GetBeforeUnitKeywordPart: string;
    function GetInterfacePart: string;
    function GetImplementationPart: string;
    function GetInitializationPart: string;
    function GetFinalizationPart: string;

    function GetBeforeUnitKeywordPartOffset: string;
    function GetInterfacePartOffset: string;
    function GetImplementationPartOffset: string;
    function GetInitializationPartOffset: string;
    function GetFinalizationPartOffset: string;

    function ColonsAreVerticallyAlignedInRange(const AStartLine, AEndLine: string): Boolean;
    function ColonsInRecordsAreAligned: Boolean;
    function LastLineBeforeEndIsDoubleSeparator: Boolean;
  end;

  [SlimFixture('DptLintRegexFixture')]
  TDptLintRegexFixture = class(TSlimFixture)
  private
    FContent: string;
    FLineOffset: Integer;
  public
    constructor Create(const AContent: string);
    procedure SetLineOffset(const AValue: string);
    function Match(const APattern: string): Boolean;
    function Fetch(const APattern: string; const AGroupName: string): string;
  end;

  [SlimFixture('DptLintUsesFixture')]
  TDptLintUsesFixture = class(TDptLintBaseFixture)
  type
    TUsesGroup = record
      Name: string;
      Patterns: TArray<string>;
    end;
    TUnitInfo = record
      Name: string;
      Line: Integer; // Local line within FContent
      GroupIdx: Integer;
    end;
  private
    FContent: string;
    FLineOffset: Integer;
    FGroups: TList<TUsesGroup>;
    FUnits: TList<TUnitInfo>;
    procedure ParseUnits;
    function MatchNamespace(const AUnitName, APattern: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetContent(const AValue: string);
    procedure SetLineOffset(const AValue: string);
    procedure AddGroupWithNamespaces(const AGroupName: string; const ANamespaces: string);

    function AllUnitsOnSeparateLines: Boolean;
    function GroupsAreSortedAlphabetically: Boolean;
    function GroupsAreSeparatedByBlankLines: Boolean;
    function GroupsFollowOrder: Boolean;
    function BlankLineAfterUsesExists: Boolean;

    function GetLinesAfterLastUsesLine(ALinesCount: Integer): string;
    function GetLinesAfterLastUsesLineOffset(ALinesCount: Integer): string;
  end;

  [SlimFixture('DptLintClassDeclarationFixture')]
  TDptLintClassDeclarationFixture = class(TDptLintBaseFixture)
  private
    type
      TMemberType = (mtField, mtMethod, mtProperty, mtClassMethod, mtConstructor, mtUnknown);
      TMemberInfo = record
        MemberType: TMemberType;
        Name: string;
        FullLine: string;
        LineIdx: Integer;
        NameStartPos: Integer;
      end;
      TMemberBlock = class
        Members: TList<TMemberInfo>;
        Visibility: string;
        constructor Create;
        destructor Destroy; override;
      end;
  private
    FContent: string;
    FLineOffset: Integer;
    FClassNamePrefixes: string;
    FVisibilityExtraIndent: Integer;
    FFieldNamePrefix: string;
    FFieldsMustBeSorted: Boolean;
    FMethodsMustBeSorted: Boolean;
    FPropertiesMustBeSorted: Boolean;
    FAlignMemberNames: Boolean;
    function ParseMember(const ALine: string; ALineIdx: Integer): TMemberInfo;
    procedure ValidateBlock(ABlock: TMemberBlock; var AResult: Boolean);
  public
    constructor Create;
    procedure SetContent(const AValue: string);
    procedure SetLineOffset(const AValue: string);

    property ClassNamePrefixes: string read FClassNamePrefixes write FClassNamePrefixes;
    property VisibilityExtraIndent: Integer read FVisibilityExtraIndent write FVisibilityExtraIndent;
    property FieldNamePrefix: string read FFieldNamePrefix write FFieldNamePrefix;
    property FieldsMustBeSorted: Boolean read FFieldsMustBeSorted write FFieldsMustBeSorted;
    property MethodsMustBeSorted: Boolean read FMethodsMustBeSorted write FMethodsMustBeSorted;
    property PropertiesMustBeSorted: Boolean read FPropertiesMustBeSorted write FPropertiesMustBeSorted;
    property AlignMemberNames: Boolean read FAlignMemberNames write FAlignMemberNames;

    function LintClassDeclarations: Boolean;
  end;

  [SlimFixture('DptLintImplementationFixture')]
  TDptLintImplementationFixture = class(TDptLintBaseFixture)
  private
    FContent: string;
    FLineOffset: Integer;
  public
    constructor Create;
    procedure SetContent(const AValue: string);
    procedure SetLineOffset(const AValue: string);

    function ValidateClassBanners: Boolean;
    function ValidateMethodOrderAndSeparators: Boolean;
    function DestructorsMustCallInherited: Boolean;
  end;

implementation

uses
  System.RegularExpressions,
  WDDT.StringTools;

{ TDptLintUnitContextFixture }

constructor TDptLintUnitContextFixture.Create(const ATargetFile: string);
begin
  inherited Create;
  FTargetFile := ATargetFile;
  FLines := TStringList.Create;

  if FileExists(FTargetFile) then
    FLines.LoadFromFile(FTargetFile, TEncoding.UTF8);

  FSingleSeparatorLine := '-------';
  FDoubleSeparatorLine := '=======';
end;

destructor TDptLintUnitContextFixture.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TDptLintUnitContextFixture.DefineSingleSeparatorLine(const AValue: string);
begin
  var LVal := AValue.Trim(['{', '}', ' ', '!', '-']);
  if LVal <> '' then
    FSingleSeparatorLine := LVal;
end;

procedure TDptLintUnitContextFixture.DefineDoubleSeparatorLine(const AValue: string);
begin
  var LVal := AValue.Trim(['{', '}', ' ', '!', '=', '-']);
  if LVal <> '' then
    FDoubleSeparatorLine := LVal;
end;

function TDptLintUnitContextFixture.GetFixture: TSlimFixture;
begin
  Result := Self;
end;

procedure TDptLintUnitContextFixture.EnsureUtf8Bom;
var
  LStream: TFileStream;
  LPreamble: TBytes;
  LHasBom: Boolean;
begin
  LHasBom := False;
  LStream := TFileStream.Create(FTargetFile, fmOpenRead or fmShareDenyNone);
  try
    LPreamble := TEncoding.UTF8.GetPreamble;
    if LStream.Size >= Length(LPreamble) then
    begin
      SetLength(LPreamble, Length(LPreamble));
      LStream.ReadBuffer(LPreamble[0], Length(LPreamble));
      LHasBom := (LPreamble[0] = $EF) and (LPreamble[1] = $BB) and (LPreamble[2] = $BF);
    end;
  finally
    LStream.Free;
  end;

  if not LHasBom then
  begin
    var LContent := TFile.ReadAllText(FTargetFile);
    TFile.WriteAllText(FTargetFile, LContent, TEncoding.UTF8);
    // Reload lines to reflect changes (though encoding doesn't change text content)
    FLines.LoadFromFile(FTargetFile, TEncoding.UTF8);
    TDptLintContext.AddWarning(Format('Warning: File "%s" was converted to UTF-8 with BOM.', [ExtractFileName(FTargetFile)]));
  end;
end;

procedure TDptLintUnitContextFixture.EnsureCrlf;
var
  LText: string;
  LFoundLFOnly: Boolean;
begin
  LText := TFile.ReadAllText(FTargetFile);

  LFoundLFOnly := False;
  for var I := 1 to LText.Length do
  begin
    if LText[I] = #10 then
    begin
      if (I = 1) or (LText[I-1] <> #13) then
      begin
        LFoundLFOnly := True;
        Break;
      end;
    end;
  end;

  if LFoundLFOnly then
  begin
    // Normalize to CRLF
    var LFixed := LText.Replace(#13#10, #10).Replace(#13, #10).Replace(#10, #13#10);
    TFile.WriteAllText(FTargetFile, LFixed, TEncoding.UTF8);
    // Reload lines
    FLines.LoadFromFile(FTargetFile, TEncoding.UTF8);
    TDptLintContext.AddWarning(Format('Warning: Line endings in file "%s" were converted to CRLF.', [ExtractFileName(FTargetFile)]));
  end;
end;

{ TDptLintBaseFixture }

procedure TDptLintBaseFixture.SetContext(AContext: TSlimFixture);
begin
  if AContext is TDptLintUnitContextFixture then
    FContext := TDptLintUnitContextFixture(AContext);
end;

procedure TDptLintBaseFixture.ReportViolation(ALine: Integer; const AMsg: string);
begin
  if Assigned(FContext) then
    TDptLintFixture.ReportError(FContext.TargetFile, ALine, AMsg)
  else
    TDptLintFixture.ReportError('', ALine, AMsg);
end;

{ TDptLintFixture }

class procedure TDptLintFixture.ReportError(const AFile: string; ALine: Integer; const AMsg: string);
begin
  TDptLintContext.Add(ALine, AFile, AMsg);
end;

function TDptLintFixture.LineStartsWith(ALine: string; const AText: string): Boolean;
var
  LLine: Integer;
begin
  LLine := StrToIntDef(ALine, 0);
  Result := Assigned(FContext) and
            (LLine > 0) and
            (LLine <= FContext.Lines.Count) and
            FContext.Lines[LLine - 1].Trim.StartsWith(AText);

  if not Result then
    ReportViolation(LLine, Format('Line should start with: "%s"', [AText]));
end;

function TDptLintFixture.FileMatchesRegex(const APattern: string): Boolean;
begin
  Result := Assigned(FContext) and TRegEx.IsMatch(FContext.Lines.Text, APattern);

  if not Result then
    ReportViolation(1, Format('File content does not match mandatory pattern: "%s"', [APattern]));
end;

function TDptLintFixture.FileContainsInclude(const AIncludeNames: string): Boolean;
var
  LFound: Boolean;
  LNames: TArray<string>;
begin
  LFound := False;
  if not Assigned(FContext) then
    Exit(False);

  LNames := AIncludeNames.Split([';']);
  for var Line in FContext.Lines do
  begin
    for var LName in LNames do
    begin
      if Line.Contains('{$I ' + LName.Trim + '}') then
      begin
        LFound := True;
        Break;
      end;
    end;
    if LFound then
      Break;
  end;

  Result := LFound;
  if not Result then
    ReportViolation(1, Format('None of the required includes found: %s', [AIncludeNames]));
end;

function TDptLintFixture.UnitNameMatchesFileName: Boolean;
var
  LFileName: string;
  I: Integer;
begin
  if not Assigned(FContext) then
    Exit(False);

  LFileName := TPath.GetFileNameWithoutExtension(FContext.TargetFile);
  for I := 0 to FContext.Lines.Count - 1 do
  begin
    if FContext.Lines[I].Trim.StartsWith('unit ', True) then
    begin
      var LUnitName := FContext.Lines[I].Trim.Substring(5).Trim([' ', ';']);
      Result := SameText(LUnitName, LFileName);
      if not Result then
        ReportViolation(I + 1, Format('Unit name "%s" does not match file name "%s".', [LUnitName, LFileName]));
      Exit(Result);
    end;
  end;

  ReportViolation(1, 'No "unit" declaration found.');
  Result := False;
end;

function TDptLintFixture.KeywordIsSurroundedBy(const AKeyword, ASeparator: string): Boolean;
var
  LSeparator: string;
  I: Integer;
begin
  Result := False;
  if not Assigned(FContext) then
    Exit(False);

  LSeparator := ASeparator;
  if LSeparator = '' then
    LSeparator := FContext.DoubleSeparatorLine;

  for I := 0 to FContext.Lines.Count - 1 do
  begin
    if SameText(FContext.Lines[I].Trim, AKeyword) then
    begin
      var LBefore := (I > 0) and FContext.Lines[I - 1].Contains(LSeparator);
      var LAfter := (I < FContext.Lines.Count - 1) and FContext.Lines[I + 1].Contains(LSeparator);
      Result := LBefore and LAfter;

      if not Result then
        ReportViolation(I + 1, Format('Keyword "%s" must be surrounded by separators: "%s"', [AKeyword, LSeparator]));
      Exit(Result);
    end;
  end;

  ReportViolation(1, Format('Keyword "%s" not found.', [AKeyword]));
end;

function TDptLintFixture.GetPart(const AStartKey, AEndKey1, AEndKey2, AEndKey3: string; AIncludeStart: Boolean): string;
var
  LStartIdx, LEndIdx: Integer;
  LBuilder: TStringBuilder;
  I: Integer;
begin
  Result := '';
  if not Assigned(FContext) then
    Exit;

  LStartIdx := -1;
  LEndIdx := FContext.Lines.Count;

  for I := 0 to FContext.Lines.Count - 1 do
  begin
    var LLine := FContext.Lines[I].Trim.ToLower;
    if (LStartIdx = -1) and (AStartKey = '') then
      LStartIdx := 0;

    if (LStartIdx = -1) and (LLine.StartsWith(AStartKey.ToLower)) then
    begin
      if AIncludeStart then
        LStartIdx := I
      else
        LStartIdx := I + 1;
      Continue;
    end;

    if (LStartIdx <> -1) then
    begin
      if (AEndKey1 <> '') and (LLine.StartsWith(AEndKey1.ToLower)) then
      begin
        LEndIdx := I;
        Break;
      end;
      if (AEndKey2 <> '') and (LLine.StartsWith(AEndKey2.ToLower)) then
      begin
        LEndIdx := I;
        Break;
      end;
      if (AEndKey3 <> '') and (LLine.StartsWith(AEndKey3.ToLower)) then
      begin
        LEndIdx := I;
        Break;
      end;
    end;
  end;

  if LStartIdx = -1 then
    Exit;

  LBuilder := TStringBuilder.Create;
  try
    for I := LStartIdx to LEndIdx - 1 do
      LBuilder.AppendLine(FContext.Lines[I]);
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TDptLintFixture.GetBeforeUnitKeywordPart: string;
begin
  Result := GetPart('', 'unit', '', '', False);
end;

function TDptLintFixture.GetInterfacePart: string;
begin
  Result := GetPart('interface', 'implementation', '', '', False);
end;

function TDptLintFixture.GetImplementationPart: string;
begin
  Result := GetPart('implementation', 'initialization', 'finalization', 'end.', False);
end;

function TDptLintFixture.GetInitializationPart: string;
begin
  Result := GetPart('initialization', 'finalization', 'end.', '', False);
end;

function TDptLintFixture.GetFinalizationPart: string;
begin
  Result := GetPart('finalization', 'end.', '', '', False);
end;

function TDptLintFixture.GetPartOffset(const AStartKey: string; AIncludeStart: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  if not Assigned(FContext) or (AStartKey = '') then
    Exit;

  for I := 0 to FContext.Lines.Count - 1 do
  begin
    if FContext.Lines[I].Trim.ToLower.StartsWith(AStartKey.ToLower) then
    begin
      if AIncludeStart then
        Exit(I)
      else
        Exit(I + 1);
    end;
  end;
end;

function TDptLintFixture.GetBeforeUnitKeywordPartOffset: string;
begin
  Result := '0';
end;

function TDptLintFixture.GetInterfacePartOffset: string;
begin
  Result := IntToStr(GetPartOffset('interface', False));
end;

function TDptLintFixture.GetImplementationPartOffset: string;
begin
  Result := IntToStr(GetPartOffset('implementation', False));
end;

function TDptLintFixture.GetInitializationPartOffset: string;
begin
  Result := IntToStr(GetPartOffset('initialization', False));
end;

function TDptLintFixture.GetFinalizationPartOffset: string;
begin
  Result := IntToStr(GetPartOffset('finalization', False));
end;

function TDptLintFixture.ColonsAreVerticallyAlignedInRange(const AStartLine, AEndLine: string): Boolean;
var
  LStart, LEnd, LFirstPos, LPos: Integer;
  I: Integer;
begin
  if not Assigned(FContext) then
    Exit(False);

  LStart := StrToIntDef(AStartLine, 0);
  LEnd := StrToIntDef(AEndLine, 0);
  LFirstPos := -1;
  Result := True;

  if (LStart <= 0) or (LEnd < LStart) or (LEnd > FContext.Lines.Count) then
  begin
    ReportViolation(LStart, 'Invalid line range for colon alignment check.');
    Exit(False);
  end;

  for I := LStart - 1 to LEnd - 1 do
  begin
    LPos := FContext.Lines[I].IndexOf(':');
    if LPos >= 0 then
    begin
      if LFirstPos = -1 then
        LFirstPos := LPos
      else if LPos <> LFirstPos then
      begin
        ReportViolation(I + 1, Format('Colon alignment mismatch. Expected position %d but found %d.', [LFirstPos, LPos]));
        Result := False;
      end;
    end;
  end;
end;

function TDptLintFixture.ColonsInRecordsAreAligned: Boolean;
var
  I: Integer;
  LInRecord: Boolean;
  LRecordStart: Integer;
begin
  Result := True;
  if not Assigned(FContext) then
    Exit(False);

  LInRecord := False;
  LRecordStart := -1;

  for I := 0 to FContext.Lines.Count - 1 do
  begin
    var LLine := FContext.Lines[I].Trim.ToLower;
    if not LInRecord then
    begin
      if TRegEx.IsMatch(LLine, '=\s*(?:packed\s+)?record', [roIgnoreCase]) then
      begin
        LInRecord := True;
        LRecordStart := I + 1;
      end;
    end
    else if LLine.StartsWith('end;') or LLine.StartsWith('end ') or (LLine = 'end') then
    begin
      if not ColonsAreVerticallyAlignedInRange(IntToStr(LRecordStart), IntToStr(I + 1)) then
        Result := False;
      LInRecord := False;
    end;
  end;
end;

function TDptLintFixture.LastLineBeforeEndIsDoubleSeparator: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if not Assigned(FContext) then
    Exit(False);

  for I := FContext.Lines.Count - 1 downto 0 do
  begin
    if FContext.Lines[I].Trim.ToLower = 'end.' then
    begin
      var LFound := False;
      J := I - 1;
      while (J >= 0) and (FContext.Lines[J].Trim = '') do
        Dec(J);

      if (J >= 0) and FContext.Lines[J].Contains(FContext.DoubleSeparatorLine) then
        LFound := True;

      if not LFound then
      begin
        ReportViolation(I + 1, 'Double separator expected before "end."');
        Exit(False);
      end;
      Exit(True);
    end;
  end;
end;

{ TDptLintRegexFixture }

constructor TDptLintRegexFixture.Create(const AContent: string);
begin
  inherited Create;
  FContent := AContent;
  FLineOffset := 0;
end;

procedure TDptLintRegexFixture.SetLineOffset(const AValue: string);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

function TDptLintRegexFixture.Match(const APattern: string): Boolean;
begin
  if FContent = '' then
    Exit(True);
  Result := TRegEx.IsMatch(FContent, APattern, [roSingleLine, roIgnoreCase]);
end;

function TDptLintRegexFixture.Fetch(const APattern: string; const AGroupName: string): string;
var
  LMatch: TMatch;
begin
  Result := '';
  if FContent = '' then
    Exit;

  LMatch := TRegEx.Match(FContent, APattern, [roSingleLine, roIgnoreCase]);
  if LMatch.Success then
    Result := LMatch.Groups[AGroupName].Value;
end;

{ TDptLintUsesFixture }

constructor TDptLintUsesFixture.Create;
begin
  inherited Create;
  FGroups := TList<TUsesGroup>.Create;
  FUnits := TList<TUnitInfo>.Create;
  FLineOffset := 0;
end;

destructor TDptLintUsesFixture.Destroy;
begin
  FUnits.Free;
  FGroups.Free;
  inherited;
end;

procedure TDptLintUsesFixture.SetContent(const AValue: string);
begin
  FContent := AValue;
end;

procedure TDptLintUsesFixture.SetLineOffset(const AValue: string);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

procedure TDptLintUsesFixture.AddGroupWithNamespaces(const AGroupName, ANamespaces: string);
var
  G: TUsesGroup;
  LPatterns: TList<string>;
  LParts: TArray<string>;
begin
  G.Name := AGroupName;
  LParts := ANamespaces.Replace(',', ' ').Replace(';', ' ').Split([' ']);
  LPatterns := TList<string>.Create;
  try
    for var S in LParts do
      if S.Trim <> '' then
        LPatterns.Add(S.Trim);
    G.Patterns := LPatterns.ToArray;
  finally
    LPatterns.Free;
  end;
  FGroups.Add(G);
end;

function TDptLintUsesFixture.MatchNamespace(const AUnitName, APattern: string): Boolean;
begin
  if APattern = '*' then
    Exit(True);

  if APattern.StartsWith('*') then
  begin
    if APattern.EndsWith('*') then
    begin
      // *Contains*
      // Remove first and last *
      var LSub := APattern.Substring(1, APattern.Length - 2);
      Exit(AUnitName.ToLower.IndexOf(LSub.ToLower) >= 0);
    end
    else
    begin
      // *EndsWith
      var LSub := APattern.Substring(1);
      Exit(AUnitName.EndsWith(LSub, True));
    end;
  end;

  if APattern.EndsWith('.*') then
    Exit(AUnitName.StartsWith(APattern.Substring(0, APattern.Length - 1), True));
  Result := SameText(AUnitName, APattern);
end;

procedure TDptLintUsesFixture.ParseUnits;
var
  LLines: TArray<string>;
  LInUses: Boolean;
  I: Integer;
begin
  FUnits.Clear;
  LLines := FContent.Split([sLineBreak]);
  LInUses := False;

  for I := 0 to High(LLines) do
  begin
    var LLine := LLines[I].Trim;
    if not LInUses then
    begin
      if SameText(LLine, 'uses') then
        LInUses := True;
      Continue;
    end;

    if LLine = '' then
      Continue;

    // Ignore compiler directives and full-line comments
    if LLine.StartsWith('{$') or LLine.StartsWith('//') or LLine.StartsWith('{') then
      Continue;

    var LStrippedLine := LLine;
    var LIsEnd := LStrippedLine.Contains(';');
    if LIsEnd then
      LStrippedLine := LStrippedLine.Substring(0, LStrippedLine.IndexOf(';'));

    var LWords := LStrippedLine.Replace(',', ' ').Split([' ']);
    for var Word in LWords do
    begin
      var LName := Word.Trim;
      if LName = '' then
        Continue;

      var U: TUnitInfo;
      U.Name := LName;
      U.Line := I + 1;
      U.GroupIdx := -1;

      var LBestMatchLen := -1;
      var LWildcardMatchGroupIdx := -1;

      for var GIdx := 0 to FGroups.Count - 1 do
      begin
        for var Pat in FGroups[GIdx].Patterns do
        begin
          if MatchNamespace(LName, Pat) then
          begin
            if Pat = '*' then
            begin
              if LWildcardMatchGroupIdx = -1 then
                LWildcardMatchGroupIdx := GIdx;
            end
            else
            begin
              if Pat.Length > LBestMatchLen then
              begin
                LBestMatchLen := Pat.Length;
                U.GroupIdx := GIdx;
              end;
            end;
          end;
        end;
      end;

      if (U.GroupIdx = -1) and (LWildcardMatchGroupIdx <> -1) then
        U.GroupIdx := LWildcardMatchGroupIdx;

      FUnits.Add(U);
    end;

    if LIsEnd then
      Break;
  end;
end;

function TDptLintUsesFixture.AllUnitsOnSeparateLines: Boolean;
begin
  ParseUnits;
  Result := True;
  var LLinesWithUnits := TDictionary<Integer, Integer>.Create;
  try
    for var U in FUnits do
    begin
      if LLinesWithUnits.ContainsKey(U.Line) then
      begin
        ReportViolation(U.Line + FLineOffset, 'Only one unit allowed per line in uses clause.');
        Result := False;
      end
      else
        LLinesWithUnits.Add(U.Line, 1);
    end;
  finally
    LLinesWithUnits.Free;
  end;
end;

function TDptLintUsesFixture.GroupsAreSortedAlphabetically: Boolean;
begin
  ParseUnits;
  Result := True;
  for var I := 1 to FUnits.Count - 1 do
  begin
    var Prev := FUnits[I - 1];
    var Curr := FUnits[I];
    if (Curr.GroupIdx = Prev.GroupIdx) and (CompareStringNatural(Curr.Name, Prev.Name) < 0) then
    begin
      ReportViolation(Curr.Line + FLineOffset, Format('Units in group "%s" must be sorted alphabetically: "%s" before "%s".', [FGroups[Curr.GroupIdx].Name, Curr.Name, Prev.Name]));
      Result := False;
    end;
  end;
end;

function TDptLintUsesFixture.GroupsAreSeparatedByBlankLines: Boolean;
var
  LLines: TArray<string>;
begin
  ParseUnits;
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  for var I := 1 to FUnits.Count - 1 do
  begin
    var Prev := FUnits[I - 1];
    var Curr := FUnits[I];
    if Curr.GroupIdx > Prev.GroupIdx then
    begin
      var LFoundBlank := False;
      for var LIdx := Prev.Line to Curr.Line - 2 do
      begin
        if LLines[LIdx].Trim = '' then
        begin
          LFoundBlank := True;
          Break;
        end;
      end;

      if not LFoundBlank then
      begin
        ReportViolation(Curr.Line + FLineOffset, Format('Missing blank line before group "%s".', [FGroups[Curr.GroupIdx].Name]));
        Result := False;
      end;
    end;
  end;
end;

function TDptLintUsesFixture.GroupsFollowOrder: Boolean;
begin
  ParseUnits;
  Result := True;
  for var I := 1 to FUnits.Count - 1 do
  begin
    var Prev := FUnits[I - 1];
    var Curr := FUnits[I];
    if (Curr.GroupIdx <> -1) and (Prev.GroupIdx <> -1) and (Curr.GroupIdx < Prev.GroupIdx) then
    begin
      ReportViolation(Curr.Line + FLineOffset, Format('Group "%s" must appear before group "%s".', [FGroups[Curr.GroupIdx].Name, FGroups[Prev.GroupIdx].Name]));
      Result := False;
    end;
  end;
end;

function TDptLintUsesFixture.BlankLineAfterUsesExists: Boolean;
var
  LLines: TArray<string>;
  I: Integer;
begin
  Result := False;
  LLines := FContent.Split([sLineBreak]);
  for I := 0 to High(LLines) do
  begin
    if SameText(LLines[I].Trim, 'uses') then
    begin
      if (I < High(LLines)) and (LLines[I + 1].Trim = '') then
        Exit(True);
      ReportViolation(I + 1 + FLineOffset, 'Missing blank line after "uses" keyword.');
      Exit(False);
    end;
  end;
end;

function TDptLintUsesFixture.GetLinesAfterLastUsesLine(ALinesCount: Integer): string;
var
  LLines: TArray<string>;
  LInUses: Boolean;
  LLastLineIdx, LCount, I: Integer;
  LBuilder: TStringBuilder;
begin
  Result := '';
  LLines := FContent.Split([sLineBreak]);
  LInUses := False;
  LLastLineIdx := -1;

  for I := 0 to High(LLines) do
  begin
    var LLine := LLines[I].Trim;
    if not LInUses then
    begin
      if SameText(LLine, 'uses') then
        LInUses := True;
      Continue;
    end;
    if LLine.Contains(';') then
    begin
      LLastLineIdx := I;
      Break;
    end;
  end;

  if LLastLineIdx = -1 then
    Exit;

  LBuilder := TStringBuilder.Create;
  try
    LCount := 0;
    for I := LLastLineIdx + 1 to High(LLines) do
    begin
      if LCount >= ALinesCount then
        Break;
      LBuilder.AppendLine(LLines[I]);
      Inc(LCount);
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TDptLintUsesFixture.GetLinesAfterLastUsesLineOffset(ALinesCount: Integer): string;
var
  LLines: TArray<string>;
  LInUses: Boolean;
  LLastLineIdx, I: Integer;
begin
  Result := '0';
  LLines := FContent.Split([sLineBreak]);
  LInUses := False;
  LLastLineIdx := -1;

  for I := 0 to High(LLines) do
  begin
    var LLine := LLines[I].Trim;
    if not LInUses then
    begin
      if SameText(LLine, 'uses') then
        LInUses := True;
      Continue;
    end;
    if LLine.Contains(';') then
    begin
      LLastLineIdx := I;
      Break;
    end;
  end;

  if LLastLineIdx <> -1 then
    Result := IntToStr(LLastLineIdx + 1 + FLineOffset);
end;

{ TDptLintClassDeclarationFixture.TMemberBlock }

constructor TDptLintClassDeclarationFixture.TMemberBlock.Create;
begin
  Members := TList<TMemberInfo>.Create;
end;

destructor TDptLintClassDeclarationFixture.TMemberBlock.Destroy;
begin
  Members.Free;
  inherited;
end;

{ TDptLintClassDeclarationFixture }

constructor TDptLintClassDeclarationFixture.Create;
begin
  inherited Create;
  FLineOffset := 0;
  FClassNamePrefixes := 'C,T';
  FVisibilityExtraIndent := 1;
  FFieldNamePrefix := 'F';
  FFieldsMustBeSorted := True;
  FMethodsMustBeSorted := True;
  FPropertiesMustBeSorted := True;
  FAlignMemberNames := True;
end;

procedure TDptLintClassDeclarationFixture.SetContent(const AValue: string);
begin
  FContent := AValue;
end;

procedure TDptLintClassDeclarationFixture.SetLineOffset(const AValue: string);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

function TDptLintClassDeclarationFixture.ParseMember(const ALine: string; ALineIdx: Integer): TMemberInfo;
var
  LMatch: TMatch;
begin
  Result.MemberType := mtUnknown;
  Result.LineIdx := ALineIdx;
  Result.FullLine := ALine;

  LMatch := TRegEx.Match(ALine, '^(\s*)class\s+(procedure|function|constructor|destructor|var)\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtClassMethod;
    Result.Name := LMatch.Groups[3].Value;
    Result.NameStartPos := LMatch.Groups[3].Index;
    Exit;
  end;

  LMatch := TRegEx.Match(ALine, '^(\s*)property\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtProperty;
    Result.Name := LMatch.Groups[2].Value;
    Result.NameStartPos := LMatch.Groups[2].Index;
    Exit;
  end;

  LMatch := TRegEx.Match(ALine, '^(\s*)(constructor|destructor)\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtConstructor;
    Result.Name := LMatch.Groups[3].Value;
    Result.NameStartPos := LMatch.Groups[3].Index;
    Exit;
  end;

  LMatch := TRegEx.Match(ALine, '^(\s*)(procedure|function)\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtMethod;
    Result.Name := LMatch.Groups[3].Value;
    Result.NameStartPos := LMatch.Groups[3].Index;
    Exit;
  end;

  LMatch := TRegEx.Match(ALine, '^(\s*)(\w+)\s*:', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtField;
    Result.Name := LMatch.Groups[2].Value;
    Result.NameStartPos := LMatch.Groups[2].Index;
    Exit;
  end;
end;

procedure TDptLintClassDeclarationFixture.ValidateBlock(ABlock: TMemberBlock; var AResult: Boolean);
var
  LFirst: TMemberInfo;
  LMustBeSorted: Boolean;
  I: Integer;
begin
  if ABlock.Members.Count < 1 then
    Exit;

  LFirst := ABlock.Members[0];
  LMustBeSorted := False;
  case LFirst.MemberType of
    mtField: LMustBeSorted := FFieldsMustBeSorted;
    mtMethod: LMustBeSorted := FMethodsMustBeSorted;
    mtProperty: LMustBeSorted := FPropertiesMustBeSorted;
  end;

  for I := 0 to ABlock.Members.Count - 1 do
  begin
    var Curr := ABlock.Members[I];
    if (Curr.MemberType = mtField) and (FFieldNamePrefix <> '') then
    begin
      // Rule: Published fields are allowed without 'F' prefix
      if not SameText(ABlock.Visibility, 'published') then
      begin
        if not Curr.Name.StartsWith(FFieldNamePrefix) then
        begin
          ReportViolation(Curr.LineIdx + 1 + FLineOffset, Format('Field name "%s" should start with prefix "%s".', [Curr.Name, FFieldNamePrefix]));
          AResult := False;
        end;
      end;
    end;

    if I > 0 then
    begin
      var Prev := ABlock.Members[I - 1];
      if LMustBeSorted and (CompareStringNatural(Curr.Name, Prev.Name) < 0) then
      begin
        ReportViolation(Curr.LineIdx + 1 + FLineOffset, Format('Member "%s" should be sorted alphabetically (before "%s").', [Curr.Name, Prev.Name]));
        AResult := False;
      end;
      if FAlignMemberNames and (Curr.NameStartPos <> Prev.NameStartPos) then
      begin
        ReportViolation(Curr.LineIdx + 1 + FLineOffset, Format('Member "%s" should be vertically aligned with "%s" (Pos %d vs %d).', [Curr.Name, Prev.Name, Curr.NameStartPos, Prev.NameStartPos]));
        AResult := False;
      end;
    end;
  end;
end;

function TDptLintClassDeclarationFixture.LintClassDeclarations: Boolean;
var
  LLines: TArray<string>;
  LClassIndents: TList<Integer>;
  LClassIndent: Integer;
  LClassName: string;
  LCurrentBlock: TMemberBlock;
  LPrevMemberType: TMemberType;
  LVisibilityKeywordFoundSinceLastMember: Boolean;
  LCurrentVisibility: string;
  I: Integer;

  procedure FlushBlock;
  begin
    if Assigned(LCurrentBlock) then
    begin
      ValidateBlock(LCurrentBlock, Result);
      FreeAndNil(LCurrentBlock);
    end;
  end;

begin
  Result := True;
  if not Assigned(FContext) then
    Exit(False);

  LLines := FContent.Split([sLineBreak]);
  LClassIndents := TList<Integer>.Create;
  LClassIndent := 0;
  LCurrentBlock := nil;
  LPrevMemberType := mtUnknown;
  LVisibilityKeywordFoundSinceLastMember := False;
  LCurrentVisibility := '';

  try
    for I := 0 to High(LLines) do
    begin
      var LLine := LLines[I];
      var LTrimmed := LLine.Trim;
      if LTrimmed = '' then
      begin
        FlushBlock;
        Continue;
      end;

      var LClassMatch := TRegEx.Match(LLine, '^(\s*)(\w+)\s*=\s*(?:packed\s+)?class', [roIgnoreCase]);
      if LClassMatch.Success then
      begin
        FlushBlock;
        LClassIndent := LClassMatch.Groups[1].Length;
        LClassIndents.Add(LClassIndent);
        LClassName := LClassMatch.Groups[2].Value;
        LPrevMemberType := mtUnknown;
        LVisibilityKeywordFoundSinceLastMember := True;
        LCurrentVisibility := 'published';

        var LPrefixes := FClassNamePrefixes.Split([',']);
        var LPrefixOk := False;
        for var P in LPrefixes do
        begin
          if LClassName.StartsWith(P.Trim) then
          begin
            LPrefixOk := True;
            Break;
          end;
        end;

        if not LPrefixOk then
        begin
          ReportViolation(I + 1 + FLineOffset, Format('Class name "%s" should start with one of: %s', [LClassName, FClassNamePrefixes]));
          Result := False;
        end;
        Continue;
      end;

      if LClassIndents.Count > 0 then
      begin
        if TRegEx.IsMatch(LLine, '^\s*end\s*;', [roIgnoreCase]) then
        begin
          FlushBlock;
          LClassIndents.Delete(LClassIndents.Count - 1);
          if LClassIndents.Count > 0 then
            LClassIndent := LClassIndents[LClassIndents.Count - 1]
          else
            LClassIndent := 0;
          Continue;
        end;

        var LVisMatch := TRegEx.Match(LLine, '^(\s*)(strict\s+)?(private|protected|public|published)', [roIgnoreCase]);
        if LVisMatch.Success then
        begin
          FlushBlock;
          LVisibilityKeywordFoundSinceLastMember := True;
          LCurrentVisibility := LVisMatch.Groups[3].Value.ToLower;
          var LVisIndent := LVisMatch.Groups[1].Length;
          if LVisIndent <> (LClassIndent + FVisibilityExtraIndent) then
          begin
            ReportViolation(I + 1 + FLineOffset, Format('Visibility keyword should have %d extra spaces (Found %d, Class at %d).', [FVisibilityExtraIndent, LVisIndent - LClassIndent, LClassIndent]));
            Result := False;
          end;
          Continue;
        end;

        var LMember := ParseMember(LLine, I);
        if LMember.MemberType <> mtUnknown then
        begin
          if not Assigned(LCurrentBlock) then
          begin
            LCurrentBlock := TMemberBlock.Create;
            LCurrentBlock.Visibility := LCurrentVisibility;
          end;

          if (LCurrentBlock.Members.Count > 0) then
          begin
            var LCurrentBlockType := LCurrentBlock.Members[0].MemberType;
            var LNeedsNewBlock := False;

            if LMember.MemberType <> LCurrentBlockType then
              LNeedsNewBlock := True;

            if (LMember.MemberType = mtConstructor) and (LCurrentBlockType = mtConstructor) then
              LNeedsNewBlock := False
            else if (LMember.MemberType = mtConstructor) or (LCurrentBlockType = mtConstructor) then
              LNeedsNewBlock := True;

            if (LMember.MemberType = mtClassMethod) or (LCurrentBlockType = mtClassMethod) then
              LNeedsNewBlock := True;

            if LNeedsNewBlock then
            begin
              FlushBlock;
              LCurrentBlock := TMemberBlock.Create;
              LCurrentBlock.Visibility := LCurrentVisibility;
            end;
          end;

          if (LPrevMemberType <> mtUnknown) and (LPrevMemberType <> LMember.MemberType) then
          begin
            if ((LMember.MemberType = mtClassMethod) or (LPrevMemberType = mtClassMethod)) and not LVisibilityKeywordFoundSinceLastMember then
            begin
              ReportViolation(I + 1 + FLineOffset, 'Class members must be separated from instance members by a visibility keyword.');
              Result := False;
            end;
          end;

          LCurrentBlock.Members.Add(LMember);
          LPrevMemberType := LMember.MemberType;
          LVisibilityKeywordFoundSinceLastMember := False;
        end;
      end;
    end;
  finally
    FlushBlock;
    LClassIndents.Free;
  end;
end;

{ TDptLintImplementationFixture }

constructor TDptLintImplementationFixture.Create;
begin
  inherited Create;
  FLineOffset := 0;
end;

procedure TDptLintImplementationFixture.SetContent(const AValue: string);
begin
  FContent := AValue;
end;

procedure TDptLintImplementationFixture.SetLineOffset(const AValue: string);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

function TDptLintImplementationFixture.ValidateClassBanners: Boolean;
var
  LLines: TArray<string>;
  LCurrentClassBanner: string;
  LReportedClasses: TStringList;
  I: Integer;
begin
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  LCurrentClassBanner := '';
  LReportedClasses := TStringList.Create;
  LReportedClasses.Sorted := True;
  LReportedClasses.Duplicates := dupIgnore;
  try
    for I := 0 to High(LLines) do
    begin
      var LLine := LLines[I].Trim;
      if LLine.Contains('=======') then
      begin
        if (I < High(LLines) - 1) and LLines[I + 2].Contains('=======') then
        begin
          var LTopLine := LLines[I];
          var LMidLine := LLines[I + 1];
          var LBotLine := LLines[I + 2];

          if LMidLine.Trim.StartsWith('{') then
          begin
            var LTopOpen := LTopLine.IndexOf('{');
            var LTopClose := LTopLine.LastIndexOf('}');
            var LMidOpen := LMidLine.IndexOf('{');
            var LMidClose := LMidLine.LastIndexOf('}');
            var LBotOpen := LBotLine.IndexOf('{');
            var LBotClose := LBotLine.LastIndexOf('}');

            if (LTopOpen >= 0) and (LMidOpen >= 0) and (LBotOpen >= 0) then
            begin
              if (LTopOpen <> LMidOpen) or (LBotOpen <> LMidOpen) then
              begin
                ReportViolation(I + 2 + FLineOffset, 'Class banner opening brace alignment mismatch.');
                Result := False;
              end;
            end;

            if (LTopClose >= 0) and (LMidClose >= 0) and (LBotClose >= 0) then
            begin
              if (LTopClose <> LMidClose) or (LBotClose <> LMidClose) then
              begin
                ReportViolation(I + 2 + FLineOffset, 'Class banner closing brace alignment mismatch.');
                Result := False;
              end;
            end;
          end;

          var LPotentialClassName := LLines[I + 1].Trim(['{', '}', ' ']);
          if LPotentialClassName <> '' then
            LCurrentClassBanner := LPotentialClassName;
        end;
      end;

      var LMatch := TRegEx.Match(LLine, '^(?:procedure|function|constructor|destructor)\s+([\w\.]+)\.(\w+)', [roIgnoreCase]);
      if LMatch.Success then
      begin
        var LClassName := LMatch.Groups[1].Value;
        if not SameText(LClassName, LCurrentClassBanner) then
        begin
          if LReportedClasses.IndexOf(LClassName) = -1 then
          begin
            ReportViolation(I + 1 + FLineOffset, Format('Missing or incorrect class implementation banner for "%s".', [LClassName]));
            LReportedClasses.Add(LClassName);
            Result := False;
          end;
        end;
      end;
    end;
  finally
    LReportedClasses.Free;
  end;
end;

function TDptLintImplementationFixture.ValidateMethodOrderAndSeparators: Boolean;
var
  LSeparator: string;
  LContentLines: TArray<string>;
  I, J, K: Integer;
begin
  Result := True;
  if not Assigned(FContext) then
    Exit(False);

  LSeparator := FContext.SingleSeparatorLine;
  LContentLines := FContent.Split([sLineBreak]);

  for I := 1 to FContext.Lines.Count - 1 do
  begin
    var LLine := FContext.Lines[I];
    var LTrimmed := LLine.Trim;

    if (LLine <> '') and (LLine[1] > ' ') and
       (LTrimmed.StartsWith('procedure') or LTrimmed.StartsWith('function') or
        LTrimmed.StartsWith('constructor') or LTrimmed.StartsWith('destructor')) then
    begin
      if (I < FLineOffset) or (I >= FLineOffset + Length(LContentLines)) then
        Continue;

      var LHasPrevMethod := False;
      for J := I - 1 downto 0 do
      begin
        var LPrevLine := FContext.Lines[J];
        var LPrevTrimmed := LPrevLine.Trim;
        if (LPrevLine <> '') and (LPrevLine[1] > ' ') and
           (LPrevTrimmed.StartsWith('procedure') or
            LPrevTrimmed.StartsWith('function') or
            LPrevTrimmed.StartsWith('constructor') or
            LPrevTrimmed.StartsWith('destructor')) then
        begin
          LHasPrevMethod := True;
          Break;
        end;
      end;

      if LHasPrevMethod then
      begin
        var LBeforeCommentIdx := I - 1;
        while (LBeforeCommentIdx >= 0) and
              (FContext.Lines[LBeforeCommentIdx].Trim.StartsWith('//') or
               FContext.Lines[LBeforeCommentIdx].Trim.StartsWith('{') or
               FContext.Lines[LBeforeCommentIdx].Trim.StartsWith('(*')) do
        begin
          if FContext.Lines[LBeforeCommentIdx].Contains(LSeparator) or
             FContext.Lines[LBeforeCommentIdx].Contains(FContext.DoubleSeparatorLine) then
            Break;
          Dec(LBeforeCommentIdx);
        end;

        if not ((LBeforeCommentIdx >= 2) and (FContext.Lines[LBeforeCommentIdx].Trim = '') and FContext.Lines[LBeforeCommentIdx - 1].Contains(LSeparator) and (FContext.Lines[LBeforeCommentIdx - 2].Trim = '')) then
        begin
          var LIsAfterBanner := False;
          for K := I - 1 downto 0 do
          begin
            var LPrevK := FContext.Lines[K].Trim;
            if LPrevK = '' then
              Continue;
            if LPrevK.Contains(FContext.DoubleSeparatorLine) then
            begin
              LIsAfterBanner := True;
              Break;
            end;
            Break;
          end;

          if not LIsAfterBanner then
          begin
            ReportViolation(I + 1, 'Methods must be separated by: blank line, separator line (---), and another blank line.');
            Result := False;
          end;
        end;
      end;
    end;
  end;
end;

function TDptLintImplementationFixture.DestructorsMustCallInherited: Boolean;
var
  LLines: TArray<string>;
  LInDestructor, LHasInherited: Boolean;
  LDestructorStartLine, I: Integer;
begin
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  LInDestructor := False;
  LHasInherited := False;
  LDestructorStartLine := 0;

  for I := 0 to High(LLines) do
  begin
    var LLine := LLines[I].Trim.ToLower;
    if not LInDestructor then
    begin
      if LLine.StartsWith('destructor') then
      begin
        LInDestructor := True;
        LHasInherited := False;
        LDestructorStartLine := I + 1;
      end;
    end
    else
    begin
      if TRegEx.IsMatch(LLine, '\binherited\b') then
        LHasInherited := True;
      if LLine.StartsWith('end;') then
      begin
        if not LHasInherited then
        begin
          ReportViolation(LDestructorStartLine + FLineOffset, 'Destructor must call "inherited;".');
          Result := False;
        end;
        LInDestructor := False;
      end;
    end;
  end;
end;

initialization

RegisterSlimFixture(TDptLintUnitContextFixture);
RegisterSlimFixture(TDptLintFixture);
RegisterSlimFixture(TDptLintRegexFixture);
RegisterSlimFixture(TDptLintUsesFixture);
RegisterSlimFixture(TDptLintClassDeclarationFixture);
RegisterSlimFixture(TDptLintImplementationFixture);

end.