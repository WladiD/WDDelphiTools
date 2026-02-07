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

  [SlimFixture('DptLintFixture')]
  TDptLintFixture = class(TSlimFixture)
  private
    FTargetFile: string;
    FLines: TStringList;
    procedure LoadFile(const APath: string);
    function  GetPart(const AStartKey, AEndKey1, AEndKey2, AEndKey3: string; AIncludeStart: Boolean): string;
    function  GetPartOffset(const AStartKey: string; AIncludeStart: Boolean): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Infrastructure
    procedure SetTargetFile(const APath: string);
    class procedure ReportError(const AFile: string; ALine: Integer; const AMsg: string); overload;
    procedure ReportError(ALine: Integer; const AMsg: string); overload;

    // General Purpose Primitives
    function LineStartsWith(ALine: string; const AText: string): Boolean;
    function FileMatchesRegex(const APattern: string): Boolean;
    function FileContainsInclude(const AIncludeNames: string): Boolean;
    function UnitNameMatchesFileName: Boolean;
    function KeywordIsSurroundedBy(const AKeyword, ASeparator: string): Boolean;

    // Unit Part Extractors for further analysis
    function GetBeforeUnitKeywordPart: String;
    function GetInterfacePart: String;
    function GetImplementationPart: String;
    function GetInitializationPart: String;
    function GetFinalizationPart: String;

    function GetBeforeUnitKeywordPartOffset: string;
    function GetInterfacePartOffset: string;
    function GetImplementationPartOffset: string;
    function GetInitializationPartOffset: string;
    function GetFinalizationPartOffset: string;

    function ColonsAreVerticallyAlignedInRange(const AStartLine, AEndLine: string): Boolean;
    function ColonsInRecordsAreAligned: Boolean;
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
    function Fetch(const APattern: string; const AGroupName: String): String;
  end;

  [SlimFixture('DptLintUsesFixture')]
  TDptLintUsesFixture = class(TSlimFixture)
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
    FFile: string;
    FLineOffset: Integer;
    FGroups: TList<TUsesGroup>;
    FUnits: TList<TUnitInfo>;
    procedure ReportError(ALine: Integer; const AMsg: string);
    procedure ParseUnits;
    function MatchNamespace(const AUnitName, APattern: string): Boolean;
  public
    constructor Create(const AFile: string);
    destructor Destroy; override;

    procedure SetContent(const AValue: string);
    procedure SetLineOffset(const AValue: string);
    procedure AddGroupWithNamespaces(const AGroupName: String; const ANamespaces: String);

    function AllUnitsOnSeparateLines: Boolean;
    function GroupsAreSortedAlphabetically: Boolean;
    function GroupsAreSeparatedByBlankLines: Boolean;
    function GroupsFollowOrder: Boolean;
    function BlankLineAfterUsesExists: Boolean;

    function GetLinesAfterLastUsesLine(ALinesCount: Integer): String;
    function GetLinesAfterLastUsesLineOffset(ALinesCount: Integer): string;
  end;

  [SlimFixture('DptLintClassDeclarationFixture')]
  TDptLintClassDeclarationFixture = class(TSlimFixture)
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
        constructor Create;
        destructor Destroy; override;
      end;
  private
    FFile: string;
    FContent: string;
    FLineOffset: Integer;
    FClassNamePrefixes: string;
    FVisibilityExtraIndent: Integer;
    FFieldNamePrefix: string;
    FFieldsMustBeSorted: Boolean;
    FMethodsMustBeSorted: Boolean;
    FPropertiesMustBeSorted: Boolean;
    FAlignMemberNames: Boolean;
    procedure ReportError(ALine: Integer; const AMsg: string);
    function  ParseMember(const ALine: string; ALineIdx: Integer): TMemberInfo;
    procedure ValidateBlock(ABlock: TMemberBlock; var AResult: Boolean);
  public
    constructor Create(const AFile: string);
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

implementation

uses
  System.RegularExpressions;

{ TDptLintFixture }

constructor TDptLintFixture.Create;
begin
  inherited;
  FLines := TStringList.Create;
end;

destructor TDptLintFixture.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TDptLintFixture.LoadFile(const APath: string);
begin
  if FTargetFile <> APath then
  begin
    FTargetFile := APath;
    FLines.LoadFromFile(APath, TEncoding.UTF8);
  end;
end;

procedure TDptLintFixture.SetTargetFile(const APath: string);
begin
  LoadFile(APath);
end;

class procedure TDptLintFixture.ReportError(const AFile: string; ALine: Integer; const AMsg: string);
begin
  TDptLintContext.Add(ALine, AFile, AMsg);
end;

procedure TDptLintFixture.ReportError(ALine: Integer; const AMsg: string);
begin
  ReportError(FTargetFile, ALine, AMsg);
end;

function TDptLintFixture.LineStartsWith(ALine: string; const AText: string): Boolean;
var
  LLine: Integer;
begin
  LLine := StrToIntDef(ALine, 0);
  Result := (LLine > 0) and (LLine <= FLines.Count) and FLines[LLine-1].Trim.StartsWith(AText);
  if not Result then
    ReportError(LLine, Format('Line should start with: "%s"', [AText]));
end;

function TDptLintFixture.FileMatchesRegex(const APattern: string): Boolean;
begin
  Result := TRegEx.IsMatch(FLines.Text, APattern);
  if not Result then
    ReportError(1, Format('File content does not match mandatory pattern: "%s"', [APattern]));
end;

function TDptLintFixture.FileContainsInclude(const AIncludeNames: string): Boolean;
var
  LFound: Boolean;
  LNames: TArray<string>;
begin
  LFound := False;
  LNames := AIncludeNames.Split([';']);
  for var Line in FLines do
  begin
    for var LName in LNames do
      if Line.Contains('{$I ' + LName.Trim + '}') then
      begin
        LFound := True;
        Break;
      end;
    if LFound then Break;
  end;

  Result := LFound;
  if not Result then
    ReportError(1, Format('None of the required includes found: %s', [AIncludeNames]));
end;

function TDptLintFixture.UnitNameMatchesFileName: Boolean;
var
  LFileName: string;
begin
  LFileName := TPath.GetFileNameWithoutExtension(FTargetFile);
  for var I := 0 to FLines.Count - 1 do
    if FLines[I].Trim.StartsWith('unit ', True) then
    begin
      var LUnitName := FLines[I].Trim.Substring(5).Trim([' ', ';']);
      Result := SameText(LUnitName, LFileName);
      if not Result then
        ReportError(I + 1, Format('Unit name "%s" does not match file name "%s".', [LUnitName, LFileName]));
      Exit(Result);
    end;

  ReportError(1, 'No "unit" declaration found.');
  Result := False;
end;

function TDptLintFixture.KeywordIsSurroundedBy(const AKeyword, ASeparator: string): Boolean;
begin
  Result := False;
  for var I := 0 to FLines.Count - 1 do
  begin
    if SameText(FLines[I].Trim, AKeyword) then
    begin
      var LBefore := (I > 0) and FLines[I-1].Contains(ASeparator);
      var LAfter := (I < FLines.Count - 1) and FLines[I+1].Contains(ASeparator);
      Result := LBefore and LAfter;
      if not Result then
        ReportError(I + 1, Format('Keyword "%s" must be surrounded by separators: "%s"', [AKeyword, ASeparator]));
      Exit(Result);
    end;
  end;
  ReportError(1, Format('Keyword "%s" not found.', [AKeyword]));
end;

function TDptLintFixture.GetPart(const AStartKey, AEndKey1, AEndKey2, AEndKey3: string; AIncludeStart: Boolean): string;
var
  LStartIdx, LEndIdx: Integer;
  LBuilder: TStringBuilder;
begin
  LStartIdx := -1;
  LEndIdx := FLines.Count;

  for var I := 0 to FLines.Count - 1 do
  begin
    var LLine := FLines[I].Trim.ToLower;
    if (LStartIdx = -1) and (AStartKey = '') then
      LStartIdx := 0;

    if (LStartIdx = -1) and (LLine.StartsWith(AStartKey.ToLower)) then
    begin
      if AIncludeStart then LStartIdx := I else LStartIdx := I + 1;
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

  if LStartIdx = -1 then Exit('');

  LBuilder := TStringBuilder.Create;
  try
    for var I := LStartIdx to LEndIdx - 1 do
      LBuilder.AppendLine(FLines[I]);
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TDptLintFixture.GetBeforeUnitKeywordPart: String;
begin
  Result := GetPart('', 'unit', '', '', False);
end;

function TDptLintFixture.GetInterfacePart: String;
begin
  Result := GetPart('interface', 'implementation', '', '', False);
end;

function TDptLintFixture.GetImplementationPart: String;
begin
  Result := GetPart('implementation', 'initialization', 'finalization', 'end.', False);
end;

function TDptLintFixture.GetInitializationPart: String;
begin
  Result := GetPart('initialization', 'finalization', 'end.', '', False);
end;

function TDptLintFixture.GetFinalizationPart: String;
begin
  Result := GetPart('finalization', 'end.', '', '', False);
end;

function TDptLintFixture.GetPartOffset(const AStartKey: string; AIncludeStart: Boolean): Integer;
begin
  Result := 0;
  if AStartKey = '' then Exit;

  for var I := 0 to FLines.Count - 1 do
  begin
    if FLines[I].Trim.ToLower.StartsWith(AStartKey.ToLower) then
    begin
      if AIncludeStart then Exit(I) else Exit(I + 1);
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
  LStart, LEnd: Integer;
  LFirstPos: Integer;
  LPos: Integer;
begin
  LStart := StrToIntDef(AStartLine, 0);
  LEnd := StrToIntDef(AEndLine, 0);
  LFirstPos := -1;
  Result := True;

  if (LStart <= 0) or (LEnd < LStart) or (LEnd > FLines.Count) then
  begin
    ReportError(LStart, 'Invalid line range for colon alignment check.');
    Exit(False);
  end;

  for var I := LStart - 1 to LEnd - 1 do
  begin
    LPos := FLines[I].IndexOf(':');
    if LPos >= 0 then
    begin
      if LFirstPos = -1 then
        LFirstPos := LPos
      else if LPos <> LFirstPos then
      begin
        ReportError(I + 1, Format('Colon alignment mismatch. Expected position %d but found %d.', [LFirstPos, LPos]));
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
  LInRecord := False;
  LRecordStart := -1;

  for I := 0 to FLines.Count - 1 do
  begin
    var LLine := FLines[I].Trim.ToLower;
    if not LInRecord then
    begin
      if LLine.Contains('record') then
      begin
        LInRecord := True;
        LRecordStart := I + 1;
      end;
    end
    else
    begin
      if LLine.StartsWith('end;') or LLine.StartsWith('end ') or (LLine = 'end') then
      begin
        if not ColonsAreVerticallyAlignedInRange(IntToStr(LRecordStart), IntToStr(I + 1)) then
          Result := False;
        LInRecord := False;
      end;
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

function TDptLintRegexFixture.Fetch(const APattern: string; const AGroupName: String): String;
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

constructor TDptLintUsesFixture.Create(const AFile: string);
begin
  inherited Create;
  FFile := AFile;
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
  LParts: TArray<string>;
  LPatterns: TList<string>;
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

procedure TDptLintUsesFixture.ReportError(ALine: Integer; const AMsg: string);
begin
  TDptLintFixture.ReportError(FFile, ALine + FLineOffset, AMsg);
end;

function TDptLintUsesFixture.MatchNamespace(const AUnitName, APattern: string): Boolean;
begin
  if APattern = '*' then Exit(True);
  if APattern.EndsWith('.*') then
    Exit(AUnitName.StartsWith(APattern.Substring(0, APattern.Length - 1), True));
  Result := SameText(AUnitName, APattern);
end;

procedure TDptLintUsesFixture.ParseUnits;
var
  LLines: TArray<string>;
  LInUses: Boolean;
begin
  FUnits.Clear;
  LLines := FContent.Split([sLineBreak]);
  LInUses := False;

  for var I := 0 to High(LLines) do
  begin
    var LLine := LLines[I].Trim;
    if not LInUses then
    begin
      if SameText(LLine, 'uses') then LInUses := True;
      Continue;
    end;

    if LLine = '' then Continue;

    var LStrippedLine := LLine;
    var LIsEnd := LStrippedLine.Contains(';');
    if LIsEnd then
      LStrippedLine := LStrippedLine.Substring(0, LStrippedLine.IndexOf(';'));

    var LWords := LStrippedLine.Replace(',', ' ').Split([' ']);
    for var Word in LWords do
    begin
      var LName := Word.Trim;
      if LName = '' then Continue;

      var U: TUnitInfo;
      U.Name := LName;
      U.Line := I + 1; // Relative line in content
      U.GroupIdx := -1;

      // Find group
      var LWildcardMatchGroupIdx: Integer := -1;
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
              U.GroupIdx := GIdx;
              Break;
            end;
          end;
        end;
        if U.GroupIdx <> -1 then Break;
      end;

      if (U.GroupIdx = -1) and (LWildcardMatchGroupIdx <> -1) then
        U.GroupIdx := LWildcardMatchGroupIdx;

      FUnits.Add(U);
    end;

    if LIsEnd then Break;
  end;
end;

function TDptLintUsesFixture.AllUnitsOnSeparateLines: Boolean;
begin
  ParseUnits;
  Result := True;
  var LLinesWithUnits: TDictionary<Integer, Integer>;
  LLinesWithUnits := TDictionary<Integer, Integer>.Create;
  try
    for var U in FUnits do
    begin
      if LLinesWithUnits.ContainsKey(U.Line) then
      begin
        ReportError(U.Line, 'Only one unit allowed per line in uses clause.');
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
    var Prev := FUnits[I-1];
    var Curr := FUnits[I];
    if (Curr.GroupIdx = Prev.GroupIdx) and (CompareText(Curr.Name, Prev.Name) < 0) then
    begin
      ReportError(Curr.Line, Format('Units in group "%s" must be sorted alphabetically: "%s" before "%s".',
        [FGroups[Curr.GroupIdx].Name, Curr.Name, Prev.Name]));
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
    var Prev := FUnits[I-1];
    var Curr := FUnits[I];
    if Curr.GroupIdx > Prev.GroupIdx then
    begin
      // Check if there is a blank line between the units
      // Curr.Line is 1-based index in LLines
      var LFoundBlank: Boolean := False;
      for var LIdx := Prev.Line to Curr.Line - 2 do
        if LLines[LIdx].Trim = '' then
        begin
          LFoundBlank := True;
          Break;
        end;

      if not LFoundBlank then
      begin
        ReportError(Curr.Line, Format('Missing blank line before group "%s".', [FGroups[Curr.GroupIdx].Name]));
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
    var Prev := FUnits[I-1];
    var Curr := FUnits[I];
    if (Curr.GroupIdx <> -1) and (Prev.GroupIdx <> -1) and (Curr.GroupIdx < Prev.GroupIdx) then
    begin
      ReportError(Curr.Line, Format('Group "%s" must appear before group "%s".', 
        [FGroups[Curr.GroupIdx].Name, FGroups[Prev.GroupIdx].Name]));
      Result := False;
    end;
  end;
end;

function TDptLintUsesFixture.BlankLineAfterUsesExists: Boolean;
var
  LLines: TArray<string>;
begin
  Result := False;
  LLines := FContent.Split([sLineBreak]);
  for var I := 0 to High(LLines) do
  begin
    if SameText(LLines[I].Trim, 'uses') then
    begin
      // Check if next line is empty
      if (I < High(LLines)) and (LLines[I+1].Trim = '') then
        Exit(True);
      
      ReportError(I + 1, 'Missing blank line after "uses" keyword.');
      Exit(False);
    end;
  end;
end;

function TDptLintUsesFixture.GetLinesAfterLastUsesLine(ALinesCount: Integer): String;var
  LLines: TArray<string>;
  LInUses: Boolean;
  LLastLineIdx: Integer;
  LBuilder: TStringBuilder;
  LCount: Integer;
begin
  Result := '';
  LLines := FContent.Split([sLineBreak]);
  LInUses := False;
  LLastLineIdx := -1;

  for var I := 0 to High(LLines) do
  begin
    var LLine := LLines[I].Trim;
    if not LInUses then
    begin
      if SameText(LLine, 'uses') then LInUses := True;
      Continue;
    end;

    if LLine.Contains(';') then
    begin
      LLastLineIdx := I;
      Break;
    end;
  end;

  if LLastLineIdx = -1 then Exit;

  LBuilder := TStringBuilder.Create;
  try
    LCount := 0;
    for var I := LLastLineIdx + 1 to High(LLines) do
    begin
      if LCount >= ALinesCount then Break;
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
  LLastLineIdx: Integer;
begin
  Result := '0';
  LLines := FContent.Split([sLineBreak]);
  LInUses := False;
  LLastLineIdx := -1;

  for var I := 0 to High(LLines) do
  begin
    var LLine := LLines[I].Trim;
    if not LInUses then
    begin
      if SameText(LLine, 'uses') then LInUses := True;
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

constructor TDptLintClassDeclarationFixture.Create(const AFile: string);
begin
  inherited Create;
  FFile := AFile;
  FLineOffset := 0;
  // Default values
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

procedure TDptLintClassDeclarationFixture.ReportError(ALine: Integer; const AMsg: string);
begin
  TDptLintFixture.ReportError(FFile, ALine + FLineOffset, AMsg);
end;

function TDptLintClassDeclarationFixture.ParseMember(const ALine: string; ALineIdx: Integer): TMemberInfo;
var
  LMatch: TMatch;
begin
  Result.MemberType := mtUnknown;
  Result.LineIdx := ALineIdx;
  Result.FullLine := ALine;

  // Class Method/Var/Const? (class procedure, class function, class constructor, class destructor, class var)
  LMatch := TRegEx.Match(ALine, '^(\s*)class\s+(procedure|function|constructor|destructor|var)\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtClassMethod;
    Result.Name := LMatch.Groups[3].Value;
    Result.NameStartPos := LMatch.Groups[3].Index;
    Exit;
  end;

  // Property?
  LMatch := TRegEx.Match(ALine, '^(\s*)property\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtProperty;
    Result.Name := LMatch.Groups[2].Value;
    Result.NameStartPos := LMatch.Groups[2].Index;
    Exit;
  end;

  // Constructor/Destructor?
  LMatch := TRegEx.Match(ALine, '^(\s*)(constructor|destructor)\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtConstructor;
    Result.Name := LMatch.Groups[3].Value;
    Result.NameStartPos := LMatch.Groups[3].Index;
    Exit;
  end;

  // Standard Method? (procedure, function)
  LMatch := TRegEx.Match(ALine, '^(\s*)(procedure|function)\s+(\w+)', [roIgnoreCase]);
  if LMatch.Success then
  begin
    Result.MemberType := mtMethod;
    Result.Name := LMatch.Groups[3].Value;
    Result.NameStartPos := LMatch.Groups[3].Index;
    Exit;
  end;

  // Field? (Ident: Type)
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
begin
  if ABlock.Members.Count < 1 then Exit;

  var LFirst := ABlock.Members[0];
  var LMustBeSorted := False;

  case LFirst.MemberType of
    mtField: LMustBeSorted := FFieldsMustBeSorted;
    mtMethod: LMustBeSorted := FMethodsMustBeSorted;
    mtProperty: LMustBeSorted := FPropertiesMustBeSorted;
  end;

    for var I := 0 to ABlock.Members.Count - 1 do
    begin
      var Curr := ABlock.Members[I];
  
      // Check Field Prefix
      if (Curr.MemberType = mtField) and (FFieldNamePrefix <> '') then
      begin
        if not Curr.Name.StartsWith(FFieldNamePrefix) then
        begin
          ReportError(Curr.LineIdx + 1, Format('Field name "%s" should start with prefix "%s".', [Curr.Name, FFieldNamePrefix]));
          AResult := False;
        end;
      end;
  
      if I > 0 then
      begin
        var Prev := ABlock.Members[I-1];
  
        // Check Sorting
        if LMustBeSorted and (CompareText(Curr.Name, Prev.Name) < 0) then
        begin
          ReportError(Curr.LineIdx + 1, Format('Member "%s" should be sorted alphabetically (before "%s").', [Curr.Name, Prev.Name]));
          AResult := False;
        end;
  
        // Check Alignment
        if FAlignMemberNames and (Curr.NameStartPos <> Prev.NameStartPos) then
        begin
          ReportError(Curr.LineIdx + 1, Format('Member "%s" should be vertically aligned with "%s" (Pos %d vs %d).', 
            [Curr.Name, Prev.Name, Curr.NameStartPos, Prev.NameStartPos]));
          AResult := False;
        end;
      end;
    end;
  end;
function TDptLintClassDeclarationFixture.LintClassDeclarations: Boolean;
var
  LLines: TArray<string>;
  LInClass: Boolean;
  LClassIndent: Integer;
  LClassName: string;
  LCurrentBlock: TMemberBlock;
  LPrevMemberType: TMemberType;
  LVisibilityKeywordFoundSinceLastMember: Boolean;

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
  LLines := FContent.Split([sLineBreak]);
  LInClass := False;
  LClassIndent := 0;
  LCurrentBlock := nil;
  LPrevMemberType := mtUnknown;
  LVisibilityKeywordFoundSinceLastMember := False;

  try
    for var I := 0 to High(LLines) do
    begin
      var LLine := LLines[I];
      var LTrimmed := LLine.Trim;

      if LTrimmed = '' then
      begin
        FlushBlock;
        Continue;
      end;

      if not LInClass then
      begin
        var LMatch := TRegEx.Match(LLine, '^(\s*)(\w+)\s*=\s*(?:packed\s+)?class', [roIgnoreCase]);
        if LMatch.Success then
        begin
          LInClass := True;
          LClassIndent := LMatch.Groups[1].Length;
          LClassName := LMatch.Groups[2].Value;
          LPrevMemberType := mtUnknown;
          LVisibilityKeywordFoundSinceLastMember := True; // Assume implicit or about to find
          
          var LPrefixes := FClassNamePrefixes.Split([',']);
          var LPrefixOk := False;
          for var P in LPrefixes do
            if LClassName.StartsWith(P.Trim) then
            begin
              LPrefixOk := True;
              Break;
            end;
          
          if not LPrefixOk then
          begin
            ReportError(I + 1, Format('Class name "%s" should start with one of: %s', [LClassName, FClassNamePrefixes]));
            Result := False;
          end;
        end;
      end;

      if LInClass then
      begin
        if TRegEx.IsMatch(LLine, '^\s*end\s*;', [roIgnoreCase]) then
        begin
          FlushBlock;
          LInClass := False;
          Continue;
        end;

        var LVisMatch := TRegEx.Match(LLine, '^(\s*)(strict\s+)?(?:private|protected|public|published)', [roIgnoreCase]);
        if LVisMatch.Success then
        begin
          FlushBlock;
          LVisibilityKeywordFoundSinceLastMember := True;
          var LVisIndent := LVisMatch.Groups[1].Length;
          if LVisIndent <> (LClassIndent + FVisibilityExtraIndent) then
          begin
            ReportError(I + 1, Format('Visibility keyword should have %d extra spaces (Found %d, Class at %d).', 
              [FVisibilityExtraIndent, LVisIndent - LClassIndent, LClassIndent]));
            Result := False;
          end;
          Continue;
        end;

        var LMember := ParseMember(LLine, I);
        if LMember.MemberType <> mtUnknown then
        begin
          if not Assigned(LCurrentBlock) then
            LCurrentBlock := TMemberBlock.Create;

          // Rule: Class members must be in their own visibility block
          if (LPrevMemberType <> mtUnknown) and (LPrevMemberType <> LMember.MemberType) then
          begin
            if ((LMember.MemberType = mtClassMethod) or (LPrevMemberType = mtClassMethod))
               and not LVisibilityKeywordFoundSinceLastMember then
            begin
              ReportError(I + 1, 'Class members must be separated from instance members by a visibility keyword.');
              Result := False;
            end;
          end;

          // Block flushing for alignment/sorting groups
          if (LCurrentBlock.Members.Count > 0) then
          begin
            var LCurrentBlockType := LCurrentBlock.Members[0].MemberType;
            var LNeedsNewBlock := False;

            if LMember.MemberType <> LCurrentBlockType then
            begin
              if not (((LCurrentBlockType = mtMethod) and (LMember.MemberType = mtProperty)) or
                      ((LCurrentBlockType = mtProperty) and (LMember.MemberType = mtMethod))) then
                LNeedsNewBlock := True;
            end;

            // Rule: Constructors and Destruktors should stay together but be isolated from others
            if (LMember.MemberType = mtConstructor) and (LCurrentBlockType = mtConstructor) then
              LNeedsNewBlock := False
            else if (LMember.MemberType = mtConstructor) or (LCurrentBlockType = mtConstructor) then
              LNeedsNewBlock := True;

            // Rule: Class methods always form their own group
            if (LMember.MemberType = mtClassMethod) or (LCurrentBlockType = mtClassMethod) then LNeedsNewBlock := True;

            if LNeedsNewBlock then
            begin
              FlushBlock;
              LCurrentBlock := TMemberBlock.Create;
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
  end;
end;
initialization
  RegisterSlimFixture(TDptLintFixture);
  RegisterSlimFixture(TDptLintRegexFixture);
  RegisterSlimFixture(TDptLintUsesFixture);
  RegisterSlimFixture(TDptLintClassDeclarationFixture);

end.
