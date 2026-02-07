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
  end;

  [SlimFixture('DptLintRegexFixture')]
  TDptLintRegexFixture = class(TSlimFixture)
  private
    FContent: string;
  public
    constructor Create(const AContent: string);
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
    FGroups: TList<TUsesGroup>;
    FUnits: TList<TUnitInfo>;
    procedure ReportError(ALine: Integer; const AMsg: string);
    procedure ParseUnits;
    function MatchNamespace(const AUnitName, APattern: string): Boolean;
  public
    constructor Create(const AFile: string);
    destructor Destroy; override;

    procedure SetContent(const AValue: string);
    procedure AddGroupWithNamespaces(const AGroupName: String; const ANamespaces: String);

    function AllUnitsOnSeparateLines: Boolean;
    function GroupsAreSortedAlphabetically: Boolean;
    function GroupsAreSeparatedByBlankLines: Boolean;
    function GroupsFollowOrder: Boolean;

    function GetLinesAfterLastUsesLine(ALinesCount: Integer): String;
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

{ TDptLintRegexFixture }

constructor TDptLintRegexFixture.Create(const AContent: string);
begin
  inherited Create;
  FContent := AContent;
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
  TDptLintFixture.ReportError(FFile, ALine, AMsg);
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

function TDptLintUsesFixture.GetLinesAfterLastUsesLine(ALinesCount: Integer): String;
var
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

initialization
  RegisterSlimFixture(TDptLintFixture);
  RegisterSlimFixture(TDptLintRegexFixture);
  RegisterSlimFixture(TDptLintUsesFixture);

end.
