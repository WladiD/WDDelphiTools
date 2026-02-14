// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Fixtures;

interface

uses

  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.RegularExpressions,
  System.SysUtils,
  System.Types,

  Slim.Fixture,
  WDDT.StringTools,

  DPT.Lint.Context;

type

  [SlimFixture('DptLintUnitContextFixture')]
  TDptLintUnitContextFixture = class(TSlimFixture)
  private
    FDoubleSeparatorLine: String;
    FLines              : TStringList;
    FSingleSeparatorLine: String;
    FTargetFile         : String;
    FClassBannerFormats : TStringList;
  public
    constructor Create(const ATargetFile: String);
    destructor  Destroy; override;

    procedure AddClassBannerLineFormat(const AFormat: String);
    procedure DefineDoubleSeparatorLine(const AValue: String);
    procedure DefineSingleSeparatorLine(const AValue: String);
    procedure EnsureCrlf;
    procedure EnsureNoMultipleBlanklines;
    procedure EnsureUtf8Bom;
    function  GetFixture: TSlimFixture;

    property TargetFile: String read FTargetFile;
    property Lines: TStringList read FLines;
    property SingleSeparatorLine: String read FSingleSeparatorLine;
    property DoubleSeparatorLine: String read FDoubleSeparatorLine;
    property ClassBannerFormats: TStringList read FClassBannerFormats;
  end;

  TDptLintBaseFixture = class(TSlimFixture)
  protected
    FContext: TDptLintUnitContextFixture;
    procedure ReportViolation(ALine: Integer; const AMsg: String);
  public
    procedure SetContext(AContext: TSlimFixture);
  end;

  [SlimFixture('DptLintFixture')]
  TDptLintFixture = class(TDptLintBaseFixture)
  private
    function GetPart(const AStartKey, AEndKey1, AEndKey2, AEndKey3: String; AIncludeStart: Boolean): String;
    function GetPartOffset(const AStartKey: String; AIncludeStart: Boolean): Integer;
  public
    class procedure ReportError(const AFile: String; ALine: Integer; const AMsg: String);
  public
    function ColonsAreVerticallyAlignedInRange(const AStartLine, AEndLine: String): Boolean;
    function ColonsInRecordsAreAligned: Boolean;
    function FileContainsInclude(const AIncludeNames: String): Boolean;
    function FileMatchesRegex(const APattern: String): Boolean;
    function GetBeforeUnitKeywordPart: String;
    function GetBeforeUnitKeywordPartOffset: String;
    function GetFinalizationPart: String;
    function GetFinalizationPartOffset: String;
    function GetImplementationPart: String;
    function GetImplementationPartOffset: String;
    function GetInitializationPart: String;
    function GetInitializationPartOffset: String;
    function GetInterfacePart: String;
    function GetInterfacePartOffset: String;
    function KeywordIsSurroundedBy(const AKeyword, ASeparator: String): Boolean;
    function LastLineBeforeEndIsDoubleSeparator: Boolean;
    function LineStartsWith(ALine: String; const AText: String): Boolean;
    function UnitNameMatchesFileName: Boolean;
  end;

  [SlimFixture('DptLintRegexFixture')]
  TDptLintRegexFixture = class(TSlimFixture)
  private
    FContent: String;
    FLineOffset: Integer;
  public
    constructor Create(const AContent: String);
    function  Fetch(const APattern: String; const AGroupName: String): String;
    function  Match(const APattern: String): Boolean;
    procedure SetLineOffset(const AValue: String);
  end;

  [SlimFixture('DptLintUsesFixture')]
  TDptLintUsesFixture = class(TDptLintBaseFixture)
  type
    TUsesGroup = record
      Name    : String;
      Patterns: TArray<String>;
    end;
    TUnitInfo = record
      GroupIdx: Integer;
      Line    : Integer; // Local line within FContent
      Name    : String;
    end;
  private
    FContent   : String;
    FGroups    : TList<TUsesGroup>;
    FLineOffset: Integer;
    FUnits     : TList<TUnitInfo>;
    function  MatchNamespace(const AUnitName, APattern: String): Boolean;
    procedure ParseUnits;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure AddGroupWithNamespaces(const AGroupName: String; const ANamespaces: String);
    function  AllUnitsOnSeparateLines: Boolean;
    function  BlankLineAfterUsesExists: Boolean;
    function  GetLinesAfterLastUsesLine(ALinesCount: Integer): String;
    function  GetLinesAfterLastUsesLineOffset(ALinesCount: Integer): String;
    function  GroupsAreSeparatedByBlankLines: Boolean;
    function  GroupsAreSortedAlphabetically: Boolean;
    function  GroupsFollowOrder: Boolean;
    procedure SetContent(const AValue: String);
    procedure SetLineOffset(const AValue: String);
  end;

  [SlimFixture('DptLintClassDeclarationFixture')]
  TDptLintClassDeclarationFixture = class(TDptLintBaseFixture)
  private
    type
      TMemberType = (mtField, mtMethod, mtProperty, mtClassMethod, mtConstructor, mtUnknown);
      TMemberInfo = record
        FullLine    : String;
        LineIdx     : Integer;
        MemberType  : TMemberType;
        Name        : String;
        NameStartPos: Integer;
      end;
      TMemberBlock = class
        Members   : TList<TMemberInfo>;
        Visibility: String;
        constructor Create;
        destructor Destroy; override;
      end;
  private
    FAlignMemberNames      : Boolean;
    FClassNamePrefixes     : String;
    FContent               : String;
    FFieldNamePrefix       : String;
    FFieldsMustBeSorted    : Boolean;
    FLineOffset            : Integer;
    FMethodsMustBeSorted   : Boolean;
    FPropertiesMustBeSorted: Boolean;
    FVisibilityExtraIndent : Integer;
    function  ParseMember(const ALine: String; ALineIdx: Integer): TMemberInfo;
    procedure ValidateBlock(ABlock: TMemberBlock; var AResult: Boolean);
  public
    constructor Create;
    function  LintClassDeclarations: Boolean;
    procedure SetContent(const AValue: String);
    procedure SetLineOffset(const AValue: String);
    property AlignMemberNames: Boolean read FAlignMemberNames write FAlignMemberNames;
    property ClassNamePrefixes: String read FClassNamePrefixes write FClassNamePrefixes;
    property FieldNamePrefix: String read FFieldNamePrefix write FFieldNamePrefix;
    property FieldsMustBeSorted: Boolean read FFieldsMustBeSorted write FFieldsMustBeSorted;
    property MethodsMustBeSorted: Boolean read FMethodsMustBeSorted write FMethodsMustBeSorted;
    property PropertiesMustBeSorted: Boolean read FPropertiesMustBeSorted write FPropertiesMustBeSorted;
    property VisibilityExtraIndent: Integer read FVisibilityExtraIndent write FVisibilityExtraIndent;
  end;

  [SlimFixture('DptLintImplementationFixture')]
  TDptLintImplementationFixture = class(TDptLintBaseFixture)
  private
    FContent   : String;
    FLineOffset: Integer;
  public
    constructor Create;
    function  DestructorsMustCallInherited: Boolean;
    procedure SetContent(const AValue: String);
    procedure SetLineOffset(const AValue: String);
    function  ValidateClassBanners: Boolean;
    function  ValidateMethodOrderAndSeparators: Boolean;
  end;

  [SlimFixture('DptLintMethodBodyFixture')]
  TDptLintMethodBodyFixture = class(TDptLintBaseFixture)
  public type
    TSpaceRule = (srForbidden, srRequired, srIgnore);
  private
    FArithmeticOperators : String;
    FArithmeticRuleAfter : TSpaceRule;
    FArithmeticRuleBefore: TSpaceRule;
    FAssignmentRuleAfter : TSpaceRule;
    FAssignmentRuleBefore: TSpaceRule;
    FContent             : String;
    FLineOffset          : Integer;
    function  StringToRule(const AValue: String): TSpaceRule;
    procedure ValidateOp(ALineIdx: Integer; const ACode, AOp, AOpName: String; ARuleBefore, ARuleAfter: TSpaceRule);
  public
    constructor Create;
    procedure SetArithmeticSpaceAfter(const AValue: String);
    procedure SetArithmeticSpaceBefore(const AValue: String);
    procedure SetAssignmentSpaceAfter(const AValue: String);
    procedure SetAssignmentSpaceBefore(const AValue: String);
    procedure SetContent(const AValue: String);
    procedure SetLineOffset(const AValue: String);
    function  ValidateOperatorSpacing: Boolean;
    property ArithmeticOperators: String read FArithmeticOperators write FArithmeticOperators;
  end;

  [SlimFixture('DptLintLocalVarFixture')]
  TDptLintLocalVarFixture = class(TDptLintBaseFixture)
  private
    FAlignColons          : Boolean;
    FContent              : String;
    FExactSpacesAfterColon: Integer;
    FLineOffset           : Integer;
    FVariablesMustBeSorted: Boolean;
    function ValidateVarBlock(AVarLines: TList<string>; AStartLineIdx: Integer): Boolean;
  public
    constructor Create;
    function  LintLocalVars: Boolean;
    procedure SetContent(const AValue: String);
    procedure SetLineOffset(const AValue: String);
    property AlignColons: Boolean read FAlignColons write FAlignColons;
    property ExactSpacesAfterColon: Integer read FExactSpacesAfterColon write FExactSpacesAfterColon;
    property VariablesMustBeSorted: Boolean read FVariablesMustBeSorted write FVariablesMustBeSorted;
  end;

implementation

{ TDptLintUnitContextFixture }

constructor TDptLintUnitContextFixture.Create(const ATargetFile: String);
begin
  inherited Create;
  FTargetFile := ATargetFile;
  FLines := TStringList.Create;
  FClassBannerFormats := TStringList.Create;

  if FileExists(FTargetFile) then
    FLines.LoadFromFile(FTargetFile, TEncoding.UTF8);

  FSingleSeparatorLine := '-------';
  FDoubleSeparatorLine := '=======';
end;

destructor TDptLintUnitContextFixture.Destroy;
begin
  FClassBannerFormats.Free;
  FLines.Free;
  inherited;
end;

procedure TDptLintUnitContextFixture.AddClassBannerLineFormat(const AFormat: String);
begin
  FClassBannerFormats.Add(AFormat);
end;

procedure TDptLintUnitContextFixture.DefineSingleSeparatorLine(const AValue: String);
begin
  var LVal: String := AValue.Trim(['{', '}', ' ', '!', '-']);
  if LVal <> '' then
    FSingleSeparatorLine := LVal;
end;

procedure TDptLintUnitContextFixture.DefineDoubleSeparatorLine(const AValue: String);
begin
  var LVal: String := AValue.Trim(['{', '}', ' ', '!', '=', '-']);
  if LVal <> '' then
    FDoubleSeparatorLine := LVal;
end;

function TDptLintUnitContextFixture.GetFixture: TSlimFixture;
begin
  Result := Self;
end;

procedure TDptLintUnitContextFixture.EnsureUtf8Bom;
var
  LHasBom  : Boolean;
  LPreamble: TBytes;
  LStream  : TFileStream;
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
  LFoundLFOnly: Boolean;
  LText       : String;
begin
  LText := TFile.ReadAllText(FTargetFile);

  LFoundLFOnly := False;
  for var I: Integer := 1 to LText.Length do
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
    var LFixed: String := LText.Replace(#13#10, #10).Replace(#13, #10).Replace(#10, #13#10);
    TFile.WriteAllText(FTargetFile, LFixed, TEncoding.UTF8);
    FLines.LoadFromFile(FTargetFile, TEncoding.UTF8); // Reload lines
    TDptLintContext.AddWarning(Format('Warning: Line endings in file "%s" were converted to CRLF.', [ExtractFileName(FTargetFile)]));
  end;
end;

procedure TDptLintUnitContextFixture.EnsureNoMultipleBlanklines;
var
  LNewText: String;
  LRegex  : TRegEx;
  LText   : String;
begin
  LText := TFile.ReadAllText(FTargetFile, TEncoding.UTF8);
  
  // Pattern matches 3 or more consecutive LineBreaks (interspersed with optional whitespace)
  // This corresponds to 2 or more blank lines.
  // We want to reduce this to 2 LineBreaks (1 blank line).
  // Note: We assume CRLF because EnsureCrlf should have run before. 
  // However, to be robust, we match \r\n explicitly as we are on Windows/Delphi usually handling this.
  
  LRegex := TRegEx.Create('(\r\n[ \t]*){3,}');
  
  if LRegex.IsMatch(LText) then
  begin
    LNewText := LRegex.Replace(LText, #13#10#13#10);
    TFile.WriteAllText(FTargetFile, LNewText, TEncoding.UTF8);
    FLines.LoadFromFile(FTargetFile, TEncoding.UTF8);
    TDptLintContext.AddWarning(Format('Warning: Multiple blank lines in file "%s" were collapsed.', [ExtractFileName(FTargetFile)]));
  end;
end;

{ TDptLintBaseFixture }

procedure TDptLintBaseFixture.SetContext(AContext: TSlimFixture);
begin
  if AContext is TDptLintUnitContextFixture then
    FContext := TDptLintUnitContextFixture(AContext);
end;

procedure TDptLintBaseFixture.ReportViolation(ALine: Integer; const AMsg: String);
begin
  if Assigned(FContext) then
    TDptLintFixture.ReportError(FContext.TargetFile, ALine, AMsg)
  else
    TDptLintFixture.ReportError('', ALine, AMsg);
end;

{ TDptLintFixture }

class procedure TDptLintFixture.ReportError(const AFile: String; ALine: Integer; const AMsg: String);
begin
  TDptLintContext.Add(ALine, AFile, AMsg);
end;

function TDptLintFixture.LineStartsWith(ALine: String; const AText: String): Boolean;
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

function TDptLintFixture.FileMatchesRegex(const APattern: String): Boolean;
begin
  Result := Assigned(FContext) and TRegEx.IsMatch(FContext.Lines.Text, APattern);

  if not Result then
    ReportViolation(1, Format('File content does not match mandatory pattern: "%s"', [APattern]));
end;

function TDptLintFixture.FileContainsInclude(const AIncludeNames: String): Boolean;
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
  LFileName: String;
begin
  if not Assigned(FContext) then
    Exit(False);

  LFileName := TPath.GetFileNameWithoutExtension(FContext.TargetFile);
  for var I: Integer := 0 to FContext.Lines.Count - 1 do
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

function TDptLintFixture.KeywordIsSurroundedBy(const AKeyword, ASeparator: String): Boolean;
var
  LSeparator: String;
begin
  Result := False;
  if not Assigned(FContext) then
    Exit(False);

  LSeparator := ASeparator;
  if LSeparator = '' then
    LSeparator := FContext.DoubleSeparatorLine;

  for var I: Integer := 0 to FContext.Lines.Count - 1 do
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

function TDptLintFixture.GetPart(const AStartKey, AEndKey1, AEndKey2, AEndKey3: String; AIncludeStart: Boolean): String;
var
  I: Integer;
  LBuilder : TStringBuilder;
  LEndIdx  : Integer;
  LStartIdx: Integer;
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

function TDptLintFixture.GetPartOffset(const AStartKey: String; AIncludeStart: Boolean): Integer;
begin
  Result := 0;
  if not Assigned(FContext) or (AStartKey = '') then
    Exit;

  for var I: Integer := 0 to FContext.Lines.Count - 1 do
  begin
    if FContext.Lines[I].Trim.StartsWith(AStartKey, True) then
    begin
      if AIncludeStart then
        Exit(I)
      else
        Exit(I + 1);
    end;
  end;
end;

function TDptLintFixture.GetBeforeUnitKeywordPartOffset: String;
begin
  Result := '0';
end;

function TDptLintFixture.GetInterfacePartOffset: String;
begin
  Result := IntToStr(GetPartOffset('interface', False));
end;

function TDptLintFixture.GetImplementationPartOffset: String;
begin
  Result := IntToStr(GetPartOffset('implementation', False));
end;

function TDptLintFixture.GetInitializationPartOffset: String;
begin
  Result := IntToStr(GetPartOffset('initialization', False));
end;

function TDptLintFixture.GetFinalizationPartOffset: String;
begin
  Result := IntToStr(GetPartOffset('finalization', False));
end;

function TDptLintFixture.ColonsAreVerticallyAlignedInRange(const AStartLine, AEndLine: String): Boolean;
var
  LEnd     : Integer;
  LFirstPos: Integer;
  LPos     : Integer;
  LStart   : Integer;
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

  for var I: Integer := LStart - 1 to LEnd - 1 do
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
  LInRecord   : Boolean;
  LRecordStart: Integer;
begin
  Result := True;
  if not Assigned(FContext) then
    Exit(False);

  LInRecord := False;
  LRecordStart := -1;

  for var I: Integer := 0 to FContext.Lines.Count - 1 do
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
  J: Integer;
begin
  Result := False;
  if not Assigned(FContext) then
    Exit(False);

  for var I: Integer := FContext.Lines.Count - 1 downto 0 do
  begin
    if not SameText(FContext.Lines[I].Trim, 'end.') then
      Continue;

    var LFound: Boolean := False;
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

{ TDptLintRegexFixture }

constructor TDptLintRegexFixture.Create(const AContent: String);
begin
  inherited Create;
  FContent := AContent;
  FLineOffset := 0;
end;

procedure TDptLintRegexFixture.SetLineOffset(const AValue: String);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

function TDptLintRegexFixture.Match(const APattern: String): Boolean;
begin
  if FContent = '' then
    Exit(True);
  Result := TRegEx.IsMatch(FContent, APattern, [roSingleLine, roIgnoreCase]);
end;

function TDptLintRegexFixture.Fetch(const APattern: String; const AGroupName: String): String;
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

procedure TDptLintUsesFixture.SetContent(const AValue: String);
begin
  FContent := AValue;
end;

procedure TDptLintUsesFixture.SetLineOffset(const AValue: String);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

procedure TDptLintUsesFixture.AddGroupWithNamespaces(const AGroupName, ANamespaces: String);
var
  G        : TUsesGroup;
  LParts   : TArray<String>;
  LPatterns: TList<String>;
begin
  G.Name := AGroupName;
  LParts := ANamespaces.Replace(',', ' ').Replace(';', ' ').Split([' ']);
  LPatterns := TList<String>.Create;
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

function TDptLintUsesFixture.MatchNamespace(const AUnitName, APattern: String): Boolean;
var
  LSub: String;
begin
  if APattern = '*' then
    Exit(True);

  if APattern.StartsWith('*') then
  begin
    if APattern.EndsWith('*') then
    begin
      // *Contains*: Remove first and last *
      LSub := APattern.Substring(1, APattern.Length - 2);
      Exit(AUnitName.ToLower.IndexOf(LSub.ToLower) >= 0);
    end
    else
    begin
      // *EndsWith
      LSub := APattern.Substring(1);
      Exit(AUnitName.EndsWith(LSub, True));
    end;
  end;

  if APattern.EndsWith('.*') then
    Exit(AUnitName.StartsWith(APattern.Substring(0, APattern.Length - 1), True));
  Result := SameText(AUnitName, APattern);
end;

procedure TDptLintUsesFixture.ParseUnits;
var
  I      : Integer;
  LInUses: Boolean;
  LLines : TArray<String>;
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
    for var U: TUnitInfo in FUnits do
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
  for var I: Integer := 1 to FUnits.Count - 1 do
  begin
    var Prev: TUnitInfo := FUnits[I - 1];
    var Curr: TUnitInfo := FUnits[I];
    if (Curr.GroupIdx = Prev.GroupIdx) and (CompareStringNatural(Curr.Name, Prev.Name) < 0) then
    begin
      ReportViolation(Curr.Line + FLineOffset, Format('Units in group "%s" must be sorted alphabetically: "%s" before "%s".', [FGroups[Curr.GroupIdx].Name, Curr.Name, Prev.Name]));
      Result := False;
    end;
  end;
end;

function TDptLintUsesFixture.GroupsAreSeparatedByBlankLines: Boolean;
var
  LLines: TArray<String>;
begin
  ParseUnits;
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  for var I: Integer := 1 to FUnits.Count - 1 do
  begin
    var Prev: TUnitInfo := FUnits[I - 1];
    var Curr: TUnitInfo := FUnits[I];
    if Curr.GroupIdx > Prev.GroupIdx then
    begin
      var LFoundBlank: Boolean := False;
      for var LIdx: Integer := Prev.Line to Curr.Line - 2 do
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
  for var I: Integer := 1 to FUnits.Count - 1 do
  begin
    var Prev: TUnitInfo := FUnits[I - 1];
    var Curr: TUnitInfo := FUnits[I];
    if (Curr.GroupIdx <> -1) and (Prev.GroupIdx <> -1) and (Curr.GroupIdx < Prev.GroupIdx) then
    begin
      ReportViolation(Curr.Line + FLineOffset, Format('Group "%s" must appear before group "%s".', [FGroups[Curr.GroupIdx].Name, FGroups[Prev.GroupIdx].Name]));
      Result := False;
    end;
  end;
end;

function TDptLintUsesFixture.BlankLineAfterUsesExists: Boolean;
var
  LLines: TArray<String>;
begin
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  for var I: Integer := 0 to High(LLines) do
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

function TDptLintUsesFixture.GetLinesAfterLastUsesLine(ALinesCount: Integer): String;
var
  I           : Integer;
  LBuilder    : TStringBuilder;
  LCount      : Integer;
  LInUses     : Boolean;
  LLastLineIdx: Integer;
  LLines      : TArray<String>;
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

function TDptLintUsesFixture.GetLinesAfterLastUsesLineOffset(ALinesCount: Integer): String;
var
  LInUses     : Boolean;
  LLastLineIdx: Integer;
  LLines      : TArray<String>;
begin
  Result := '0';
  LLines := FContent.Split([sLineBreak]);
  LInUses := False;
  LLastLineIdx := -1;

  for var I: Integer := 0 to High(LLines) do
  begin
    var LLine: String := LLines[I].Trim;
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

procedure TDptLintClassDeclarationFixture.SetContent(const AValue: String);
begin
  FContent := AValue;
end;

procedure TDptLintClassDeclarationFixture.SetLineOffset(const AValue: String);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

function TDptLintClassDeclarationFixture.ParseMember(const ALine: String; ALineIdx: Integer): TMemberInfo;
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
  LFirst       : TMemberInfo;
  LMustBeSorted: Boolean;
begin
  if ABlock.Members.Count < 1 then
    Exit;

  LFirst := ABlock.Members[0];
  LMustBeSorted := False;
  case LFirst.MemberType of
    mtField:
      LMustBeSorted := FFieldsMustBeSorted;
    mtMethod:
      LMustBeSorted := FMethodsMustBeSorted;
    mtProperty:
      LMustBeSorted := FPropertiesMustBeSorted;
  end;

  for var I: Integer := 0 to ABlock.Members.Count - 1 do
  begin
    var Curr: TMemberInfo := ABlock.Members[I];
    if (Curr.MemberType = mtField) and (FFieldNamePrefix <> '') then
    begin
      // Rule: Published fields are allowed without 'F' prefix
      if not SameText(ABlock.Visibility, 'published') and
         not Curr.Name.StartsWith(FFieldNamePrefix) then
      begin
        ReportViolation(Curr.LineIdx + 1 + FLineOffset, Format('Field name "%s" should start with prefix "%s".', [Curr.Name, FFieldNamePrefix]));
        AResult := False;
      end;
    end;

    if I > 0 then
    begin
      var Prev: TMemberInfo := ABlock.Members[I - 1];
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
  I                         : Integer;
  LClassIndent              : Integer;
  LClassIndents             : TList<Integer>;
  LClassName                : String;
  LCurrentBlock             : TMemberBlock;
  LCurrentVisibility        : String;
  LLines                    : TArray<string>;
  LNestedDepth              : Integer;
  LPrevMemberType           : TMemberType;
  LVisibilitySinceLastMember: Boolean;

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
  LNestedDepth := 0;
  LCurrentBlock := nil;
  LPrevMemberType := mtUnknown;
  LVisibilitySinceLastMember := False;
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
        LVisibilitySinceLastMember := True;
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
        if TRegEx.IsMatch(LLine, '^\s*end\s*[;$]', [roIgnoreCase]) then
        begin
          if LNestedDepth > 0 then
          begin
            Dec(LNestedDepth);
            Continue;
          end;

          FlushBlock;
          LClassIndents.Delete(LClassIndents.Count - 1);
          if LClassIndents.Count > 0 then
            LClassIndent := LClassIndents[LClassIndents.Count - 1]
          else
            LClassIndent := 0;
          Continue;
        end;

        if LNestedDepth > 0 then
          Continue;

        if TRegEx.IsMatch(LLine, '=\s*(?:packed\s+)?(?:record|interface|dispinterface|object)', [roIgnoreCase]) then
        begin
          Inc(LNestedDepth);
          Continue;
        end;

        var LVisMatch := TRegEx.Match(LLine, '^(\s*)(strict\s+)?(private|protected|public|published)', [roIgnoreCase]);
        if LVisMatch.Success then
        begin
          FlushBlock;
          LVisibilitySinceLastMember := True;
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

          if (LPrevMemberType <> mtUnknown) and
             (LPrevMemberType <> LMember.MemberType) and
             (
               (
                 (LMember.MemberType = mtClassMethod) or
                 (LPrevMemberType = mtClassMethod)
               ) and
               not LVisibilitySinceLastMember
             ) then
          begin
            ReportViolation(I + 1 + FLineOffset, 'Class members must be separated from instance members by a visibility keyword.');
            Result := False;
          end;

          LCurrentBlock.Members.Add(LMember);
          LPrevMemberType := LMember.MemberType;
          LVisibilitySinceLastMember := False;
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

procedure TDptLintImplementationFixture.SetContent(const AValue: String);
begin
  FContent := AValue;
end;

procedure TDptLintImplementationFixture.SetLineOffset(const AValue: String);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

function TDptLintImplementationFixture.ValidateClassBanners: Boolean;
var
  LCurrentClassBanner: String;
  LLines             : TArray<String>;
  LReportedClasses   : TStringList;
begin
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  LCurrentClassBanner := '';
  LReportedClasses := TStringList.Create;
  try
    LReportedClasses.Sorted := True;
    LReportedClasses.Duplicates := dupIgnore;
    for var I: Integer := 0 to High(LLines) do
    begin
      var LLine: String := LLines[I].Trim;
      if LLine.Contains('=======') then
      begin
        if (I < High(LLines) - 1) and LLines[I + 2].Contains('=======') then
        begin
          var LTopLine: String := LLines[I];
          var LMidLine: String := LLines[I + 1];
          var LBotLine: String := LLines[I + 2];

          if LMidLine.Trim.StartsWith('{') then
          begin
            var LTopOpen: Integer := LTopLine.IndexOf('{');
            var LTopClose: Integer := LTopLine.LastIndexOf('}');
            var LMidOpen: Integer := LMidLine.IndexOf('{');
            var LMidClose: Integer := LMidLine.LastIndexOf('}');
            var LBotOpen: Integer := LBotLine.IndexOf('{');
            var LBotClose: Integer := LBotLine.LastIndexOf('}');

            if (LTopOpen >= 0) and
               (LMidOpen >= 0) and
               (LBotOpen >= 0) and
               (
                 (LTopOpen <> LMidOpen) or
                 (LBotOpen <> LMidOpen)
               ) then
            begin
              ReportViolation(I + 2 + FLineOffset, 'Class banner opening brace alignment mismatch.');
              Result := False;
            end;

            if (LTopClose >= 0) and
               (LMidClose >= 0) and
               (LBotClose >= 0) and
               (
                 (LTopClose <> LMidClose) or
                 (LBotClose <> LMidClose)
               ) then
            begin
              ReportViolation(I + 2 + FLineOffset, 'Class banner closing brace alignment mismatch.');
              Result := False;
            end;
          end;

          var LPotentialClassName := LLines[I + 1].Trim(['{', '}', ' ']);
          if LPotentialClassName <> '' then
            LCurrentClassBanner := LPotentialClassName;
        end;
      end;

      var LMatch: TMatch := TRegEx.Match(LLine, '^(?:procedure|function|constructor|destructor)\s+([\w\.]+)\.(\w+)', [roIgnoreCase]);
      if LMatch.Success then
      begin
        var LClassName := LMatch.Groups[1].Value;
        if Assigned(FContext) and (FContext.ClassBannerFormats.Count > 0) then
        begin
          if not SameText(LClassName, LCurrentClassBanner) and
             (LReportedClasses.IndexOf(LClassName) = -1) then
          begin
            var LExpectedBanner := '';
            for var LFormat in FContext.ClassBannerFormats do
            begin
              if LExpectedBanner <> '' then LExpectedBanner := LExpectedBanner + sLineBreak;
              try
                if LFormat.Contains('%') then
                  LExpectedBanner := LExpectedBanner + Format(LFormat, [LClassName])
                else
                  LExpectedBanner := LExpectedBanner + LFormat;
              except
                on E: Exception do
                  LExpectedBanner := LExpectedBanner + 'Invalid Format: ' + LFormat;
              end;
            end;

            ReportViolation(I + 1 + FLineOffset,
              Format('Missing or incorrect class implementation banner for "%s".' + sLineBreak +
                     'Expected:' + sLineBreak + '%s', [LClassName, LExpectedBanner]));
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
  J            : Integer;
  K            : Integer;
  LContentLines: TArray<string>;
  LSeparator   : String;
begin
  Result := True;
  if not Assigned(FContext) then
    Exit(False);

  LSeparator := FContext.SingleSeparatorLine;
  LContentLines := FContent.Split([sLineBreak]);

  for var I: Integer := 1 to FContext.Lines.Count - 1 do
  begin
    var LLine: String := FContext.Lines[I];
    var LTrimmed: String := LLine.Trim;

    if (LLine <> '') and
       (LLine[1] > ' ') and
       (
         LTrimmed.StartsWith('procedure') or
         LTrimmed.StartsWith('function') or
         LTrimmed.StartsWith('constructor') or
         LTrimmed.StartsWith('destructor')
       ) then
    begin
      if (I < FLineOffset) or (I >= FLineOffset + Length(LContentLines)) then
        Continue;

      var LHasPrevMethod: Boolean := False;
      for J := I - 1 downto 0 do
      begin
        var LPrevLine: String := FContext.Lines[J];
        var LPrevTrimmed: String := LPrevLine.Trim;
        if (LPrevLine <> '') and
           (LPrevLine[1] > ' ') and
           (
             LPrevTrimmed.StartsWith('procedure') or
             LPrevTrimmed.StartsWith('function') or
             LPrevTrimmed.StartsWith('constructor') or
             LPrevTrimmed.StartsWith('destructor')
           ) then
        begin
          LHasPrevMethod := True;
          Break;
        end;
      end;

      if LHasPrevMethod then
      begin
        var LBeforeCommentIdx: Integer := I - 1;
        while (LBeforeCommentIdx >= 0) and
              (
                FContext.Lines[LBeforeCommentIdx].Trim.StartsWith('//') or
                FContext.Lines[LBeforeCommentIdx].Trim.StartsWith('{') or
                FContext.Lines[LBeforeCommentIdx].Trim.StartsWith('(*')
              ) do
        begin
          if FContext.Lines[LBeforeCommentIdx].Contains(LSeparator) or
             FContext.Lines[LBeforeCommentIdx].Contains(FContext.DoubleSeparatorLine) then
            Break;
          Dec(LBeforeCommentIdx);
        end;

        if not
           (
             (LBeforeCommentIdx >= 2) and
             (FContext.Lines[LBeforeCommentIdx].Trim = '') and
             FContext.Lines[LBeforeCommentIdx - 1].Contains(LSeparator) and
             (FContext.Lines[LBeforeCommentIdx - 2].Trim = '')
           ) then
        begin
          var LIsAfterBanner: Boolean := False;
          for K := I - 1 downto 0 do
          begin
            var LPrevK: String := FContext.Lines[K].Trim;
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
  I                   : Integer;
  LDestructorStartLine: Integer;
  LHasInherited       : Boolean;
  LInDestructor       : Boolean;
  LLines              : TArray<string>;
begin
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  LInDestructor := False;
  LHasInherited := False;
  LDestructorStartLine := 0;

  for I := 0 to High(LLines) do
  begin
    var LLine: String := LLines[I].Trim.ToLower;
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

{ TDptLintMethodBodyFixture }

constructor TDptLintMethodBodyFixture.Create;
begin
  inherited Create;
  FAssignmentRuleBefore := srForbidden;
  FAssignmentRuleAfter := srForbidden;
  FArithmeticRuleBefore := srForbidden;
  FArithmeticRuleAfter := srForbidden;
  FArithmeticOperators := '+,-,*,/';
end;

function TDptLintMethodBodyFixture.StringToRule(const AValue: String): TSpaceRule;
begin
  if SameText(AValue, 'Forbidden') then
    Result := srForbidden
  else if SameText(AValue, 'Required') then
    Result := srRequired
  else
    Result := srIgnore;
end;

procedure TDptLintMethodBodyFixture.SetAssignmentSpaceBefore(const AValue: String);
begin
  FAssignmentRuleBefore := StringToRule(AValue);
end;

procedure TDptLintMethodBodyFixture.SetAssignmentSpaceAfter(const AValue: String);
begin
  FAssignmentRuleAfter := StringToRule(AValue);
end;

procedure TDptLintMethodBodyFixture.SetArithmeticSpaceBefore(const AValue: String);
begin
  FArithmeticRuleBefore := StringToRule(AValue);
end;

procedure TDptLintMethodBodyFixture.SetArithmeticSpaceAfter(const AValue: String);
begin
  FArithmeticRuleAfter := StringToRule(AValue);
end;

procedure TDptLintMethodBodyFixture.SetContent(const AValue: String);
begin
  FContent := AValue;
end;

procedure TDptLintMethodBodyFixture.SetLineOffset(const AValue: String);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

procedure TDptLintMethodBodyFixture.ValidateOp(ALineIdx: Integer; const ACode, AOp, AOpName: String; ARuleBefore, ARuleAfter: TSpaceRule);
var
  LEscaped: String;
begin
  LEscaped := TRegEx.Escape(AOp);

  // Rule BEFORE
  case ARuleBefore of
    srForbidden:
      if TRegEx.IsMatch(ACode, '\S\s+' + LEscaped) then
        ReportViolation(ALineIdx + 1 + FLineOffset, Format('No space allowed before operator "%s".', [AOpName]));
    srRequired:
      if TRegEx.IsMatch(ACode, '\S' + LEscaped) then
        ReportViolation(ALineIdx + 1 + FLineOffset, Format('Space required before operator "%s".', [AOpName]));
  end;

  // Rule AFTER
  // Exception: If operator is at end of code (followed only by whitespace), we don't enforce "Required"
  case ARuleAfter of
    srForbidden:
      if TRegEx.IsMatch(ACode, LEscaped + '\s+\S') then
        ReportViolation(ALineIdx + 1 + FLineOffset, Format('No space allowed after operator "%s".', [AOpName]));
    srRequired:
      if TRegEx.IsMatch(ACode, LEscaped + '\S') then
        ReportViolation(ALineIdx + 1 + FLineOffset, Format('Space required after operator "%s".', [AOpName]));
  end;
end;

function TDptLintMethodBodyFixture.ValidateOperatorSpacing: Boolean;
var
  C                : Char;
  J                : Integer;
  LCode            : String;
  LInCurlyComment  : Boolean;
  LInParenComment  : Boolean;
  LInString        : Boolean;
  LLine            : String;
  LLines           : TArray<String>;
  LViolationsBefore: Integer;
  NextC            : Char;
begin
  LViolationsBefore := TDptLintContext.Violations.Count;
  LLines := FContent.Split([sLineBreak]);
  LInCurlyComment := False;
  LInParenComment := False;

  for var I: Integer := 0 to High(LLines) do
  begin
    LLine := LLines[I];
    LCode := '';
    LInString := False;
    
    J := 1;
    while J <= Length(LLine) do
    begin
      C := LLine[J];
      if J < Length(LLine) then
        NextC := LLine[J+1]
      else
        NextC := #0;

      if LInCurlyComment then
      begin
        if C = '}' then LInCurlyComment := False;
        Inc(J);
        Continue;
      end;

      if LInParenComment then
      begin
        if (C = '*') and (NextC = ')') then
        begin
          LInParenComment := False;
          Inc(J, 2);
          Continue;
        end;
        Inc(J);
        Continue;
      end;

      if LInString then
      begin
        if C = '''' then
        begin
          if NextC = '''' then
            Inc(J, 2)
          else
          begin
            LInString := False;
            Inc(J);
            LCode := LCode + 'S';
          end;
        end
        else
          Inc(J);
        Continue;
      end;

      if C = '{' then
      begin
        LInCurlyComment := True;
        Inc(J);
        Continue;
      end;

      if (C = '(') and (NextC = '*') then
      begin
        LInParenComment := True;
        Inc(J, 2);
        Continue;
      end;

      if (C = '/') and (NextC = '/') then
        Break;

      if C = '''' then
      begin
        LInString := True;
        Inc(J);
        Continue;
      end;

      LCode := LCode + C;
      Inc(J);
    end;

    if FAssignmentRuleBefore <> srIgnore then
      ValidateOp(I, LCode, ':=', ':=', FAssignmentRuleBefore, FAssignmentRuleAfter);

    if (FArithmeticRuleBefore <> srIgnore) or (FArithmeticRuleAfter <> srIgnore) then
    begin
      var LOps: TArray<String> := FArithmeticOperators.Split([',']);
      for var LOp in LOps do
      begin
        var LTrimmedOp: String := LOp.Trim;
        if LTrimmedOp <> '' then
          ValidateOp(I, LCode, LTrimmedOp, LTrimmedOp, FArithmeticRuleBefore, FArithmeticRuleAfter);
      end;
    end;
  end;

  Result := TDptLintContext.Violations.Count = LViolationsBefore;
end;

{ TDptLintLocalVarFixture }

constructor TDptLintLocalVarFixture.Create;
begin
  inherited Create;
  FVariablesMustBeSorted := True;
  FAlignColons := True;
  FExactSpacesAfterColon := 1;
end;

procedure TDptLintLocalVarFixture.SetContent(const AValue: String);
begin
  FContent := AValue;
end;

procedure TDptLintLocalVarFixture.SetLineOffset(const AValue: String);
begin
  FLineOffset := StrToIntDef(AValue, 0);
end;

function TDptLintLocalVarFixture.ValidateVarBlock(AVarLines: TList<string>; AStartLineIdx: Integer): Boolean;
var
  LColonPos    : Integer;
  LMatch       : TMatch;
  LPrevColonPos: Integer;
  LPrevVarName : String;
  LVarName     : String;
begin
  Result := True;
  LPrevVarName := '';
  LPrevColonPos := -1;

  for var I: Integer := 0 to AVarLines.Count - 1 do
  begin
    var LLine: String := AVarLines[I];
    var LFullLineIdx: Integer := AStartLineIdx + I + 1 + FLineOffset;

    LColonPos := LLine.IndexOf(':');

    // Check for multiple variables per line (comma before colon)
    if (LColonPos > 0) and LLine.Substring(0, LColonPos).Contains(',') then
    begin
      ReportViolation(LFullLineIdx, 'Only one variable allowed per line.');
      Result := False;
    end;

    // Parse variable name and type
    LMatch := TRegEx.Match(LLine, '^\s*(\w+)\s*:\s*(.+);', [roIgnoreCase]);
    if not LMatch.Success then
      Continue;

    LVarName := LMatch.Groups[1].Value;
    // LColonPos already found above

    // Check Sorting
    if FVariablesMustBeSorted and
       (LPrevVarName <> '') and
       (CompareStringNatural(LVarName, LPrevVarName) < 0) then
    begin
      ReportViolation(LFullLineIdx, Format('Variables should be sorted alphabetically: "%s" before "%s".', [LVarName, LPrevVarName]));
      Result := False;
    end;

    // Check Colon Alignment
    if FAlignColons and
       (LPrevColonPos <> -1) and
       (LColonPos <> LPrevColonPos) then
    begin
      ReportViolation(LFullLineIdx, Format('Colons should be vertically aligned (Pos %d vs %d).', [LColonPos, LPrevColonPos]));
      Result := False;
    end;

    // Check Spaces After Colon
    if FExactSpacesAfterColon >= 0 then
    begin
      var LAfterColon: String := LLine.Substring(LColonPos + 1);
      var LSpacesCount: Integer := 0;
      while (LSpacesCount < LAfterColon.Length) and (LAfterColon[LSpacesCount + 1] = ' ') do
        Inc(LSpacesCount);

      if LSpacesCount <> FExactSpacesAfterColon then
      begin
        ReportViolation(LFullLineIdx, Format('Exactly %d space(s) required after colon (Found %d).', [FExactSpacesAfterColon, LSpacesCount]));
        Result := False;
      end;
    end;

    LPrevVarName := LVarName;
    LPrevColonPos := LColonPos;
  end;
end;

function TDptLintLocalVarFixture.LintLocalVars: Boolean;
var
  LCurrentVarLines : TList<string>;
  LInVarBlock      : Boolean;
  LLines           : TArray<string>;
  LVarBlockStartIdx: Integer;
begin
  Result := True;
  LLines := FContent.Split([sLineBreak]);
  LInVarBlock := False;
  LVarBlockStartIdx := 0;
  LCurrentVarLines := TList<string>.Create;
  try
    for var I: Integer := 0 to High(LLines) do
    begin
      var LLine: String := LLines[I].Trim;
      var LLower: String := LLine.ToLower;

      if not LInVarBlock then
      begin
        if LLower = 'var' then
        begin
          LInVarBlock := True;
          LVarBlockStartIdx := I + 1;
          LCurrentVarLines.Clear;
        end;
      end
      else
      begin
        if (LLower = 'begin') or
           (LLower = 'const') or
           (LLower = 'type') or
           (LLower = 'var') or
           LLower.StartsWith('procedure') or
           LLower.StartsWith('function') or
           LLower.StartsWith('constructor') or
           LLower.StartsWith('destructor') then
        begin
          if not ValidateVarBlock(LCurrentVarLines, LVarBlockStartIdx) then
            Result := False;

          if LLower = 'var' then
          begin
            LVarBlockStartIdx := I + 1;
            LCurrentVarLines.Clear;
          end
          else
            LInVarBlock := False;
        end
        else if LLine <> '' then
          LCurrentVarLines.Add(LLines[I]);
      end;
    end;
    
    if LInVarBlock and
       not ValidateVarBlock(LCurrentVarLines, LVarBlockStartIdx) then
      Result := False;
  finally
    LCurrentVarLines.Free;
  end;
end;

initialization

RegisterSlimFixture(TDptLintUnitContextFixture);
RegisterSlimFixture(TDptLintFixture);
RegisterSlimFixture(TDptLintRegexFixture);
RegisterSlimFixture(TDptLintUsesFixture);
RegisterSlimFixture(TDptLintClassDeclarationFixture);
RegisterSlimFixture(TDptLintImplementationFixture);
RegisterSlimFixture(TDptLintMethodBodyFixture);
RegisterSlimFixture(TDptLintLocalVarFixture);

end.
