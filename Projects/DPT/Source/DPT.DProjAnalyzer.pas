// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.DProjAnalyzer;

interface

uses

  System.Collections.Interfaces;

type

  TDProjAnalyzer = class
  private
    FContent    : String;
    FProjectFile: String;
    function  EvaluateCondition(const ACondition: String; const ADefinitions: IDictionary_String_String): Boolean;
    procedure LoadContent;
    function  EvaluateProperties(const AConfig, APlatform: String): IDictionary_String_String;
    procedure ParseProperties(const ABody: String; const ADefinitions: IDictionary_String_String);
    function  ResolveVariables(const AValue: String; const ADefinitions: IDictionary_String_String): String;
  public
    class function ResolvePath(const APath, ABaseDir: String): String;
  public
    constructor Create(const AProjectFile: String);
    function GetConfigs: TArray<String>;
    function GetDefaultConfig: String;
    function GetProjectFiles: TArray<String>;
    function GetProjectOutputFile(const AConfig, APlatform: String): String;
    function GetProjectSearchPath(const AConfig, APlatform: String): String;
  end;

implementation

uses

  System.Character,
  System.Classes,
  System.SysUtils,
  System.RegularExpressions,
  System.IOUtils,

  System.Collections.Factory;

type

  EMsBuildCondition = class(Exception);

  TMsBuildConditionParser = class
  private
    FBaseDir    : String;
    FDefinitions: IDictionary_String_String;
    FPos        : Integer;
    FText       : String;
    function  AtEnd: Boolean;
    function  ExpandVariables(const AValue: String): String;
    function  IsIdentChar(AChar: Char): Boolean;
    function  ParseAnd: Boolean;
    function  ParseComparison: Boolean;
    function  ParseNot: Boolean;
    function  ParseOr: Boolean;
    function  ParsePrimary: Boolean;
    function  ParseTerm: String;
    function  Peek(AOffset: Integer = 0): Char;
    procedure SkipWhitespace;
    function  TryConsumeKeyword(const AKeyword: String): Boolean;
    function  TryConsumeSymbol(const ASymbol: String): Boolean;
  public
    constructor Create(const ADefinitions: IDictionary_String_String; const ABaseDir: String);
    function Evaluate(const ACondition: String): Boolean;
  end;

{ TMsBuildConditionParser }

constructor TMsBuildConditionParser.Create(const ADefinitions: IDictionary_String_String; const ABaseDir: String);
begin
  FDefinitions := ADefinitions;
  FBaseDir := ABaseDir;
end;

/// <summary>
/// Evaluates a full MSBuild condition. Grammar (lowest precedence first):
///   Or         := And ( 'or' And )*
///   And        := Not ( 'and' Not )*
///   Not        := '!' Not | Primary
///   Primary    := '(' Or ')' | 'Exists' '(' Term ')' | Comparison
///   Comparison := Term ( ( '==' | '!=' ) Term )?
///   Term       := '...' | bare word
/// Malformed input evaluates to False instead of raising.
/// </summary>
function TMsBuildConditionParser.Evaluate(const ACondition: String): Boolean;
begin
  FText := ACondition;
  FPos := 1;
  try
    Result := ParseOr;
    SkipWhitespace;
    if not AtEnd then
      Result := False;
  except
    on EMsBuildCondition do
      Result := False;
  end;
end;

function TMsBuildConditionParser.AtEnd: Boolean;
begin
  Result := FPos > Length(FText);
end;

function TMsBuildConditionParser.Peek(AOffset: Integer): Char;
begin
  if FPos + AOffset <= Length(FText) then
    Result := FText[FPos + AOffset]
  else
    Result := #0;
end;

function TMsBuildConditionParser.IsIdentChar(AChar: Char): Boolean;
begin
  Result := AChar.IsLetterOrDigit or (AChar = '_');
end;

procedure TMsBuildConditionParser.SkipWhitespace;
begin
  while not AtEnd and FText[FPos].IsWhiteSpace do
    Inc(FPos);
end;

function TMsBuildConditionParser.TryConsumeSymbol(const ASymbol: String): Boolean;
begin
  SkipWhitespace;
  Result := SameText(Copy(FText, FPos, Length(ASymbol)), ASymbol);
  if Result then
    Inc(FPos, Length(ASymbol));
end;

/// <summary>
/// Consumes a case-insensitive keyword (and/or/Exists) only when it is a
/// whole word, so a bare term such as "order" is not mistaken for "or".
/// </summary>
function TMsBuildConditionParser.TryConsumeKeyword(const AKeyword: String): Boolean;
begin
  SkipWhitespace;
  Result := SameText(Copy(FText, FPos, Length(AKeyword)), AKeyword) and
    not IsIdentChar(Peek(Length(AKeyword)));
  if Result then
    Inc(FPos, Length(AKeyword));
end;

function TMsBuildConditionParser.ExpandVariables(const AValue: String): String;
var
  VarMatch: TMatch;
  VarValue: String;
begin
  Result := AValue;
  for VarMatch in TRegEx.Matches(AValue, '\$\((\w+)\)') do
  begin
    if not FDefinitions.TryGetValue(LowerCase(VarMatch.Groups[1].Value), VarValue) then
      VarValue := '';
    Result := StringReplace(Result, VarMatch.Value, VarValue, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TMsBuildConditionParser.ParseOr: Boolean;
var
  Operand: Boolean;
begin
  Result := ParseAnd;
  while TryConsumeKeyword('or') do
  begin
    Operand := ParseAnd; // Always consume the operand - no short-circuit
    Result := Result or Operand;
  end;
end;

function TMsBuildConditionParser.ParseAnd: Boolean;
var
  Operand: Boolean;
begin
  Result := ParseNot;
  while TryConsumeKeyword('and') do
  begin
    Operand := ParseNot;
    Result := Result and Operand;
  end;
end;

function TMsBuildConditionParser.ParseNot: Boolean;
begin
  SkipWhitespace;
  if (Peek = '!') and (Peek(1) <> '=') then
  begin
    Inc(FPos);
    Result := not ParseNot;
  end
  else
    Result := ParsePrimary;
end;

function TMsBuildConditionParser.ParsePrimary: Boolean;
var
  Path: String;
begin
  SkipWhitespace;
  if Peek = '(' then
  begin
    Inc(FPos);
    Result := ParseOr;
    if not TryConsumeSymbol(')') then
      raise EMsBuildCondition.Create('Missing )');
    Exit;
  end;

  if TryConsumeKeyword('Exists') then
  begin
    if not TryConsumeSymbol('(') then
      raise EMsBuildCondition.Create('Missing ( after Exists');
    Path := ParseTerm;
    if not TryConsumeSymbol(')') then
      raise EMsBuildCondition.Create('Missing ) after Exists');
    if not TPath.IsPathRooted(Path) then
      Path := TPath.Combine(FBaseDir, Path);
    Exit(FileExists(Path) or DirectoryExists(Path));
  end;

  Result := ParseComparison;
end;

function TMsBuildConditionParser.ParseComparison: Boolean;
var
  Left : String;
  Right: String;
begin
  Left := ParseTerm;
  if TryConsumeSymbol('==') then
  begin
    Right := ParseTerm;
    Result := SameText(Left, Right);
  end
  else if TryConsumeSymbol('!=') then
  begin
    Right := ParseTerm;
    Result := not SameText(Left, Right);
  end
  else
    Result := SameText(Left, 'true');
end;

/// <summary>
/// Reads either a single-quoted string or a bare word; $(Var) references are
/// expanded from the collected definitions, unknown variables become ''.
/// </summary>
function TMsBuildConditionParser.ParseTerm: String;
var
  Start: Integer;
begin
  SkipWhitespace;
  if Peek = '''' then
  begin
    Inc(FPos);
    Start := FPos;
    while not AtEnd and (FText[FPos] <> '''') do
      Inc(FPos);
    if AtEnd then
      raise EMsBuildCondition.Create('Unterminated string');
    Result := Copy(FText, Start, FPos - Start);
    Inc(FPos); // closing quote
  end
  else
  begin
    Start := FPos;
    while not AtEnd do
    begin
      if (Peek = '$') and (Peek(1) = '(') then
      begin
        while not AtEnd and (FText[FPos] <> ')') do
          Inc(FPos);
        if AtEnd then
          raise EMsBuildCondition.Create('Unterminated $(');
        Inc(FPos);
      end
      else if FText[FPos].IsWhiteSpace or CharInSet(FText[FPos], ['(', ')', '=', '!', '''']) then
        Break
      else
        Inc(FPos);
    end;
    if FPos = Start then
      raise EMsBuildCondition.CreateFmt('Unexpected "%s" at %d', [Peek, FPos]);
    Result := Copy(FText, Start, FPos - Start);
  end;
  Result := ExpandVariables(Result);
end;

{ TDProjAnalyzer }

constructor TDProjAnalyzer.Create(const AProjectFile: String);
begin
  FProjectFile := AProjectFile;
  LoadContent;
end;

procedure TDProjAnalyzer.LoadContent;
begin
  if not FileExists(FProjectFile) then
    raise Exception.CreateFmt('Project file not found: %s', [FProjectFile]);
    
  FContent := TFile.ReadAllText(FProjectFile);
end;

/// <summary>
/// Evaluates an MSBuild PropertyGroup/property condition against the
/// properties collected so far. Supports everything the Delphi IDE writes
/// into a .dproj: 'A'=='B', 'A'!='B', and/or, parentheses, unary !, bare
/// true/false and Exists('path'). A malformed condition evaluates to False.
/// </summary>
function TDProjAnalyzer.EvaluateCondition(const ACondition: String; const ADefinitions: IDictionary_String_String): Boolean;
var
  Parser: TMsBuildConditionParser;
begin
  if Trim(ACondition) = '' then
    Exit(True);

  Parser := TMsBuildConditionParser.Create(ADefinitions, ExtractFilePath(FProjectFile));
  try
    Result := Parser.Evaluate(ACondition);
  finally
    Parser.Free;
  end;
end;

/// <summary>
/// Applies all properties of one PropertyGroup body whose element-level
/// condition holds. Like MSBuild, $(Var) references in the value are
/// expanded at assignment time, so the IDE's append idiom
/// <c>..\pas;$(DCC_UnitSearchPath)</c> accumulates across inherited groups.
/// Variables that are neither defined nor in the environment stay literal
/// for later macro resolution (e.g. $(BDS), $(ProductVersion)).
/// </summary>
procedure TDProjAnalyzer.ParseProperties(const ABody: String; const ADefinitions: IDictionary_String_String);
var
  CondAttrMatch: TMatch;
  Condition    : String;
  Key          : String;
  PropMatch    : TMatch;
  PropMatches  : TMatchCollection;
  Value        : String;
begin
  // Match <Key>Value</Key> as well as <Key Attr1="..." Attr2="...">Value</Key>.
  // Group 1: tag name, Group 2: attributes (optional), Group 3: value.
  PropMatches := TRegEx.Matches(ABody, '<(\w+)((?:\s+\w+="[^"]*")*)\s*>([^<]+)</\1>');
  for PropMatch in PropMatches do
  begin
    Key := PropMatch.Groups[1].Value;
    Value := PropMatch.Groups[3].Value;

    Condition := '';
    CondAttrMatch := TRegEx.Match(PropMatch.Groups[2].Value, 'Condition="([^"]+)"', [roIgnoreCase]);
    if CondAttrMatch.Success then
      Condition := CondAttrMatch.Groups[1].Value;

    if (Condition = '') or EvaluateCondition(Condition, ADefinitions) then
      ADefinitions[LowerCase(Key)] := ResolveVariables(Value, ADefinitions);
  end;
end;

function TDProjAnalyzer.ResolveVariables(const AValue: String; const ADefinitions: IDictionary_String_String): String;
var
  EnvValue  : String;
  VarMatch  : TMatch;
  VarMatches: TMatchCollection;
  VarName   : String;
  VarValue  : String;
begin
  Result := AValue;
  VarMatches := TRegEx.Matches(Result, '\$\((\w+)\)');
  for VarMatch in VarMatches do
  begin
    VarName := VarMatch.Groups[1].Value;
    if ADefinitions.TryGetValue(LowerCase(VarName), VarValue) and (VarValue <> '') then
      Result := StringReplace(Result, VarMatch.Value, VarValue, [rfReplaceAll, rfIgnoreCase])
    else
    begin
      EnvValue := GetEnvironmentVariable(VarName);
      if EnvValue <> '' then
        Result := StringReplace(Result, VarMatch.Value, EnvValue, [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
end;

/// <summary>
/// Evaluates all PropertyGroups of the project for the given configuration
/// and platform the way MSBuild does: a single pass in file order, applying
/// each group whose condition holds. The IDE models configuration
/// inheritance (Base < Base_<Platform> < Cfg_N < Cfg_N_<Platform>) through
/// marker properties (Base, Base_Win64, Cfg_2, ...) that it seeds and
/// switches on in exactly this order, so the last assignment wins.
/// Keys are lower-cased property names.
/// </summary>
function TDProjAnalyzer.EvaluateProperties(const AConfig, APlatform: String): IDictionary_String_String;
var
  GroupMatch  : TMatch;
  RawCondition: String;
  ValMatch    : TMatch;
begin
  Result := TCollections.CreateDictionary_String_String;
  Result['config'] := AConfig;
  Result['platform'] := APlatform;

  for GroupMatch in TRegEx.Matches(FContent, '<PropertyGroup(.*?)>([\s\S]*?)</PropertyGroup>', [roIgnoreCase]) do
  begin
    RawCondition := '';
    ValMatch := TRegEx.Match(GroupMatch.Groups[1].Value, 'Condition="([^"]+)"', [roIgnoreCase]);
    if ValMatch.Success then
      RawCondition := ValMatch.Groups[1].Value;

    if EvaluateCondition(RawCondition, Result) then
      ParseProperties(GroupMatch.Groups[2].Value, Result);
  end;
end;

function TDProjAnalyzer.GetProjectOutputFile(const AConfig, APlatform: String): String;
var
  BaseName    : String;
  Definitions : IDictionary_String_String;
  ExeOutput   : String;
  PossiblePath: String;
  RootPath    : String;
begin
  BaseName := ChangeFileExt(ExtractFileName(FProjectFile), '.exe');
  Definitions := EvaluateProperties(AConfig, APlatform);

  if not Definitions.TryGetValue('dcc_exeoutput', ExeOutput) then
    ExeOutput := '';

  if ExeOutput <> '' then
  begin
    ExeOutput := ResolveVariables(ExeOutput, Definitions);

    // Construct path
    if TPath.IsPathRooted(ExeOutput) then
      PossiblePath := TPath.Combine(ExeOutput, BaseName)
    else
      PossiblePath := ExpandFileName(
        IncludeTrailingPathDelimiter(ExtractFilePath(FProjectFile)) +
        IncludeTrailingPathDelimiter(ExeOutput) +
        BaseName);

    Exit(PossiblePath);
  end;

  RootPath := ExpandFileName(IncludeTrailingPathDelimiter(ExtractFilePath(FProjectFile)) + BaseName);
  Result := RootPath;
end;

function TDProjAnalyzer.GetProjectFiles: TArray<String>;
var
  Matches: TMatchCollection;
  Match  : TMatch;
  List   : IList_String;
  ProjDir: string;
begin
  List := TCollections.CreateList_String;
  ProjDir := ExtractFilePath(FProjectFile);

  // Match <DCCReference Include="DPT.Build.Task.pas"/> or <DCCReference Include="DPT.Build.Task.pas">
  Matches := TRegEx.Matches(FContent, '<DCCReference\s+Include="([^"]+)"', [roIgnoreCase]);
  for Match in Matches do
  begin
    List.Add(TPath.GetFullPath(TPath.Combine(ProjDir, Match.Groups[1].Value)));
  end;

  Result := List.ToArray;
end;

function TDProjAnalyzer.GetConfigs: TArray<String>;
var
  Matches: TMatchCollection;
  Match  : TMatch;
  List   : IList_String;
begin
  List := TCollections.CreateList_String;

  // Match <BuildConfiguration Include="Debug">
  Matches := TRegEx.Matches(FContent, '<BuildConfiguration Include="([^"]+)">', [roIgnoreCase]);
  for Match in Matches do
  begin
    List.Add(Match.Groups[1].Value);
  end;

  // Fallback: If no explicit BuildConfiguration items, look for PropertyGroups with Config conditions
  if List.Count = 0 then
  begin
    Matches := TRegEx.Matches(FContent, '''\$\(Config\)''==''([^'']*)''', [roIgnoreCase]);
    for Match in Matches do
    begin
      if not List.Contains(Match.Groups[1].Value) then
        List.Add(Match.Groups[1].Value);
    end;
  end;

  List.Sort;
  Result := List.ToArray;
end;

function TDProjAnalyzer.GetDefaultConfig: String;
var
  Match: TMatch;
begin
  // Look for <Config Condition="'$(Config)'==''">Debug</Config>
  // Or simply <Config>Debug</Config> inside a PropertyGroup without conditions (less common for Config itself)
  
  // Regex to find the default config assignment
  Match := TRegEx.Match(FContent, '<Config\s+Condition="''\$\(Config\)''==''''">([^<]+)</Config>', [roIgnoreCase]);
  if Match.Success then
    Result := Match.Groups[1].Value
  else
    Result := 'Debug'; // Reasonable default if not found
end;

/// <summary>
/// Returns the effective DCC_UnitSearchPath for the configuration/platform,
/// evaluated with the same condition and inheritance semantics as the output
/// file. IDE macros that the project itself does not define ($(BDS),
/// $(ProductVersion), ...) are left literal for TEnvOptions.ResolveMacros.
/// </summary>
function TDProjAnalyzer.GetProjectSearchPath(const AConfig, APlatform: String): String;
var
  Definitions: IDictionary_String_String;
begin
  Definitions := EvaluateProperties(AConfig, APlatform);
  if not Definitions.TryGetValue('dcc_unitsearchpath', Result) then
    Result := '';

  // The first group using the append idiom references a still undefined
  // $(DCC_UnitSearchPath), which stays literal - resolve it to empty.
  Result := StringReplace(Result, '$(DCC_UnitSearchPath)', '', [rfReplaceAll, rfIgnoreCase]);

  // Cleanup empty entries resulting from the replacement above.
  while Result.Contains(';;') do
    Result := StringReplace(Result, ';;', ';', [rfReplaceAll]);
  if (Result <> '') and (Result[1] = ';') then
    Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = ';') then
    Delete(Result, Length(Result), 1);
end;

class function TDProjAnalyzer.ResolvePath(const APath, ABaseDir: String): String;
var
  P       : String;
  Parts   : TArray<String>;
  Resolved: String;
begin
  // Helper to resolve relative paths against BaseDir
  // Only resolves if path is relative
  Result := '';
  Parts := APath.Split([';']);
  for P in Parts do
  begin
    if P.Trim = '' then Continue;
    
    Resolved := P;
    if (Pos('$(', P) = 0) and (not TPath.IsPathRooted(P)) then
    begin
      try
        Resolved := TPath.GetFullPath(TPath.Combine(ABaseDir, P));
      except
        // Ignore invalid paths
      end;
    end;
    
    if Result = '' then
      Result := Resolved
    else
      Result := Result + ';' + Resolved;
  end;
end;

end.
