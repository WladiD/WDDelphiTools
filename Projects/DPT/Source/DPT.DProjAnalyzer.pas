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
    procedure ParseProperties(const ABody: String; const ADefinitions: IDictionary_String_String);
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

  System.Classes,
  System.SysUtils,
  System.RegularExpressions,
  System.IOUtils,

  System.Collections.Factory;

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

function TDProjAnalyzer.EvaluateCondition(const ACondition: String; const ADefinitions: IDictionary_String_String): Boolean;
var
  EqPos     : Integer;
  Left      : String;
  NeqPos    : Integer;
  Part      : String;
  Parts     : TArray<String>;
  Resolved  : String;
  ResultBool: Boolean;
  Right     : String;
  VarMatch  : TMatch;
  VarMatches: TMatchCollection;
  VarName   : String;
  VarValue  : String;
begin
  if Trim(ACondition) = '' then
    Exit(True);

  Resolved := ACondition;

  // 1. Resolve variables $(Var) by finding them and looking up in Definitions
  VarMatches := TRegEx.Matches(Resolved, '\$\((\w+)\)');
  for VarMatch in VarMatches do
  begin
    VarName := VarMatch.Groups[1].Value; // e.g. 'Config'
    if ADefinitions.TryGetValue(LowerCase(VarName), VarValue) then
      Resolved := StringReplace(Resolved, VarMatch.Value, VarValue, [rfReplaceAll, rfIgnoreCase])
    else
      Resolved := StringReplace(Resolved, VarMatch.Value, '', [rfReplaceAll, rfIgnoreCase]);
  end;

  // 2. Split by ' or ' (simple OR support)
  Parts := Resolved.Split([' or ', ' OR '], TStringSplitOptions.ExcludeEmpty);
  if Length(Parts) = 0 then
     Parts := [Resolved];

  Result := False;
  for Part in Parts do
  begin
    // Evaluate single expression
    // Handle 'A'=='B'
    EqPos := Pos('==', Part);
    NeqPos := Pos('!=', Part);

    ResultBool := False;
    if EqPos > 0 then
    begin
      Left := Trim(Copy(Part, 1, EqPos - 1));
      Right := Trim(Copy(Part, EqPos + 2, Length(Part)));
      // Remove quotes
      Left := StringReplace(Left, '''', '', [rfReplaceAll]);
      Right := StringReplace(Right, '''', '', [rfReplaceAll]);
      if SameText(Left, Right) then ResultBool := True;
    end
    else if NeqPos > 0 then
    begin
      Left := Trim(Copy(Part, 1, NeqPos - 1));
      Right := Trim(Copy(Part, NeqPos + 2, Length(Part)));
      // Remove quotes
      Left := StringReplace(Left, '''', '', [rfReplaceAll]);
      Right := StringReplace(Right, '''', '', [rfReplaceAll]);
      if not SameText(Left, Right) then ResultBool := True;
    end
    else
    begin
      // Fallback: If part is just "true" or "false" (after var replacement)
      if SameText(Trim(Part), 'true') then ResultBool := True;
    end;

    if ResultBool then
    begin
      Result := True;
      Break; // Short-circuit OR
    end;
  end;
end;

procedure TDProjAnalyzer.ParseProperties(const ABody: String; const ADefinitions: IDictionary_String_String);
var
  Key        : String;
  PropMatch  : TMatch;
  PropMatches: TMatchCollection;
  Value      : String;
begin
  // Regex to find <Key>Value</Key>
  PropMatches := TRegEx.Matches(ABody, '<(\w+)>([^<]+)</\1>');
  for PropMatch in PropMatches do
  begin
    Key := PropMatch.Groups[1].Value;
    Value := PropMatch.Groups[2].Value;
    ADefinitions[LowerCase(Key)] := Value;
  end;
end;

function TDProjAnalyzer.GetProjectOutputFile(const AConfig, APlatform: String): String;
var
  BaseName    : String;
  Body        : String;
  Definitions : IDictionary_String_String;
  ExeOutput   : String;
  GroupMatch  : TMatch;
  GroupMatches: TMatchCollection;
  PossiblePath: String;
  RawCondition: String;
  RootPath    : String;
  ValMatch    : TMatch;
begin
  BaseName := ChangeFileExt(ExtractFileName(FProjectFile), '.exe');
  ExeOutput := '';
  Definitions := TCollections.CreateDictionary_String_String;

  Definitions['config'] := AConfig;
  Definitions['platform'] := APlatform;
  Definitions['base'] := ''; // Initialize Base as empty/undefined initially

  // Find all PropertyGroups sequentially
  GroupMatches := TRegEx.Matches(FContent, '<PropertyGroup(.*?)>([\s\S]*?)</PropertyGroup>', [roIgnoreCase]);

  for GroupMatch in GroupMatches do
  begin
    // Extract Condition from attributes
    RawCondition := '';
    ValMatch := TRegEx.Match(GroupMatch.Groups[1].Value, 'Condition="([^"]+)"', [roIgnoreCase]);
    if ValMatch.Success then
      RawCondition := ValMatch.Groups[1].Value;

    Body := GroupMatch.Groups[2].Value;

    if EvaluateCondition(RawCondition, Definitions) then
    begin
      // Parse properties to update Definitions (like Cfg_1, Base, etc.)
      ParseProperties(Body, Definitions);

      // Check for DCC_ExeOutput in this active block
      ValMatch := TRegEx.Match(Body, '<DCC_ExeOutput>(.*?)</DCC_ExeOutput>', [roIgnoreCase]);
      if ValMatch.Success then
      begin
        ExeOutput := ValMatch.Groups[1].Value;
      end;
    end;
  end;

  if ExeOutput <> '' then
  begin
    // Replace variables
    ExeOutput := ExeOutput.Replace('$(Platform)', APlatform, [rfReplaceAll, rfIgnoreCase]);
    ExeOutput := ExeOutput.Replace('$(Config)', AConfig, [rfReplaceAll, rfIgnoreCase]);
    
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

function TDProjAnalyzer.GetProjectSearchPath(const AConfig, APlatform: String): String;
var
  Body        : String;
  CombinedPath: String;
  Condition   : String;
  CondMatch   : TMatch;
  GroupMatch  : TMatch;
  GroupMatches: TMatchCollection;
  RawPath     : String;
  SkipGroup   : Boolean;
  ValMatch    : TMatch;
begin
  CombinedPath := '';
  
  // Scan all PropertyGroups to build up the search path.
  // Note: Values in later PropertyGroups can override or append to earlier ones.
  // Typically, paths are cumulative if they use $(DCC_UnitSearchPath), but often they are just set.
  // However, in .dproj, usually the most specific one wins or it appends. 
  // We will look for the most specific matching group.
  
  GroupMatches := TRegEx.Matches(FContent, '<PropertyGroup(.*?)>([\s\S]*?)</PropertyGroup>', [roIgnoreCase]);
  for GroupMatch in GroupMatches do
  begin
    Condition := GroupMatch.Groups[1].Value;
    Body := GroupMatch.Groups[2].Value;
    
    if not Body.Contains('DCC_UnitSearchPath') then
      Continue;
      
    SkipGroup := False;
    
    // Check Config mismatch
    for CondMatch in TRegEx.Matches(Condition, '''\$\(Config\)''==''([^'']*)''', [roIgnoreCase]) do
    begin
      if not SameText(CondMatch.Groups[1].Value, AConfig) then
      begin
        SkipGroup := True;
        Break;
      end;
    end;

    if SkipGroup then
      Continue;

    // Check Platform mismatch
    for CondMatch in TRegEx.Matches(Condition, '''\$\(Platform\)''==''([^'']*)''', [roIgnoreCase]) do
    begin
      if not SameText(CondMatch.Groups[1].Value, APlatform) then
      begin
        SkipGroup := True;
        Break;
      end;
    end;

    if SkipGroup then
      Continue;

    // Extract DCC_UnitSearchPath
    ValMatch := TRegEx.Match(Body, '<DCC_UnitSearchPath>([\s\S]*?)</DCC_UnitSearchPath>', [roIgnoreCase]);
    if ValMatch.Success then
    begin
      RawPath := ValMatch.Groups[1].Value;
      
      // MSBuild/Delphi Property logic:
      // If the new value contains $(DCC_UnitSearchPath), it appends/prepends to the existing value.
      // If it does NOT contain it, it overwrites the existing value entirely.
      
      if RawPath.Contains('$(DCC_UnitSearchPath)') then
      begin
        CombinedPath := StringReplace(RawPath, '$(DCC_UnitSearchPath)', CombinedPath, [rfReplaceAll, rfIgnoreCase]);
      end
      else
      begin
        CombinedPath := RawPath;
      end;
    end;
  end;
  
  // Cleanup any remaining $(DCC_UnitSearchPath) which might be left if the first definition used it (resolves to empty)
  CombinedPath := StringReplace(CombinedPath, '$(DCC_UnitSearchPath)', '', [rfReplaceAll, rfIgnoreCase]);
  
  // Cleanup double semicolons resulting from empty replacements
  CombinedPath := StringReplace(CombinedPath, ';;', ';', [rfReplaceAll]);
  if (CombinedPath <> '') and (CombinedPath[1] = ';') then
    Delete(CombinedPath, 1, 1);
  if (CombinedPath <> '') and (CombinedPath[Length(CombinedPath)] = ';') then
    Delete(CombinedPath, Length(CombinedPath), 1);
  
  Result := CombinedPath;
  
  // Resolve basic variables
  Result := StringReplace(Result, '$(Config)', AConfig, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(Platform)', APlatform, [rfReplaceAll, rfIgnoreCase]);
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
