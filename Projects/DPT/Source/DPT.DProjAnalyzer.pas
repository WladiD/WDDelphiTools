// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.DProjAnalyzer;

interface

uses

  System.Classes,
  System.SysUtils,
  System.RegularExpressions,
  System.IOUtils,

  System.Collections.Factory,
  System.Collections.Interfaces;

type

  TDProjAnalyzer = class
  private
    FContent    : String;
    FProjectFile: String;
    procedure LoadContent;
  public
    class function ResolvePath(const APath, ABaseDir: String): String;
  public
    constructor Create(const AProjectFile: String);
    function GetConfigs: TArray<String>;
    function GetDefaultConfig: String;
    function GetProjectSearchPath(const AConfig, APlatform: String): String;
  end;

implementation

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
