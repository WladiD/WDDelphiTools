// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Preprocessor;

interface

uses

  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  System.SysUtils,
  System.Types,

  System.Collections.Factory,
  System.Collections.Interfaces,

  TmplCodeGen.Common,
  TmplCodeGen.Generator,
  TmplCodeGen.Includer,
  TmplCodeGen.Logger;

type

  TDptPreprocessor = class
  private
    FLogger: ILogger;
    procedure HandleIncludePartials(const AInputFile, AParam: String);
    function  HandleTmplCodeGen(const AInputFile, APrefix: String; const AConfigBlocks: IDictionary_String_String): String;
  public
    constructor Create(const ALogger: ILogger = nil);
    function Execute(const AInputFile: String): String;
  end;

implementation

{ TDptPreprocessor }

constructor TDptPreprocessor.Create(const ALogger: ILogger = nil);
begin
  if Assigned(ALogger) then
    FLogger := ALogger
  else
    FLogger := TConsoleLogger.Create;
end;

procedure TDptPreprocessor.HandleIncludePartials(const AInputFile, AParam: String);
var
  IncludeParts: TIncludePartials;
  Parts       : TArray<String>;
  Target      : String;
begin
  Parts := AParam.Split([' '], TStringSplitOptions.ExcludeEmpty);
  if Length(Parts) > 1 then
    Target := Parts[1]
  else
    Target := AInputFile;

  if (Target <> '') and (not TPath.IsPathRooted(Target)) then
    Target := TPath.Combine(ExtractFilePath(AInputFile), Target);

  IncludeParts := TIncludePartials.Create(Target, FLogger);
  try
    IncludeParts.WriteOnlyIfChanged := True;
    IncludeParts.Execute;
  finally
    IncludeParts.Free;
  end;
end;

function TDptPreprocessor.HandleTmplCodeGen(const AInputFile, APrefix: String; const AConfigBlocks: IDictionary_String_String): String;
var
  AbsPrefix   : String;
  ConfContent : String;
  ConfFileName: String;
  Key         : String;
  TmplCodeGen : TTmplCodeGen;
begin
  ConfFileName := APrefix + '-conf.json';

  if TPath.IsPathRooted(APrefix) then
    AbsPrefix := APrefix
  else
    AbsPrefix := TPath.Combine(ExtractFilePath(AInputFile), APrefix);

  TmplCodeGen := TTmplCodeGen.Create(AbsPrefix, FLogger);
  try
    TmplCodeGen.WriteOnlyIfChanged := True;
    Key := ExtractFileName(ConfFileName);
    if AConfigBlocks.TryGetValue(Key, ConfContent) then
      TmplCodeGen.SetConfig(ConfContent);

    TmplCodeGen.ProcessTemplate;
    Result := TmplCodeGen.OutputFileName;
  finally
    TmplCodeGen.Free;
  end;
end;

/// <summary>
/// Scans the input file for embedded configuration blocks (* Name-conf.json ... *)
/// and "// TmplCodeGen" instructions, executing them in the order they appear.
/// Returns the path to the last generated project file (facilitating parameter transition),
/// or the input file if no project was generated.
/// </summary>
function TDptPreprocessor.Execute(const AInputFile: String): String;
var
  BlockRegEx  : TRegEx;
  ConfigBlocks: IDictionary_String_String;
  ConfigName  : String;
  Content     : String;
  Line        : String;
  Lines       : TStringDynArray;
  Match       : TMatch;
  Param       : String;
  RegEx       : TRegEx;
begin
  Result := AInputFile;
  if not FileExists(AInputFile) then
    Exit;

  ConfigBlocks := TCollections.CreateDictionary_String_String;
  Content := TFile.ReadAllText(AInputFile, TEncoding.UTF8);

  // Extract (* Name-conf.json ... *) blocks
  BlockRegEx := TRegEx.Create('\(\*\s*(?P<Name>[\w\-\.]+-conf\.json)\s+(?P<Content>[\s\S]*?)\*\)', [roMultiLine]);
  var Matches := BlockRegEx.Matches(Content);
  for Match in Matches do
  begin
    ConfigName := Match.Groups['Name'].Value;
    ConfigBlocks[ConfigName] := Match.Groups['Content'].Value;
  end;

  // Regex to match: // TmplCodeGen [Param]
  RegEx := TRegEx.Create('^\s*//\s*TmplCodeGen\s+(?P<Param>.+?)\s*$', [roIgnoreCase, roMultiLine]);
  
  Lines := TFile.ReadAllLines(AInputFile);

  for Line in Lines do
  begin
    Match := RegEx.Match(Line);
    if Match.Success then
    begin
      Param := Match.Groups['Param'].Value;
      if Param.StartsWith('include_partials', True) then
        HandleIncludePartials(AInputFile, Param)
      else
        Result := HandleTmplCodeGen(AInputFile, Param, ConfigBlocks);
    end;
  end;
end;

end.
