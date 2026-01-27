// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit TmplCodeGen.Generator;

interface

uses

  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  System.SysUtils,

  mormot.core.base,
  mormot.core.json,
  mormot.core.mustache,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.variants,

  TmplCodeGen.Common,
  TmplCodeGen.Logger,
  TmplCodeGen.PreProcess,
  TmplCodeGen.Utils;

type

  TTmplCodeGen = class
  private
    FConfJson             : TDocVariantData;
    FConfJsonFileName     : String;
    FEmptyParenthesesRegEx: TRegEx;
    FLogger               : ILogger;
    FMainTemplate         : String;
    FMainTemplateFileName : String;
    FOutputFileName       : String;
    FParamsCallRegEx      : TRegEx;
    FParamsCallSubRegEx   : TRegEx;
    FParamsDefineRegEx    : TRegEx;
    FParamsDefineSubRegEx : TRegEx;
    FPrefix               : String;
    FWriteOnlyIfChanged   : Boolean;
    procedure ExportPartials(const AOutputContent: String);
    function  PostFix(const AOutputContent: String): String;
    function  PostFixParamsCall(const ALine: String): String;
    function  PostFixParamsDefine(const ALine: String): String;
    function  PostFixRoute(const ACommand: String; const ALine: String): String;
    procedure PrefixHelper(const Value: Variant; out Result: Variant);
    procedure PreProcessConfJson;
  public
    constructor Create(const APrefix: String; const ALogger: ILogger);
    procedure ProcessTemplate;
    procedure SetConfig(const AJsonContent: String);
    property OutputFileName: String read FOutputFileName;
    property WriteOnlyIfChanged: Boolean read FWriteOnlyIfChanged write FWriteOnlyIfChanged;
  end;

implementation

{ TTmplCodeGen }

constructor TTmplCodeGen.Create(const APrefix: String; const ALogger: ILogger);
begin
  FPrefix := APrefix;
  FLogger := ALogger;

  FParamsCallRegEx := TRegEx.Create('([\w\.])+(?P<Parentheses>\([^\)]*\))', [roCompiled]);
  FParamsCallSubRegEx := TRegEx.Create('\((?P<Params>[^\)]+),\s*\)', [roCompiled]);
  FParamsDefineRegEx := TRegEx.Create('(procedure|function|constructor)\s*([\w\.])*(?P<Parentheses>\([^\)]*\))', [roIgnoreCase, roCompiled]);
  FParamsDefineSubRegEx := TRegEx.Create('\((?P<Params>[^\)]+);\s*\)', [roCompiled]);
  FEmptyParenthesesRegEx := TRegEx.Create('\(\s*\)', [roCompiled]);
end;

procedure TTmplCodeGen.SetConfig(const AJsonContent: String);
begin
  if not FConfJson.InitJson(UTF8String(AJsonContent)) then
    raise Exception.Create('Provided JSON content is not valid');
end;

/// <remarks>
/// The PostFix system: In the template, desired lines can be defined by a
/// Pascal line comment (at the end of the line) in the form:
/// <code>
/// MyPascalCode; // PostFixMyCommand
/// </code>
/// This allows for specific post-processing operations that are not (or only with difficulty)
/// possible in the context of the Mustache template engine.
/// PostFix processing takes place after the template has been parsed;
/// the PostFix comments are removed and are no longer included in the output.
/// </remarks>
function TTmplCodeGen.PostFix(const AOutputContent: String): String;
var
  Command    : String;
  LastIndex  : Integer;
  Line       : String;
  LineMatch  : TMatch;
  LineMatches: TMatchCollection;
  SB         : TStringBuilder;
begin
  LineMatches := TRegEx.Matches(AOutputContent, '^(?P<Line>.*)\s+\/\/\s*(?P<Command>PostFix\w+)$',
    [roIgnoreCase, roMultiLine]);

  if LineMatches.Count = 0 then
    Exit(AOutputContent);

  SB := TStringBuilder.Create(AOutputContent.Length);
  try
    LastIndex := 1;
    for LineMatch in LineMatches do
    begin
      if LineMatch.Index > LastIndex then
        SB.Append(AOutputContent, LastIndex - 1, LineMatch.Index - LastIndex);

      Line := LineMatch.Groups['Line'].Value;
      Command := LineMatch.Groups['Command'].Value;

      Line := PostFixRoute(Command, Line);
      SB.Append(Line);

      LastIndex := LineMatch.Index + LineMatch.Length;
    end;

    if LastIndex <= AOutputContent.Length then
      SB.Append(AOutputContent, LastIndex - 1, AOutputContent.Length - LastIndex + 1);

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TTmplCodeGen.PostFixParamsCall(const ALine: String): String;
var
  Match           : TMatch;
  ParenthesesGroup: TGroup;
  Parentheses     : String;
  SubMatch        : TMatch;
begin
  Result := ALine;
  Match := FParamsCallRegEx.Match(ALine);
  if not Match.Success then
    Exit;

  ParenthesesGroup := Match.Groups['Parentheses'];
  Parentheses := ParenthesesGroup.Value;

  if FEmptyParenthesesRegEx.IsMatch(Parentheses) then
  begin
    Parentheses := ''; // Leere Klammern entfernen
  end
  else
  begin
    SubMatch := FParamsCallSubRegEx.Match(Parentheses);
    if SubMatch.Success then
      Parentheses := '(' + SubMatch.Groups['Params'].Value + ')';
    // Abschließendes invalides Komma entfernen
  end;

  if Parentheses <> ParenthesesGroup.Value then
    Result := Copy(ALine, 1, ParenthesesGroup.Index - 1) + Parentheses +
      Copy(ALine, ParenthesesGroup.Index + ParenthesesGroup.Length);
end;

/// <remarks>
/// If a dynamic number of parameters needs to be implemented for a method in the template,
/// this PostFix can be helpful. For example:
/// <code>procedure MyMethod(); // PostFixParamsDefine</code>
/// becomes:
/// <code>procedure MyMethod;</code>
/// Or:
/// <code>procedure MyMethod(ADynParam: String; ADynParam2: Integer;); // PostFixParamsDefine</code>
/// becomes:
/// <code>procedure MyMethod(ADynParam: String; ADynParam2: Integer);</code>
/// </remarks>
function TTmplCodeGen.PostFixParamsDefine(const ALine: String): String;
var
  Match           : TMatch;
  ParenthesesGroup: TGroup;
  Parentheses     : String;
  SubMatch        : TMatch;
begin
  Result := ALine;
  Match := FParamsDefineRegEx.Match(ALine);
  if not Match.Success then
    Exit;

  ParenthesesGroup := Match.Groups['Parentheses'];
  Parentheses := ParenthesesGroup.Value;

  if FEmptyParenthesesRegEx.IsMatch(Parentheses) then
  begin
    Parentheses := ''; // Leere Klammern entfernen
  end
  else
  begin
    SubMatch := FParamsDefineSubRegEx.Match(Parentheses);
    if SubMatch.Success then
      Parentheses := '(' + SubMatch.Groups['Params'].Value + ')';
    // Abschließendes invalides Semikolon entfernen
  end;

  if Parentheses <> ParenthesesGroup.Value then
    Result := Copy(ALine, 1, ParenthesesGroup.Index - 1) + Parentheses +
      Copy(ALine, ParenthesesGroup.Index + ParenthesesGroup.Length);
end;

function TTmplCodeGen.PostFixRoute(const ACommand, ALine: String): String;
begin
  if SameText(ACommand, 'PostFixParamsDefine') then
  begin
    Result := PostFixParamsDefine(ALine);
  end
  else if SameText(ACommand, 'PostFixParamsCall') then
  begin
    Result := PostFixParamsCall(ALine);
  end
  else
  begin
    Result := ALine;
  end;
end;

procedure TTmplCodeGen.PrefixHelper(const Value: Variant; out Result: Variant);
begin
  Result := FPrefix;
end;

procedure TTmplCodeGen.PreProcessConfJson;
var
  PreProcessor: TPreProcessConfigForCollectionsList;
begin
  PreProcessor := TPreProcessConfigForCollectionsList.Create(FConfJson);
  try
    if PreProcessor.Execute then
      FConfJson := PreProcessor.ConfJsonOut;
  finally
    PreProcessor.Free;
  end;
end;

procedure TTmplCodeGen.ExportPartials(const AOutputContent: String);

  procedure ExportPartial(const AStartMatch: TMatch);
  var
    EndIndex       : Integer;
    EndMatch       : TMatch;
    EndRegEx       : TRegEx;
    FileStream     : TFileStream;
    PartialContent : String;
    PartialFileName: String;
    PartialName    : String;
    StartIndex     : Integer;
    StringStream   : TStringStream;
  begin
    PartialName := AStartMatch.Groups['Param'].Value;
    EndRegEx := TRegEx.Create(CreateRegExpr(DefinePartial, false, PartialName), [roIgnoreCase]);
    EndMatch := EndRegEx.Match(AOutputContent);
    if not EndMatch.Success then
      raise Exception.CreateFmt('Closing region for %s not found',
        [PartialName]);
    PartialFileName := ChangeFileExt(FOutputFileName, '-' + PartialName + PartialApndx);
    StartIndex := AStartMatch.Index + AStartMatch.Length;
    EndIndex := EndMatch.Index;

    if EndIndex < StartIndex then
      raise Exception.Create('Invalid match positions');

    PartialContent := Copy(AOutputContent, StartIndex, EndIndex - StartIndex);

    if FWriteOnlyIfChanged and FileExists(PartialFileName) and (TFile.ReadAllText(PartialFileName, TEncoding.UTF8) = PartialContent) then
      Exit;

    FLogger.Log('- ' + PartialFileName);
    FileStream := nil;
    StringStream := TStringStream.Create(PartialContent, TEncoding.UTF8, false);
    try
      FileStream := TFileStream.Create(PartialFileName, fmCreate);
      FileStream.CopyFrom(StringStream);
    finally
      StringStream.Free;
      FileStream.Free;
    end;
  end;

var
  EndMatches  : TMatchCollection;
  EndRegEx    : TRegEx;
  Loop        : Integer;
  StartMatches: TMatchCollection;
  StartRegEx  : TRegEx;
begin
  StartRegEx := TRegEx.Create(CreateRegExpr(DefinePartial, true), [roIgnoreCase]);
  EndRegEx := TRegEx.Create(CreateRegExpr(DefinePartial, false), [roIgnoreCase]);

  StartMatches := StartRegEx.Matches(AOutputContent);
  if StartMatches.Count = 0 then
    Exit;
  EndMatches := EndRegEx.Matches(AOutputContent);
  if StartMatches.Count <> EndMatches.Count then
    raise Exception.CreateFmt(
      'Number of start/end regions %s (%d / %d) does not match',
      [DefinePartial, StartMatches.Count, EndMatches.Count]);

  FLogger.Log('Output Partials:');

  for Loop := 0 to StartMatches.Count - 1 do
    ExportPartial(StartMatches.Item[Loop]);
end;

procedure TTmplCodeGen.ProcessTemplate;
var
  ConfJsonContent    : RawUtf8;
  FullTemplatesDir   : String;
  GeneratedString    : String;
  Helpers            : TSynMustacheHelpers;
  mustache           : TSynMustache;
  OutputContent      : RawUtf8;
  PartialFileName    : String;
  PartialFileNameOnly: String;
  PartialFileNames   : TArray<String>;
  PartialName        : UTF8String;
  Partials           : TSynMustachePartials;
  TemplateContent    : RawUtf8;
  TemplatesPath      : String;
begin
  FConfJsonFileName := FPrefix + ConfJsonFileApndx;

  if FConfJson.Count = 0 then
  begin
     // Try load from file
     if not FileExists(FConfJsonFileName) then
       raise Exception.CreateFmt('File not found "%s"', [FConfJsonFileName]);

     ConfJsonContent := RawUtf8FromFile(FConfJsonFileName);

     if not FConfJson.InitJson(ConfJsonContent) then
       raise Exception.CreateFmt('Conf file "%s" not valid', [FConfJsonFileName]);
  end;

  if not (FConfJson.GetValueIndex('Template') >= 0) then
    raise Exception.Create('No template defined (Key "Template" missing)');

  if FConfJson.GetValueIndex('TemplatesPath') >= 0 then
    TemplatesPath := FConfJson.S['TemplatesPath']
  else
    TemplatesPath := TEMPLATES_DIR;

  PreProcessConfJson;
  FMainTemplate := FConfJson.S['Template'];
  FMainTemplateFileName := ExpandFileName(
    IncludeTrailingPathDelimiter(TemplatesPath) + FMainTemplate);
  FullTemplatesDir := ExpandFileName(IncludeTrailingPathDelimiter(TemplatesPath));

  if not FileExists(FMainTemplateFileName) then
    raise Exception.CreateFmt('Template not found "%s"', [FMainTemplateFileName]);

  FLogger.Log('Conf file         : ' + FConfJsonFileName);
  FLogger.Log('Main template     : ' + FMainTemplate);
  FLogger.Log('Main template file: ' + FMainTemplateFileName);

  if FConfJson.GetValueIndex('OutputExtension') >= 0 then
    FOutputFileName := ExpandFileName(FPrefix + FConfJson.S['OutputExtension'])
  else
  begin
    var TemplateExt: String := ExtractFileExt(FMainTemplate);
    if TemplateExt.IsEmpty then
      TemplateExt := OutputApndx;
    FOutputFileName := ExpandFileName(FPrefix + TemplateExt);
  end;

  FLogger.Log('Output file       : ' + FOutputFileName);

  TemplateContent := RawUtf8FromFile(FMainTemplateFileName);
  mustache := TSynMustache.Parse(TemplateContent);
  Helpers := TSynMustache.HelpersGetStandardList;
  TSynMustache.HelperAdd(Helpers, 'Prefix', PrefixHelper);

  Partials := TSynMustachePartials.Create;
  try
    PartialFileNames := TDirectory.GetFiles(FullTemplatesDir, '*' + PartialApndx);
    for PartialFileName in PartialFileNames do
    begin
      PartialFileNameOnly := ExtractFileName(PartialFileName);
      PartialName := StringToUTF8(StringReplace(PartialFileNameOnly, PartialApndx, '',
        [rfReplaceAll]));
      Partials.Add(PartialName, RawUtf8FromFile(PartialFileName));
    end;

    OutputContent := mustache.Render(Variant(FConfJson), Partials, Helpers, nil,
      { EscapeInvert = } true);
    
    GeneratedString := String(OutputContent);
    GeneratedString := RemoveMultipleCommentLines(GeneratedString);
    GeneratedString := RemoveBigNewLineGaps(GeneratedString);
    GeneratedString := PostFix(GeneratedString);

    if FWriteOnlyIfChanged and FileExists(FOutputFileName) then
    begin
      var ExistingContent: String := TFile.ReadAllText(FOutputFileName, TEncoding.UTF8);
      if ExistingContent = GeneratedString then
      begin
        FLogger.Log('Output file is up to date: ' + FOutputFileName);
        Exit;
      end;
    end;

    OutputContent := UTF8String(GeneratedString);
    if not FileFromString(OutputContent, FOutputFileName) then
      raise Exception.Create('Output file could not be written');

    ExportPartials(GeneratedString);
  finally
    Partials.Free;
  end;
end;

end.

