// ======================================================================
// Copyright (c) 2025 Waldemar Derr. All rights reserved.
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
  TmplCodeGen.PreProcess,
  TmplCodeGen.Utils;

type

  TTmplCodeGen = class
  private
    FConfJson            : TDocVariantData;
    FConfJsonFileName    : String;
    FMainTemplate        : String;
    FMainTemplateFileName: String;
    FOutputFileName      : String;
    FPrefix              : String;
    procedure ExportPartials(const AOutputContent: String);
    function  PostFix(const AOutputContent: String): String;
    function  PostFixParamsCall(const ALine: String): String;
    function  PostFixParamsDefine(const ALine: String): String;
    function  PostFixRoute(const ACommand: String; const ALine: String): String;
    procedure PrefixHelper(const Value: Variant; out Result: Variant);
    procedure PreProcessConfJson;
  public
    constructor Create(const APrefix: String);
    procedure ProcessTemplate;
  end;

implementation

{ TTmplCodeGen }

constructor TTmplCodeGen.Create(const APrefix: String);
begin
  FPrefix := APrefix;
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
  AfterLine : String;
  BeforeLine: String;
  Command   : String;
  EndIndex  : Integer;
  Line      : String;
  LineMatch : TMatch;
  LineRegEx : TRegEx;
  StartIndex: Integer;
begin
  Result := AOutputContent;
  LineRegEx := TRegEx.Create('^(?P<Line>.*)\s+\/\/\s*(?P<Command>PostFix\w+)$',
    [roIgnoreCase, roMultiLine]);
  LineMatch := LineRegEx.Match(Result);

  while LineMatch.Success do
  begin
    Line := LineMatch.Groups['Line'].Value;
    Command := LineMatch.Groups['Command'].Value;
    StartIndex := LineMatch.Index - 1;
    EndIndex := LineMatch.Index + LineMatch.Length;
    BeforeLine := Copy(Result, 1, StartIndex);
    AfterLine := Copy(Result, EndIndex);
    Line := PostFixRoute(Command, Line);
    Result := BeforeLine + Line + AfterLine;
    LineMatch := LineRegEx.Match(Result);
  end;
end;

{ ----------------------------------------------------------------------- }

function TTmplCodeGen.PostFixParamsCall(const ALine: String): String;
var
  Match           : TMatch;
  ParenthesesGroup: TGroup;
  Parentheses     : String;
  SubMatch        : TMatch;
begin
  Result := ALine;
  Match := TRegEx.Match(ALine, '([\w\.])+(?P<Parentheses>\([^\)]*\))');
  if not Match.Success then
    Exit;

  ParenthesesGroup := Match.Groups['Parentheses'];
  Parentheses := ParenthesesGroup.Value;

  if TRegEx.IsMatch(Parentheses, '\(\s*\)') then
  begin
    Parentheses := ''; // Leere Klammern entfernen
  end
  else
  begin
    SubMatch := TRegEx.Match(Parentheses, '\((?P<Params>[^\)]+),\s*\)');
    if SubMatch.Success then
      Parentheses := '(' + SubMatch.Groups['Params'].Value + ')';
    // Abschließendes invalides Komma entfernen
  end;

  if Parentheses <> ParenthesesGroup.Value then
    Result := Copy(ALine, 1, ParenthesesGroup.Index - 1) + Parentheses +
      Copy(ALine, ParenthesesGroup.Index + ParenthesesGroup.Length);
end;

{ ----------------------------------------------------------------------- }

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
  Match := TRegEx.Match(ALine,
    '(procedure|function|constructor)\s*([\w\.])*(?P<Parentheses>\([^\)]*\))');
  if not Match.Success then
    Exit;

  ParenthesesGroup := Match.Groups['Parentheses'];
  Parentheses := ParenthesesGroup.Value;

  if TRegEx.IsMatch(Parentheses, '\(\s*\)') then
  begin
    Parentheses := ''; // Leere Klammern entfernen
  end
  else
  begin
    SubMatch := TRegEx.Match(Parentheses, '\((?P<Params>[^\)]+);\s*\)');
    if SubMatch.Success then
      Parentheses := '(' + SubMatch.Groups['Params'].Value + ')';
    // Abschließendes invalides Semikolon entfernen
  end;

  if Parentheses <> ParenthesesGroup.Value then
    Result := Copy(ALine, 1, ParenthesesGroup.Index - 1) + Parentheses +
      Copy(ALine, ParenthesesGroup.Index + ParenthesesGroup.Length);
end;

{ ----------------------------------------------------------------------- }

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

{ ----------------------------------------------------------------------- }

procedure TTmplCodeGen.PrefixHelper(const Value: Variant; out Result: Variant);
begin
  Result := FPrefix;
end;

{ ----------------------------------------------------------------------- }

procedure TTmplCodeGen.PreProcessConfJson;
var
  PreProcessor: CPreProcessConfigForBaseCollectionsList;
begin
  PreProcessor := CPreProcessConfigForBaseCollectionsList.Create(FConfJson);
  try
    if PreProcessor.Execute then
      FConfJson := PreProcessor.ConfJsonOut;
  finally
    PreProcessor.Free;
  end;
end;

{ ----------------------------------------------------------------------- }

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
      raise Exception.CreateFmt('Schliessende Region für %s nicht gefunden',
        [PartialName]);
    PartialFileName := ChangeFileExt(FOutputFileName, '-' + PartialName + PartialApndx);
    StartIndex := AStartMatch.Index + AStartMatch.Length;
    EndIndex := EndMatch.Index;

    if EndIndex < StartIndex then
      raise Exception.Create('Ungültige Fundstellen');

    Writeln('- ' + PartialFileName);
    PartialContent := Copy(AOutputContent, StartIndex, EndIndex - StartIndex);
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
      'Anzahl der startenden/schliessenden Regionen %s (%d / %d) unterschiedlich',
      [DefinePartial, StartMatches.Count, EndMatches.Count]);

  Writeln('Output Partials:');

  for Loop := 0 to StartMatches.Count - 1 do
    ExportPartial(StartMatches.Item[Loop]);
end;

procedure TTmplCodeGen.ProcessTemplate;
var
  ConfJsonContent    : RawUtf8;
  FullTemplatesDir   : String;
  Helpers            : TSynMustacheHelpers;
  mustache           : TSynMustache;
  OutputContent      : RawUtf8;
  PartialFileName    : String;
  PartialFileNameOnly: String;
  PartialFileNames   : TArray<String>;
  PartialName        : UTF8String;
  Partials           : TSynMustachePartials;
  TemplateContent    : RawUtf8;
begin
  FConfJsonFileName := FPrefix + ConfJsonFileApndx;

  if not FileExists(FConfJsonFileName) then
    raise Exception.CreateFmt('File not found "%s"', [FConfJsonFileName]);

  ConfJsonContent := RawUtf8FromFile(FConfJsonFileName);

  if not FConfJson.InitJson(ConfJsonContent) then
    raise Exception.CreateFmt('Conf file "%s" not valid', [FConfJsonFileName]);

  if not (FConfJson.GetValueIndex('Template') >= 0) then
    raise Exception.Create('No template defined (Key "Template" missing)');

  PreProcessConfJson;
  FMainTemplate := FConfJson.S['Template'];
  FMainTemplateFileName := ExpandFileName(
    IncludeTrailingPathDelimiter(TemplatesDir) + FMainTemplate);
  FullTemplatesDir := ExpandFileName(IncludeTrailingPathDelimiter(TemplatesDir));

  if not FileExists(FMainTemplateFileName) then
    raise Exception.CreateFmt('Template not found "%s"', [FMainTemplateFileName]);

  FOutputFileName := ExpandFileName(FPrefix + OutputApndx);

  Writeln('Conf file         : ' + FConfJsonFileName);
  Writeln('Main template     : ' + FMainTemplate);
  Writeln('Main template file: ' + FMainTemplateFileName);
  Writeln('Output file       : ' + FOutputFileName);

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
      PartialName := UTF8String(StringReplace(PartialFileNameOnly, PartialApndx, '',
        [rfReplaceAll]));
      Partials.Add(PartialName, RawUtf8FromFile(PartialFileName));
    end;

    OutputContent := mustache.Render(Variant(FConfJson), Partials, Helpers, nil,
      { EscapeInvert = } true);
    OutputContent := UTF8String(RemoveMultipleCommentLines(String(OutputContent)));
    OutputContent := UTF8String(RemoveBigNewLineGaps(String(OutputContent)));
    OutputContent := UTF8String(PostFix(String(OutputContent)));

    if not FileFromString(OutputContent, FOutputFileName) then
      raise Exception.Create('Output file could not be written');

    ExportPartials(String(OutputContent));
  finally
    Partials.Free;
  end;
end;

end.
