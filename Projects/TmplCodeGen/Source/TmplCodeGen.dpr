// ======================================================================
// Copyright (c) 2025 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================
program TmplCodeGen;

{$APPTYPE CONSOLE}
{$R *.res}

uses

  System.Classes,
  System.Diagnostics,
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

  TmplCodeGen.PreProcess in 'TmplCodeGen.PreProcess.pas';

const

  ConfJsonFileApndx = '-conf.json';
  OutputApndx = '.pas';
  PartialApndx = '.part.pas';
  TemplatesDir = 'TEMPLATES';

const

  DefinePartial = 'DEFINE-PARTIAL';
  IncludePartial = 'INCLUDE-PARTIAL';

type

  TTmplCodeGen = class
  private
    FConfJson: TDocVariantData;
    FConfJsonFileName: String;
    FMainTemplate: String;
    FMainTemplateFileName: String;
    FOutputFileName: String;
    FPrefix: String;
    procedure ExportPartials(const AOutputContent: String);
    function PostFix(const AOutputContent: String): String;
    function PostFixParamsCall(const ALine: String): String;
    function PostFixParamsDefine(const ALine: String): String;
    function PostFixRoute(const ACommand: String; const ALine: String): String;
    procedure PrefixHelper(const Value: Variant; out Result: Variant);
    procedure PreProcessConfJson;
  public
    constructor Create(const APrefix: String);
    procedure ProcessTemplate;
  end;

  TIncludePartials = class
  private
    FTargetFile: String;
    function RetakeInterfaceGuids(const AOldPartial,
      ANewPartial: String): String;
  public
    constructor Create(const ATargetFile: String);
    procedure Execute;
  end;

function CreateRegExpr(const APartialStmt: String; AStartRegion: Boolean; const AExpectedParam: String = ''): String;
var
  Region: String;
begin
  if AStartRegion then
    Region := 'REGION'
  else
    Region := 'ENDREGION';

  Result := ' *\{\$' + Region + '\s+''(?P<Stmt>' + APartialStmt + ')\s+\/\s+';
  if AExpectedParam = '' then
    Result := Result + '(?P<Param>[^'']+)'
  else
    Result := Result + '(?P<Param>' + TRegEx.Escape(AExpectedParam) + ')';
  Result := Result + '''\s*\}';
end;

function RemovePartialStmts(const AContent: String; const AStmts: Array of String): String;
begin
  Result := AContent;
  for var Stmt in AStmts do
  begin
    Result := TRegEx.Replace(Result, CreateRegExpr(Stmt, true), '', [roIgnoreCase]);
    Result := TRegEx.Replace(Result, CreateRegExpr(Stmt, false), '', [roIgnoreCase]);
  end;
end;

function RemoveTrailingCommentLines(const AContent: String): String;
begin
  Result := TRegEx.Replace(AContent, '({ (-{3,}|={3,}) }\s*)+$', '', []);
end;

function RemoveMultipleCommentLines(const AContent: String): String;
begin
  Result := TRegEx.Replace(AContent, '{ -{3,} }\s+{ ={3,} }',
    '{ ======================================================================= }',
    []);
  Result := TRegEx.Replace(Result, '({ -{3,} }\s+){2,}',
    '{ ----------------------------------------------------------------------- }',
    []);
end;

function RemoveBigNewLineGaps(const AContent: String): String;
var
  PrevLength: Integer;
begin
  Result := AContent;
  repeat
    PrevLength := Length(Result);
    Result := TRegEx.Replace(Result, '(\r\n([ \t]*\r\n){2,})+', sLineBreak + sLineBreak, []);
  until PrevLength = Length(Result);
end;

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
  StartIndex: Integer;
begin
  Result := AOutputContent;

  var LineRegEx: TRegEx := TRegEx.Create('^(?P<Line>.*)\s+\/\/\s*(?P<Command>PostFix\w+)$',
    [roIgnoreCase, roMultiLine]);
  var LineMatch: TMatch := LineRegEx.Match(Result);

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
begin
  Result := ALine;
  var Match: TMatch := TRegEx.Match(ALine, '([\w\.])+(?P<Parentheses>\([^\)]*\))');
  if not Match.Success then
    Exit;

  var ParenthesesGroup: TGroup := Match.Groups['Parentheses'];
  var Parentheses: String := ParenthesesGroup.Value;

  if TRegEx.IsMatch(Parentheses, '\(\s*\)') then
  begin
    Parentheses := ''; // Leere Klammern entfernen
  end
  else
  begin
    var SubMatch: TMatch := TRegEx.Match(Parentheses, '\((?P<Params>[^\)]+),\s*\)');
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
begin
  Result := ALine;
  var Match: TMatch := TRegEx.Match(ALine,
    '(procedure|function|constructor)\s*([\w\.])*(?P<Parentheses>\([^\)]*\))');
  if not Match.Success then
    Exit;

  var ParenthesesGroup: TGroup := Match.Groups['Parentheses'];
  var Parentheses: String := ParenthesesGroup.Value;

  if TRegEx.IsMatch(Parentheses, '\(\s*\)') then
  begin
    Parentheses := ''; // Leere Klammern entfernen
  end
  else
  begin
    var SubMatch: TMatch := TRegEx.Match(Parentheses, '\((?P<Params>[^\)]+);\s*\)');
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
begin
  var PreProcessor: CPreProcessConfigForBaseCollectionsList :=
    CPreProcessConfigForBaseCollectionsList.Create(FConfJson);
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
  begin
    var PartialName: String := AStartMatch.Groups['Param'].Value;
    var EndRegEx: TRegEx := TRegEx.Create(CreateRegExpr(DefinePartial, false, PartialName), [roIgnoreCase]);
    var EndMatch: TMatch := EndRegEx.Match(AOutputContent);
    if not EndMatch.Success then
      raise Exception.CreateFmt('Schliessende Region für %s nicht gefunden',
        [PartialName]);
    var PartialFileName: String := ChangeFileExt(FOutputFileName,
      '-' + PartialName + PartialApndx);
    var StartIndex: Integer := AStartMatch.Index + AStartMatch.Length;
    var EndIndex: Integer := EndMatch.Index;

    if EndIndex < StartIndex then
      raise Exception.Create('Ungültige Fundstellen');

    Writeln('- ' + PartialFileName);
    var PartialContent: String := Copy(AOutputContent, StartIndex, EndIndex - StartIndex);
    var FileStream: TFileStream := nil;
    var StringStream: TStringStream := TStringStream.Create(PartialContent, TEncoding.UTF8, false);
    try
      FileStream := TFileStream.Create(PartialFileName, fmCreate);
      FileStream.CopyFrom(StringStream);
    finally
      StringStream.Free;
      FileStream.Free;
    end;
  end;

var
  EndRegEx  : TRegEx;
  StartRegEx: TRegEx;
begin
  StartRegEx := TRegEx.Create(CreateRegExpr(DefinePartial, true), [roIgnoreCase]);
  EndRegEx := TRegEx.Create(CreateRegExpr(DefinePartial, false), [roIgnoreCase]);

  var StartMatches: TMatchCollection := StartRegEx.Matches(AOutputContent);
  if StartMatches.Count = 0 then
    Exit;
  var EndMatches: TMatchCollection := EndRegEx.Matches(AOutputContent);
  if StartMatches.Count <> EndMatches.Count then
    raise Exception.CreateFmt(
      'Anzahl der startenden/schliessenden Regionen %s (%d / %d) unterschiedlich',
      [DefinePartial, StartMatches.Count, EndMatches.Count]);

  Writeln('Output Partials:');

  for var Loop: Integer := 0 to StartMatches.Count - 1 do
    ExportPartial(StartMatches.Item[Loop]);
end;

procedure TTmplCodeGen.ProcessTemplate;
begin
  FConfJsonFileName := FPrefix + ConfJsonFileApndx;

  if not FileExists(FConfJsonFileName) then
    raise Exception.CreateFmt('File not found "%s"', [FConfJsonFileName]);

  var ConfJsonContent: RawUtf8 := RawUtf8FromFile(FConfJsonFileName);

  if not FConfJson.InitJson(ConfJsonContent) then
    raise Exception.CreateFmt('Conf file "%s" not valid', [FConfJsonFileName]);

  if not FConfJson.GetValueIndex('Template') >= 0 then
    raise Exception.Create('No template defined (Key "Template" missing)');

  PreProcessConfJson;
  FMainTemplate := FConfJson.S['Template'];
  FMainTemplateFileName := ExpandFileName(
    IncludeTrailingPathDelimiter(TemplatesDir) + FMainTemplate);
  var FullTemplatesDir: String := ExpandFileName(IncludeTrailingPathDelimiter(TemplatesDir));

  if not FileExists(FMainTemplateFileName) then
    raise Exception.CreateFmt('Template not found "%s"', [FMainTemplateFileName]);

  FOutputFileName := ExpandFileName(FPrefix + OutputApndx);

  Writeln('Conf file         : ' + FConfJsonFileName);
  Writeln('Main template     : ' + FMainTemplate);
  Writeln('Main template file: ' + FMainTemplateFileName);
  Writeln('Output file       : ' + FOutputFileName);

  var TemplateContent: RawUtf8 := RawUtf8FromFile(FMainTemplateFileName);
  var mustache: TSynMustache := TSynMustache.Parse(TemplateContent);
  var Helpers: TSynMustacheHelpers := TSynMustache.HelpersGetStandardList;
  TSynMustache.HelperAdd(Helpers, 'Prefix', PrefixHelper);

  var Partials: TSynMustachePartials := TSynMustachePartials.Create;
  var PartialFileNames: TArray<String> := TDirectory.GetFiles(FullTemplatesDir, '*' + PartialApndx);
  for var PartialFileName: String in PartialFileNames do
  begin
    var PartialFileNameOnly: String := ExtractFileName(PartialFileName);
    var PartialName: UTF8String := UTF8String(StringReplace(PartialFileNameOnly, PartialApndx, '',
      [rfReplaceAll]));
    Partials.Add(PartialName, RawUtf8FromFile(PartialFileName));
  end;

  var OutputContent: RawUtf8 := mustache.Render(Variant(FConfJson), Partials, Helpers, nil,
    { EscapeInvert = } true);
  OutputContent := UTF8String(RemoveMultipleCommentLines(String(OutputContent)));
  OutputContent := UTF8String(RemoveBigNewLineGaps(String(OutputContent)));
  OutputContent := UTF8String(PostFix(String(OutputContent)));

  if not FileFromString(OutputContent, FOutputFileName) then
    raise Exception.Create('Output file could not be written');

  ExportPartials(String(OutputContent));
end;

{ TIncludePartials }

constructor TIncludePartials.Create(const ATargetFile: String);
begin
  FTargetFile := ATargetFile;
end;

procedure TIncludePartials.Execute;
var
  DoneIncludes : Integer;
  EndIndex     : Integer;
  EndMatch     : TMatch;
  EndRegEx     : TRegEx;
  FoundIncludes: Integer;
  StartIndex   : Integer;
  StartMatch   : TMatch;
  StartMatchPos: Integer;
  StartRegEx   : TRegEx;
  TargetContent: String;
begin
  Writeln('Include-Partials in ' + FTargetFile);

  StartRegEx := TRegEx.Create(CreateRegExpr(IncludePartial, true), [roIgnoreCase]);
  EndRegEx := TRegEx.Create(CreateRegExpr(IncludePartial, false), [roIgnoreCase]);

  TargetContent := TFile.ReadAllText(FTargetFile, TEncoding.UTF8);

  FoundIncludes := StartRegEx.Matches(TargetContent).Count;
  DoneIncludes := 0;
  StartMatchPos := 0;

  while DoneIncludes < FoundIncludes do
  begin
    StartMatch := StartRegEx.Match(TargetContent, StartMatchPos);
    EndMatch := EndRegEx.Match(TargetContent, StartMatchPos);
    if not(StartMatch.Success and EndMatch.Success) then
      raise Exception.CreateFmt('Keine %s-Regionen gefunden', [IncludePartial]);

    StartIndex := StartMatch.Index + StartMatch.Length - 1;
    EndIndex := EndMatch.Index;

    var PartialFileName: String := StartMatch.Groups['Param'].Value;
    var PartialContent: String := TFile.ReadAllText(PartialFileName, TEncoding.UTF8);

    var BeforePartialContent: String := Copy(TargetContent, 1, StartIndex);
    var OldPartialContent: String := Copy(TargetContent, StartIndex, EndIndex - StartIndex);
    var AfterPartialContent: String := Copy(TargetContent, EndIndex);

    PartialContent := RetakeInterfaceGuids(OldPartialContent, PartialContent);
    PartialContent := RemovePartialStmts(PartialContent, [DefinePartial]);
    PartialContent := RemoveTrailingCommentLines(PartialContent);

    StartMatchPos := Length(BeforePartialContent) + Length(PartialContent) + EndMatch.Length;

    TargetContent := BeforePartialContent + sLineBreak + PartialContent + sLineBreak +
      AfterPartialContent;
    Writeln(Format(
      '- "%s" included between index %d and %d',
      [PartialFileName, StartIndex, EndIndex]));
    Inc(DoneIncludes);
  end;

  TargetContent := RemoveBigNewLineGaps(TargetContent);
  TFile.WriteAllText(FTargetFile, TargetContent, TEncoding.UTF8);
end;

{ ----------------------------------------------------------------------- }

/// <remarks>
/// To ensure that new GUIDs are not assigned to the interfaces with every run of the code generator,
/// we try to adopt them from the previous partial into the new partial.
/// </remarks>
function TIncludePartials.RetakeInterfaceGuids(const AOldPartial, ANewPartial: String): String;
const
  GuidRegEx = '(?<GUID>\[''{[A-F0-9]{8}(\-[A-F0-9]{4}){3}\-[A-F0-9]{12}}''\])';
begin
  Result := ANewPartial;
  var OldMatches: TMatchCollection := TRegEx.Matches(AOldPartial,
    '(?<InterfaceSignature>\w+\s*=\s*interface(\([^\)]+\))?)(?<Whitespace>\s+)'
    + GuidRegEx, [roIgnoreCase, roMultiLine]);

  for var OldMatch in OldMatches do
  begin
    var OldInterfaceSignature: String := OldMatch.Groups['InterfaceSignature'].Value;
    var NewRegEx: String := Format('(?<InterfaceSignature>%s)(?<Whitespace>\s+)',
      [TRegEx.Escape(OldInterfaceSignature)]) + GuidRegEx;
    var Replacement := OldMatch.Value;
    Result := TRegEx.Replace(Result, NewRegEx, Replacement, [roIgnoreCase, roMultiLine]);
  end;
end;

{ ======================================================================= }

procedure ProcessTemplate(const APrefix: String);
begin
  var CodeGen: TTmplCodeGen := TTmplCodeGen.Create(APrefix);
  try
    CodeGen.ProcessTemplate;
  finally
    CodeGen.Free;
  end;
end;

procedure IncludePartials(const ATargetFile: String);
begin
  var IncPartials: TIncludePartials := TIncludePartials.Create(ATargetFile);
  try
    IncPartials.Execute;
  finally
    IncPartials.Free;
  end;
end;

begin
  try
    if ParamCount < 1 then
    begin
      var ExeFileName: String := ExtractFileName(ParamStr(0));
      Writeln(Format('%s prefix', [ExeFileName]));
      Writeln(Format('%s include_partials target_file', [ExeFileName]));
      Exit;
    end;

    var Stopper: TStopwatch := TStopwatch.StartNew;
    var FirstParam: String := ParamStr(1);

    if SameText(FirstParam, 'include_partials') and (ParamCount = 2) then
      IncludePartials(ParamStr(2))
    else
      ProcessTemplate(FirstParam);

    Writeln(Format(
      'Success! (Duration: %.d ms.)',
      [Stopper.ElapsedMilliseconds]) + sLineBreak);
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 42;
    end;
  end;
end.
