// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit TmplCodeGen.Includer;

interface

uses

  System.Classes,
  System.IOUtils,
  System.RegularExpressions,
  System.SysUtils,

  TmplCodeGen.Common,
  TmplCodeGen.Logger,
  TmplCodeGen.Utils;

type

  TIncludePartials = class
  private
    FLogger    : ILogger;
    FTargetFile: String;
    function RetakeInterfaceGuids(const AOldPartial, ANewPartial: String): String;
  public
    constructor Create(const ATargetFile: String; const ALogger: ILogger);
    procedure Execute;
  end;

implementation

{ TIncludePartials }

constructor TIncludePartials.Create(const ATargetFile: String; const ALogger: ILogger);
begin
  FTargetFile := ATargetFile;
  FLogger := ALogger;
end;

procedure TIncludePartials.Execute;
var
  AfterPartialContent : String;
  BeforePartialContent: String;
  DoneIncludes        : Integer;
  EndIndex            : Integer;
  EndMatch            : TMatch;
  EndRegEx            : TRegEx;
  FoundIncludes       : Integer;
  OldPartialContent   : String;
  PartialContent      : String;
  PartialFileName     : String;
  StartIndex          : Integer;
  StartMatch          : TMatch;
  StartMatchPos       : Integer;
  StartRegEx          : TRegEx;
  TargetContent       : String;
begin
  FLogger.Log('Include-Partials in ' + FTargetFile);

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
    if not (StartMatch.Success and EndMatch.Success) then
      raise Exception.CreateFmt('No %s regions found', [IncludePartial]);

    StartIndex := StartMatch.Index + StartMatch.Length - 1;
    EndIndex := EndMatch.Index;

    PartialFileName := StartMatch.Groups['Param'].Value;
    PartialContent := TFile.ReadAllText(PartialFileName, TEncoding.UTF8);

    BeforePartialContent := Copy(TargetContent, 1, StartIndex);
    OldPartialContent := Copy(TargetContent, StartIndex, EndIndex - StartIndex);
    AfterPartialContent := Copy(TargetContent, EndIndex);

    PartialContent := RetakeInterfaceGuids(OldPartialContent, PartialContent);
    PartialContent := RemovePartialStmts(PartialContent, [DefinePartial]);
    PartialContent := RemoveTrailingCommentLines(PartialContent);

    StartMatchPos := Length(BeforePartialContent) + Length(PartialContent) + EndMatch.Length;

    TargetContent := BeforePartialContent + sLineBreak + PartialContent + sLineBreak +
      AfterPartialContent;
    FLogger.Log(Format(
      '- "%s" included between index %d and %d',
      [PartialFileName, StartIndex, EndIndex]));
    Inc(DoneIncludes);
  end;

  TargetContent := RemoveBigNewLineGaps(TargetContent);
  TFile.WriteAllText(FTargetFile, TargetContent, TEncoding.UTF8);
end;

/// <remarks>
/// To ensure that new GUIDs are not assigned to the interfaces with every run of the code generator,
/// we try to adopt them from the previous partial into the new partial.
/// </remarks>
function TIncludePartials.RetakeInterfaceGuids(const AOldPartial, ANewPartial: String): String;
const
  GuidRegEx = '(?<GUID>\[''{[A-F0-9]{8}(\-[A-F0-9]{4}){3}\-[A-F0-9]{12}}''\])';
var
  NewRegEx             : String;
  OldInterfaceSignature: String;
  OldMatch             : TMatch;
  OldMatches           : TMatchCollection;
  Replacement          : String;
begin
  Result := ANewPartial;
  OldMatches := TRegEx.Matches(AOldPartial,
    '(?<InterfaceSignature>\w+\s*=\s*interface(\([^\)]+\))?)(?<Whitespace>\s+)'
    + GuidRegEx, [roIgnoreCase, roMultiLine]);

  for OldMatch in OldMatches do
  begin
    OldInterfaceSignature := OldMatch.Groups['InterfaceSignature'].Value;
    NewRegEx := Format('(?<InterfaceSignature>%s)(?<Whitespace>\s+)',
      [TRegEx.Escape(OldInterfaceSignature)]) + GuidRegEx;
    Replacement := OldMatch.Value;
    Result := TRegEx.Replace(Result, NewRegEx, Replacement, [roIgnoreCase, roMultiLine]);
  end;
end;

end.
