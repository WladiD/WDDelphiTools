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
  EndMatch         : TMatch;
  EndRegEx         : TRegEx;
  FoundIncludes    : Integer;
  LastIndex        : Integer;
  OldPartialContent: String;
  OldPartialLen    : Integer;
  OldPartialStart  : Integer;
  PartialContent   : String;
  PartialFileName  : String;
  SB               : TStringBuilder;
  StartMatch       : TMatch;
  StartRegEx       : TRegEx;
  TargetContent    : String;
begin
  FLogger.Log('Include-Partials in ' + FTargetFile);

  StartRegEx := TRegEx.Create(CreateRegExpr(IncludePartial, true), [roIgnoreCase]);
  EndRegEx := TRegEx.Create(CreateRegExpr(IncludePartial, false), [roIgnoreCase]);

  TargetContent := TFile.ReadAllText(FTargetFile, TEncoding.UTF8);

  FoundIncludes := StartRegEx.Matches(TargetContent).Count;

  if FoundIncludes = 0 then
    Exit;

  SB := TStringBuilder.Create(TargetContent.Length);
  try
    LastIndex := 1;
    StartMatch := StartRegEx.Match(TargetContent);

    while StartMatch.Success do
    begin
      EndMatch := EndRegEx.Match(TargetContent, StartMatch.Index + StartMatch.Length);
      if not EndMatch.Success then
        raise Exception.CreateFmt('No %s regions found (closing tag missing)', [IncludePartial]);

      // 1. Append text before the include + start tag
      SB.Append(TargetContent, LastIndex - 1, (StartMatch.Index + StartMatch.Length) - LastIndex);
      SB.Append(sLineBreak);

      // 2. Load partial
      PartialFileName := StartMatch.Groups['Param'].Value;
      PartialContent := TFile.ReadAllText(PartialFileName, TEncoding.UTF8);

      // 3. Extract old content for GUID retake
      OldPartialStart := StartMatch.Index + StartMatch.Length;
      OldPartialLen := EndMatch.Index - OldPartialStart;
      if OldPartialLen > 0 then
        OldPartialContent := Copy(TargetContent, OldPartialStart, OldPartialLen)
      else
        OldPartialContent := '';

      // 4. Process
      PartialContent := RetakeInterfaceGuids(OldPartialContent, PartialContent);
      PartialContent := RemovePartialStmts(PartialContent, [DefinePartial]);
      PartialContent := RemoveTrailingCommentLines(PartialContent);

      // 5. Insert new content
      SB.Append(PartialContent);
      SB.Append(sLineBreak);

      FLogger.Log(Format(
        '- "%s" included between index %d and %d',
        [PartialFileName, StartMatch.Index + StartMatch.Length, EndMatch.Index]));

      // 6. Continue searching from the end of the current end tag
      LastIndex := EndMatch.Index;
      StartMatch := StartRegEx.Match(TargetContent, EndMatch.Index + EndMatch.Length);
    end;

    // Den Rest der Datei anfügen
    if LastIndex <= Length(TargetContent) then
      SB.Append(TargetContent, LastIndex - 1, Length(TargetContent) - LastIndex + 1);

    TargetContent := SB.ToString;
  finally
    SB.Free;
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
