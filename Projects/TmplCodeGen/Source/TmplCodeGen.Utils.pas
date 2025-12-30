// ======================================================================
// Copyright (c) 2025 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit TmplCodeGen.Utils;

interface

uses

  System.RegularExpressions,
  System.SysUtils,

  TmplCodeGen.Common;

function CreateRegExpr(const APartialStmt: String; AStartRegion: Boolean; const AExpectedParam: String = ''): String;
function RemovePartialStmts(const AContent: String; const AStmts: Array of String): String;
function RemoveTrailingCommentLines(const AContent: String): String;
function RemoveMultipleCommentLines(const AContent: String): String;
function RemoveBigNewLineGaps(const AContent: String): String;

implementation

function CreateRegExpr(const APartialStmt: String; AStartRegion: Boolean; const AExpectedParam: String = ''): String;
var
  Region: String;
begin
  if AStartRegion then
    Region := 'REGION'
  else
    Region := 'ENDREGION';

  Result := ' *\{\$' + Region + '\s+\''(?P<Stmt>' + APartialStmt + ')\s+\/\s+';
  if AExpectedParam = '' then
    Result := Result + '(?P<Param>[^'']*)'
  else
    Result := Result + '(?P<Param>' + TRegEx.Escape(AExpectedParam) + ')';
  Result := Result + '''\s*\}';
end;

function RemovePartialStmts(const AContent: String; const AStmts: Array of String): String;
var
  Stmt: String;
begin
  Result := AContent;
  for Stmt in AStmts do
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
    '{ ======================================================================= }', []);
  Result := TRegEx.Replace(Result, '({ -{3,} }\s+){2,}',
    '{ ----------------------------------------------------------------------- }', []);
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

end.