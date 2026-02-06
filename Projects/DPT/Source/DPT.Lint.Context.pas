// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Context;

interface

uses

  System.Generics.Collections;

type

  TStyleViolation = record
    Line: Integer;
    FileSpec: string;
    Message: string;
  end;

  TDptLintContext = class
  private
    class var FViolations: TList<TStyleViolation>;
  public
    class destructor Destroy;
    class procedure Clear;
    class procedure Add(ALine: Integer; const AFile, AMsg: string);
    class function Violations: TList<TStyleViolation>;
  end;

implementation

{ TDptLintContext }

class procedure TDptLintContext.Add(ALine: Integer; const AFile, AMsg: string);
var
  V: TStyleViolation;
begin
  V.Line := ALine;
  V.FileSpec := AFile;
  V.Message := AMsg;
  Violations.Add(V);
end;

class procedure TDptLintContext.Clear;
begin
  Violations.Clear;
end;

class destructor TDptLintContext.Destroy;
begin
  FViolations.Free;
end;

class function TDptLintContext.Violations: TList<TStyleViolation>;
begin
  if not Assigned(FViolations) then
    FViolations := TList<TStyleViolation>.Create;
  Result := FViolations;
end;

end.
