// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Context;

interface

uses

  System.Generics.Collections,
  System.SyncObjs;

type

  TStyleViolation = record
    Line: Integer;
    FileSpec: string;
    Message: string;
  end;

  TDptLintContext = class
  private
    class var FViolations: TList<TStyleViolation>;
    class var FLock: TObject;
    class procedure CheckInit;
  public
    class destructor Destroy;
    class procedure Clear;
    class procedure Add(ALine: Integer; const AFile, AMsg: string);
    class function Violations: TList<TStyleViolation>;
  end;

implementation

{ TDptLintContext }

class procedure TDptLintContext.CheckInit;
begin
  if not Assigned(FLock) then
  begin
    // Simple thread-safety for initialization (though normally class constructor handles this once)
    var LLock := TObject.Create;
    if TInterlocked.CompareExchange(Pointer(FLock), Pointer(LLock), nil) <> nil then
      LLock.Free
    else
      FViolations := TList<TStyleViolation>.Create;
  end;
end;

class procedure TDptLintContext.Add(ALine: Integer; const AFile, AMsg: string);
var
  V: TStyleViolation;
begin
  CheckInit;
  V.Line := ALine;
  V.FileSpec := AFile;
  V.Message := AMsg;
  TMonitor.Enter(FLock);
  try
    FViolations.Add(V);
  finally
    TMonitor.Exit(FLock);
  end;
end;

class procedure TDptLintContext.Clear;
begin
  CheckInit;
  TMonitor.Enter(FLock);
  try
    FViolations.Clear;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class destructor TDptLintContext.Destroy;
begin
  FViolations.Free;
  FLock.Free;
end;

class function TDptLintContext.Violations: TList<TStyleViolation>;
begin
  CheckInit;
  Result := FViolations;
end;

end.
