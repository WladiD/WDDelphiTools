// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Context;

interface

uses

  System.SyncObjs,

  mormot.core.collections;

type

  TStyleViolation = record
    Line: Integer;
    FileSpec: string;
    Message: string;
  end;

  TDptLintContext = class
  private
    class var FLock      : TObject;
    class var FViolations: IList<TStyleViolation>;
    class var FWarnings  : IList<String>;
    class procedure CheckInit;
  public
    class destructor Destroy;
    class procedure Clear;
    class procedure Add(ALine: Integer; const AFile, AMsg: String);
    class procedure AddWarning(const AMsg: String);
    class function Violations: IList<TStyleViolation>;
    class function Warnings: IList<String>;
  end;

implementation

{ TDptLintContext }

class destructor TDptLintContext.Destroy;
begin
  FLock.Free;
end;

class procedure TDptLintContext.CheckInit;
begin
  if not Assigned(FLock) then
  begin
    // Simple thread-safety for initialization (though normally class constructor handles this once)
    var LLock := TObject.Create;
    if TInterlocked.CompareExchange(Pointer(FLock), Pointer(LLock), nil) <> nil then
      LLock.Free
    else
    begin
      FViolations := Collections.NewPlainList<TStyleViolation>;
      FWarnings := Collections.NewList<string>;
    end;
  end;
end;

class procedure TDptLintContext.Add(ALine: Integer; const AFile, AMsg: String);
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

class procedure TDptLintContext.AddWarning(const AMsg: String);
begin
  CheckInit;
  TMonitor.Enter(FLock);
  try
    FWarnings.Add(AMsg);
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
    FWarnings.Clear;
  finally
    TMonitor.Exit(FLock);
  end;
end;

class function TDptLintContext.Violations: IList<TStyleViolation>;
begin
  CheckInit;
  Result := FViolations;
end;

class function TDptLintContext.Warnings: IList<String>;
begin
  CheckInit;
  Result := FWarnings;
end;

end.
