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
    class var FWarnings: TList<string>;
    class var FLock: TObject;
    class procedure CheckInit;
  public
    class destructor Destroy;
    class procedure Clear;
    class procedure Add(ALine: Integer; const AFile, AMsg: string);
    class procedure AddWarning(const AMsg: string);
    class function Violations: TList<TStyleViolation>;
    class function Warnings: TList<string>;
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
    begin
      FViolations := TList<TStyleViolation>.Create;
      FWarnings := TList<string>.Create;
    end;
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

class procedure TDptLintContext.AddWarning(const AMsg: string);
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

class destructor TDptLintContext.Destroy;
begin
  FViolations.Free;
  FWarnings.Free;
  FLock.Free;
end;

class function TDptLintContext.Violations: TList<TStyleViolation>;
begin
  CheckInit;
  Result := FViolations;
end;

class function TDptLintContext.Warnings: TList<string>;
begin
  CheckInit;
  Result := FWarnings;
end;

end.