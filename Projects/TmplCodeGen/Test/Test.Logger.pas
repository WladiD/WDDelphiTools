// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.Logger;

interface

uses
  System.Classes,
  TmplCodeGen.Logger;

type

  TTestLogger = class(TInterfacedObject, ILogger)
  public
    LogMessages: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Log(const AMessage: String);
  end;

implementation

{ TTestLogger }

constructor TTestLogger.Create;
begin
  inherited Create;
  LogMessages := TStringList.Create;
end;

destructor TTestLogger.Destroy;
begin
  LogMessages.Free;
  inherited;
end;

procedure TTestLogger.Log(const AMessage: String);
begin
  LogMessages.Add(AMessage);
end;

end.
