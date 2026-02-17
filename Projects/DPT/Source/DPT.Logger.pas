// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Logger;

interface

uses

  System.Classes,
  System.SysUtils;

type

  ILogger = interface
    ['{09D81280-0B03-4579-8AAE-81D13EFF2DC1}']
    procedure Log(const Text: String);
    procedure LogFmt(const FormatStr: String; const Args: array of const);
  end;

  TConsoleLogger = class(TInterfacedObject, ILogger)
  public
    procedure Log(const Text: String);
    procedure LogFmt(const FormatStr: String; const Args: array of const);
  end;

  TNullLogger = class(TInterfacedObject, ILogger)
  public
    procedure Log(const Text: String);
    procedure LogFmt(const FormatStr: String; const Args: array of const);
  end;

implementation

{ TConsoleLogger }

procedure TConsoleLogger.Log(const Text: String);
begin
  Writeln(Text);
end;

procedure TConsoleLogger.LogFmt(const FormatStr: String; const Args: array of const);
begin
  Writeln(Format(FormatStr, Args));
end;

{ TNullLogger }

procedure TNullLogger.Log(const Text: String);
begin
  // Do nothing
end;

procedure TNullLogger.LogFmt(const FormatStr: String; const Args: array of const);
begin
  // Do nothing
end;

end.
