// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Logger;

interface

uses

  System.SyncObjs,
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

  /// <summary>
  ///   Thread-safe capturing logger. Accumulates every logged line into an
  ///   internal buffer instead of writing to the console. Used to drive the
  ///   build pipeline from inside the MCP server, whose stdout carries the
  ///   JSON-RPC stream and must never receive build chatter.
  /// </summary>
  TCaptureLogger = class(TInterfacedObject, ILogger)
  private
    FLines: TStringBuilder;
    FLock : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const Text: String);
    procedure LogFmt(const FormatStr: String; const Args: array of const);
    /// <summary>Returns the full captured text accumulated so far.</summary>
    function Text: String;
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

{ TCaptureLogger }

constructor TCaptureLogger.Create;
begin
  inherited Create;
  FLines := TStringBuilder.Create;
  FLock := TCriticalSection.Create;
end;

destructor TCaptureLogger.Destroy;
begin
  FLines.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TCaptureLogger.Log(const Text: String);
begin
  FLock.Enter;
  try
    FLines.AppendLine(Text);
  finally
    FLock.Leave;
  end;
end;

procedure TCaptureLogger.LogFmt(const FormatStr: String; const Args: array of const);
begin
  Log(Format(FormatStr, Args));
end;

function TCaptureLogger.Text: String;
begin
  FLock.Enter;
  try
    Result := FLines.ToString;
  finally
    FLock.Leave;
  end;
end;

end.
