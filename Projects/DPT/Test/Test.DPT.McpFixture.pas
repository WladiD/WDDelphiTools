// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.McpFixture;

// Shared test apparatus for the MCP-server end-to-end tests:
//
//   - TStringTextReader / TStringTextWriter feed JSON-RPC lines into
//     the server and capture its responses for assertion.
//   - WaitForOutput blocks until the writer has at least N lines,
//     covering the async pause-on-breakpoint timing window.
//   - TMcpEvalFixture bundles the standard "initialize ->
//     set_breakpoint -> ignore_exception -> continue ->
//     wait_until_paused" preamble plus the four owned objects
//     (debugger, MCP server, reader, writer) and their teardown
//     order, so each evaluate-style test becomes a single
//     constructor call followed by Fixture.Eval(name, type) calls.

interface

uses
  System.Classes,
  System.SyncObjs,

  DPT.Debugger,
  DPT.MCP.Server;

type
  TStringTextReader = class(TTextReader)
  public
    FLines: TStringList;
    FIndex: Integer;
    constructor Create(const AText: string);
    destructor Destroy; override;
    function ReadLine: string; override;
    function Peek: Integer; override;
    function GetEndOfStream: Boolean; override;
    procedure Close; override;
    function Read: Integer; override;
    function ReadBlock(var Buffer: TArray<Char>; Index, Count: Integer): Integer; override;
    function ReadToEnd: string; override;
    procedure Rewind; override;
  end;

  TStringTextWriter = class(TTextWriter)
  private
    FOutput: TStringList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write(const S: string); override;
    procedure WriteLine(const S: string); override;
    procedure Close; override;
    procedure Flush; override;
    function GetCount: Integer;
    function GetLine(AIndex: Integer): string;
    property Output: TStringList read FOutput;
  end;

/// <summary>
///   Blocks (with 50 ms polling, default 5 s budget) until the
///   writer has at least <paramref name="AMinCount"/> captured
///   lines. Asserts on timeout so a flaky async sequence surfaces
///   here instead of as a misleading "line out of range" further
///   down.
/// </summary>
procedure WaitForOutput(AWriter: TStringTextWriter; AMinCount: Integer;
  ATimeoutMs: Integer = 5000);

type
  /// <summary>
  ///   Bundles the standard "initialize -> set_breakpoint ->
  ///   ignore_exception -> continue -> wait_until_paused" preamble
  ///   used by every <c>evaluate</c>-style test, plus the four owned
  ///   objects (debugger, MCP server, input reader, output writer)
  ///   and their teardown order. A test becomes a single
  ///   <c>Fixture := TMcpEvalFixture.CreateAtBreakpoint(...)</c>
  ///   followed by <c>Fixture.Eval(name, type)</c> calls, freeing
  ///   ~30 lines of identical boilerplate per test.
  /// </summary>
  /// <remarks>
  ///   Kept intentionally narrow: tests that need a non-standard
  ///   preamble (no breakpoint, no map file, different exception
  ///   policy, ...) keep constructing things by hand so the test
  ///   body still reads as a self-contained scenario.
  /// </remarks>
  TMcpEvalFixture = class
  private
    FDebugger    : TDebugger;
    FServer      : TMcpServer;
    FInputReader : TStringTextReader;
    FOutputWriter: TStringTextWriter;
    FNextEvalId  : Integer;
    FNextLineIdx : Integer;
  public
    constructor CreateAtBreakpoint(const AExePath, AMapPath,
      AUnitName: String; ALine: Integer);
    destructor  Destroy; override;
    /// <summary>
    ///   Sends an <c>evaluate</c> tool-call for
    ///   <paramref name="AVarName"/> with the given
    ///   <paramref name="AType"/>, runs the server once, and returns
    ///   the response line for assertion. The JSON id and output-
    ///   line cursor advance automatically across calls.
    /// </summary>
    function    Eval(const AVarName, AType: String): String;
    /// <summary>
    ///   Like <see cref="Eval"/> but omits the <c>type</c> argument
    ///   entirely from the request, exercising the auto-detection
    ///   path: the server picks the formatter based on the
    ///   field's RSM-derived type information.
    /// </summary>
    function    EvalAuto(const AVarName: String): String;
    property Debugger    : TDebugger          read FDebugger;
    property Server      : TMcpServer         read FServer;
    property InputReader : TStringTextReader  read FInputReader;
    property OutputWriter: TStringTextWriter  read FOutputWriter;
  end;

implementation

uses
  System.SysUtils,
  DUnitX.TestFramework;

{ TStringTextReader }

constructor TStringTextReader.Create(const AText: string);
begin
  inherited Create;
  FLines := TStringList.Create;
  FLines.Text := AText;
  FIndex := 0;
end;

destructor TStringTextReader.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TStringTextReader.Close;
begin
end;

function TStringTextReader.GetEndOfStream: Boolean;
begin
  Result := FIndex >= FLines.Count;
end;

function TStringTextReader.Peek: Integer;
begin
  if FIndex < FLines.Count then Result := 1 else Result := -1;
end;

function TStringTextReader.Read: Integer;
begin
  Result := -1;
end;

function TStringTextReader.ReadBlock(var Buffer: TArray<Char>; Index, Count: Integer): Integer;
begin
  Result := 0;
end;

function TStringTextReader.ReadLine: string;
begin
  if FIndex < FLines.Count then
  begin
    Result := FLines[FIndex];
    Inc(FIndex);
  end
  else
    Result := '';
end;

function TStringTextReader.ReadToEnd: string;
begin
  Result := '';
end;

procedure TStringTextReader.Rewind;
begin
  FIndex := 0;
end;

{ TStringTextWriter }

constructor TStringTextWriter.Create;
begin
  inherited Create;
  FOutput := TStringList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TStringTextWriter.Destroy;
begin
  FLock.Free;
  FOutput.Free;
  inherited;
end;

procedure TStringTextWriter.Close;
begin
end;

procedure TStringTextWriter.Flush;
begin
end;

procedure TStringTextWriter.Write(const S: string);
begin
end;

procedure TStringTextWriter.WriteLine(const S: string);
begin
  FLock.Enter;
  try
    FOutput.Add(S);
  finally
    FLock.Leave;
  end;
end;

function TStringTextWriter.GetCount: Integer;
begin
  FLock.Enter;
  try
    Result := FOutput.Count;
  finally
    FLock.Leave;
  end;
end;

function TStringTextWriter.GetLine(AIndex: Integer): string;
begin
  FLock.Enter;
  try
    Result := FOutput[AIndex];
  finally
    FLock.Leave;
  end;
end;

procedure WaitForOutput(AWriter: TStringTextWriter; AMinCount: Integer; ATimeoutMs: Integer);
var
  Elapsed: Integer;
begin
  Elapsed := 0;
  while (AWriter.GetCount < AMinCount) and (Elapsed < ATimeoutMs) do
  begin
    Sleep(50);
    Inc(Elapsed, 50);
  end;
  Assert.IsTrue(AWriter.GetCount >= AMinCount,
    Format('Timeout waiting for output. Expected %d lines, got %d', [AMinCount, AWriter.GetCount]));
end;

{ TMcpEvalFixture }

constructor TMcpEvalFixture.CreateAtBreakpoint(const AExePath, AMapPath,
  AUnitName: String; ALine: Integer);
begin
  inherited Create;
  FInputReader := TStringTextReader.Create(
    '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05"}}' + sLineBreak +
    Format('{"jsonrpc": "2.0", "id": 2, "method": "tools/call", ' +
           '"params": {"name": "set_breakpoint", "arguments": ' +
           '{"unit": "%s", "line": %d}}}', [AUnitName, ALine]) + sLineBreak +
    '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "ignore_exception", "arguments": {"class_name": "Exception"}}}' + sLineBreak +
    '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "continue", "arguments": {}}}');

  FDebugger := TDebugger.Create;
  FDebugger.LoadMapFile(AMapPath);
  FDebugger.LoadDebugInfoFromExe(AExePath);
  TDebuggerThread.Create(FDebugger, AExePath);

  FOutputWriter := TStringTextWriter.Create;
  FServer := TMcpServer.Create(FDebugger, FInputReader, FOutputWriter);
  FServer.RunOnce; // init
  FServer.RunOnce; // set_breakpoint
  FServer.RunOnce; // ignore_exception
  FServer.RunOnce; // continue
  WaitForOutput(FOutputWriter, 6);

  // Init / set_bp / ignore_exc / continue / pause-on-bp produced
  // lines 1..5; the first evaluate response will land on line 6
  // and have JSON id 5. Tests use Eval() rather than touching
  // these cursors directly.
  FNextEvalId  := 5;
  FNextLineIdx := 6;
end;

destructor TMcpEvalFixture.Destroy;
begin
  // Teardown order matters: the server holds references to reader /
  // writer, the debugger owns the target process. Free in the
  // reverse of construction.
  FServer.Free;
  FInputReader.Free;
  FOutputWriter.Free;
  FDebugger.Free;
  inherited;
end;

function TMcpEvalFixture.Eval(const AVarName, AType: String): String;
begin
  FInputReader.FLines.Add(Format(
    '{"jsonrpc": "2.0", "id": %d, "method": "tools/call", ' +
    '"params": {"name": "evaluate", "arguments": ' +
    '{"name": "%s", "type": "%s"}}}', [FNextEvalId, AVarName, AType]));
  FServer.RunOnce;
  Result := FOutputWriter.GetLine(FNextLineIdx);
  Inc(FNextEvalId);
  Inc(FNextLineIdx);
end;

function TMcpEvalFixture.EvalAuto(const AVarName: String): String;
begin
  FInputReader.FLines.Add(Format(
    '{"jsonrpc": "2.0", "id": %d, "method": "tools/call", ' +
    '"params": {"name": "evaluate", "arguments": ' +
    '{"name": "%s"}}}', [FNextEvalId, AVarName]));
  FServer.RunOnce;
  Result := FOutputWriter.GetLine(FNextLineIdx);
  Inc(FNextEvalId);
  Inc(FNextLineIdx);
end;

end.
