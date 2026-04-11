unit Test.DPT.MapFileParser;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  DPT.MapFileParser;

type
  [TestFixture]
  TTestMapFileParser = class
  private
    function MapPath32: string;
    function MapPath64: string;
  public
    // 32-bit map file tests
    [Test]
    procedure Parse32;
    [Test]
    procedure ProcNameFromAddr32;
    [Test]
    procedure ModuleNameFromAddr32;
    [Test]
    procedure LineNumberFromAddr32;
    [Test]
    procedure VAFromUnitAndProcName32;
    [Test]
    procedure LineNumberIteration32;

    // 64-bit map file tests
    [Test]
    procedure Parse64;
    [Test]
    procedure ProcNameFromAddr64;
    [Test]
    procedure ModuleNameFromAddr64;
    [Test]
    procedure LineNumberFromAddr64;
    [Test]
    procedure VAFromUnitAndProcName64;
    [Test]
    procedure LineNumberIteration64;

    // Edge cases
    [Test]
    procedure NonExistentFile;
    [Test]
    procedure AddressNotFound;
  end;

implementation

function TTestMapFileParser.MapPath32: string;
begin
  Result := ExpandFileName('Projects\DPT\Test\DebugTarget.map');
  if not FileExists(Result) then
    Result := ExpandFileName('DebugTarget.map');
end;

function TTestMapFileParser.MapPath64: string;
begin
  Result := ExpandFileName('Projects\DPT\Test\Win64\DebugTarget.map');
  if not FileExists(Result) then
    Result := ExpandFileName('Win64\DebugTarget.map');
end;

// === 32-bit tests ===

procedure TTestMapFileParser.Parse32;
var
  Parser: TMapFileParser;
begin
  Parser := TMapFileParser.Create(MapPath32);
  try
    Assert.IsTrue(Parser.LineNumbersCnt > 0, 'Should parse line numbers');
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.ProcNameFromAddr32;
var
  Parser: TMapFileParser;
  VA: UInt64;
  Name: string;
  Offset: Integer;
begin
  Parser := TMapFileParser.Create(MapPath32);
  try
    VA := Parser.VAFromUnitAndProcName('DebugTarget', 'DeepProcedure');
    Assert.IsTrue(VA > 0, 'DeepProcedure VA should be > 0');
    // Also check module name works (verifies segments were parsed)
    Assert.AreNotEqual('', Parser.ModuleNameFromAddr(VA),
      Format('ModuleNameFromAddr($%x) should find module', [VA]));
    Name := Parser.ProcNameFromAddr(VA, Offset);
    Assert.AreEqual('DebugTarget.DeepProcedure', Name,
      Format('ProcNameFromAddr($%x) failed, offset=%d', [VA, Offset]));
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.ModuleNameFromAddr32;
var
  Parser: TMapFileParser;
  VA: UInt64;
begin
  Parser := TMapFileParser.Create(MapPath32);
  try
    VA := Parser.VAFromUnitAndProcName('DebugTarget', 'DeepProcedure');
    Assert.AreEqual('DebugTarget', Parser.ModuleNameFromAddr(VA));
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.LineNumberFromAddr32;
var
  Parser: TMapFileParser;
  Found: Boolean;
begin
  Parser := TMapFileParser.Create(MapPath32);
  try
    Found := False;
    for var I := 0 to Parser.LineNumbersCnt - 1 do
    begin
      var LI := Parser.LineNumberByIndex[I];
      if (LI.LineNumber = 13) and (LI.UnitName = 'DebugTarget') then
      begin
        Assert.AreEqual(13, Parser.LineNumberFromAddr(LI.VA));
        Found := True;
        Break;
      end;
    end;
    Assert.IsTrue(Found, 'Line 13 should exist in map');
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.VAFromUnitAndProcName32;
var
  Parser: TMapFileParser;
begin
  Parser := TMapFileParser.Create(MapPath32);
  try
    Assert.IsTrue(Parser.VAFromUnitAndProcName('DebugTarget', 'DeepProcedure') > 0,
      'DeepProcedure should be found');
    Assert.IsTrue(Parser.VAFromUnitAndProcName('DebugTarget', 'TargetProcedure') > 0,
      'TargetProcedure should be found');
    Assert.AreEqual(UInt64(0), Parser.VAFromUnitAndProcName('DebugTarget', 'NonExistent'),
      'NonExistent should not be found');
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.LineNumberIteration32;
var
  Parser: TMapFileParser;
  FoundDebugTarget: Boolean;
begin
  Parser := TMapFileParser.Create(MapPath32);
  try
    FoundDebugTarget := False;
    for var I := 0 to Parser.LineNumbersCnt - 1 do
    begin
      var LI := Parser.LineNumberByIndex[I];
      if LI.UnitName = 'DebugTarget' then
      begin
        FoundDebugTarget := True;
        Assert.IsTrue(LI.VA > 0, 'VA should be > 0');
        Assert.IsTrue(LI.LineNumber > 0, 'LineNumber should be > 0');
        Break;
      end;
    end;
    Assert.IsTrue(FoundDebugTarget, 'DebugTarget unit should be in line numbers');
  finally
    Parser.Free;
  end;
end;

// === 64-bit tests ===

procedure TTestMapFileParser.Parse64;
var
  Parser: TMapFileParser;
begin
  Parser := TMapFileParser.Create(MapPath64);
  try
    Assert.IsTrue(Parser.LineNumbersCnt > 0, 'Should parse line numbers from 64-bit map');
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.ProcNameFromAddr64;
var
  Parser: TMapFileParser;
  VA: UInt64;
  Name: string;
begin
  Parser := TMapFileParser.Create(MapPath64);
  try
    VA := Parser.VAFromUnitAndProcName('DebugTarget', 'DeepProcedure');
    Assert.IsTrue(VA > 0, 'DeepProcedure VA should be > 0 in 64-bit map');
    Name := Parser.ProcNameFromAddr(VA);
    Assert.AreEqual('DebugTarget.DeepProcedure', Name);
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.ModuleNameFromAddr64;
var
  Parser: TMapFileParser;
  VA: UInt64;
begin
  Parser := TMapFileParser.Create(MapPath64);
  try
    VA := Parser.VAFromUnitAndProcName('DebugTarget', 'DeepProcedure');
    Assert.AreEqual('DebugTarget', Parser.ModuleNameFromAddr(VA));
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.LineNumberFromAddr64;
var
  Parser: TMapFileParser;
  Found: Boolean;
begin
  Parser := TMapFileParser.Create(MapPath64);
  try
    Found := False;
    for var I := 0 to Parser.LineNumbersCnt - 1 do
    begin
      var LI := Parser.LineNumberByIndex[I];
      if (LI.LineNumber = 13) and (LI.UnitName = 'DebugTarget') then
      begin
        Assert.AreEqual(13, Parser.LineNumberFromAddr(LI.VA));
        Found := True;
        Break;
      end;
    end;
    Assert.IsTrue(Found, 'Line 13 should exist in 64-bit map');
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.VAFromUnitAndProcName64;
var
  Parser: TMapFileParser;
begin
  Parser := TMapFileParser.Create(MapPath64);
  try
    Assert.IsTrue(Parser.VAFromUnitAndProcName('DebugTarget', 'DeepProcedure') > 0,
      'DeepProcedure should be found in 64-bit map');
    Assert.IsTrue(Parser.VAFromUnitAndProcName('DebugTarget', 'TargetProcedure') > 0,
      'TargetProcedure should be found in 64-bit map');
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.LineNumberIteration64;
var
  Parser: TMapFileParser;
  FoundDebugTarget: Boolean;
begin
  Parser := TMapFileParser.Create(MapPath64);
  try
    FoundDebugTarget := False;
    for var I := 0 to Parser.LineNumbersCnt - 1 do
    begin
      var LI := Parser.LineNumberByIndex[I];
      if LI.UnitName = 'DebugTarget' then
      begin
        FoundDebugTarget := True;
        Assert.IsTrue(LI.VA > 0, 'VA should be > 0');
        Assert.IsTrue(LI.LineNumber > 0, 'LineNumber should be > 0');
        Break;
      end;
    end;
    Assert.IsTrue(FoundDebugTarget, 'DebugTarget unit should be in 64-bit line numbers');
  finally
    Parser.Free;
  end;
end;

// === Edge cases ===

procedure TTestMapFileParser.NonExistentFile;
var
  Parser: TMapFileParser;
begin
  Parser := TMapFileParser.Create('C:\NonExistent\File.map');
  try
    Assert.AreEqual(0, Parser.LineNumbersCnt, 'Should have no line numbers');
    Assert.AreEqual('', Parser.ProcNameFromAddr(0));
  finally
    Parser.Free;
  end;
end;

procedure TTestMapFileParser.AddressNotFound;
var
  Parser: TMapFileParser;
begin
  Parser := TMapFileParser.Create(MapPath32);
  try
    Assert.AreEqual('', Parser.ProcNameFromAddr($FFFFFFFF));
    Assert.AreEqual('', Parser.ModuleNameFromAddr($FFFFFFFF));
    Assert.AreEqual(0, Parser.LineNumberFromAddr($FFFFFFFF));
  finally
    Parser.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestMapFileParser);

end.
