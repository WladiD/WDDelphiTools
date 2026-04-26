unit Test.DPT.Dcu.Reader;

interface

uses

  DUnitX.TestFramework,

  System.SysUtils,

  DPT.Dcu.Reader;

type

  [TestFixture]
  TTestDcuReader = class
  public
    [Test] procedure ConstructFromBytes;
    [Test] procedure ConstructFromMissingFileYieldsEmpty;
    [Test] procedure ReadByteAdvancesPosition;
    [Test] procedure ReadByteAtEofReturnsZero;
    [Test] procedure ReadWordIsLittleEndian;
    [Test] procedure ReadDWordIsLittleEndian;
    [Test] procedure ReadShortStringConsumesLengthAndPayload;
    [Test] procedure ReadShortStringSurvivesBufferOverflow;
    [Test] procedure TryReadShortStringRejectsZeroLength;
    [Test] procedure TryReadShortStringRejectsNonPrintable;
    [Test] procedure TryReadShortStringAcceptsValidName;
    [Test] procedure IndexOfFindsPattern;
    [Test] procedure IndexOfReturnsMinusOneOnMiss;
    [Test] procedure IndexOfRespectsStartOffset;
    [Test] procedure HexPreviewProducesExpectedFormat;
  end;

implementation

function MakeBytes(const AValues: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(AValues));
  for I := 0 to High(AValues) do
    Result[I] := AValues[I];
end;

{ TTestDcuReader }

procedure TTestDcuReader.ConstructFromBytes;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([1, 2, 3]));
  try
    Assert.AreEqual(3, Reader.Size);
    Assert.AreEqual(0, Reader.Position);
    Assert.IsFalse(Reader.Eof);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.ConstructFromMissingFileYieldsEmpty;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create('C:\Definitely\Not\A\Real\File.dcu');
  try
    Assert.AreEqual(0, Reader.Size);
    Assert.IsTrue(Reader.Eof);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.ReadByteAdvancesPosition;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([$AB, $CD]));
  try
    Assert.AreEqual(Byte($AB), Reader.ReadByte);
    Assert.AreEqual(1, Reader.Position);
    Assert.AreEqual(Byte($CD), Reader.ReadByte);
    Assert.IsTrue(Reader.Eof);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.ReadByteAtEofReturnsZero;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([]));
  try
    Assert.AreEqual(Byte(0), Reader.ReadByte);
    Assert.IsTrue(Reader.Eof);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.ReadWordIsLittleEndian;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([$34, $12]));
  try
    Assert.AreEqual(Word($1234), Reader.ReadWordLE);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.ReadDWordIsLittleEndian;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([$78, $56, $34, $12]));
  try
    Assert.AreEqual(UInt32($12345678), Reader.ReadDWordLE);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.ReadShortStringConsumesLengthAndPayload;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([6,
    Ord('S'), Ord('y'), Ord('s'), Ord('t'), Ord('e'), Ord('m')]));
  try
    Assert.AreEqual('System', Reader.ReadShortStringUtf8);
    Assert.IsTrue(Reader.Eof);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.ReadShortStringSurvivesBufferOverflow;
var
  Reader: TDcuReader;
begin
  // Length byte announces 100 chars, but only 2 follow
  Reader := TDcuReader.Create(MakeBytes([100, Ord('a'), Ord('b')]));
  try
    Assert.AreEqual('', Reader.ReadShortStringUtf8);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.TryReadShortStringRejectsZeroLength;
var
  Consumed: Integer;
  Reader  : TDcuReader;
  S       : string;
begin
  Reader := TDcuReader.Create(MakeBytes([0, 5, Ord('h'), Ord('e'), Ord('l'), Ord('l'), Ord('o')]));
  try
    Assert.IsFalse(Reader.TryReadShortStringAt(0, S, Consumed));
    Assert.AreEqual(0, Consumed);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.TryReadShortStringRejectsNonPrintable;
var
  Consumed: Integer;
  Reader  : TDcuReader;
  S       : string;
begin
  Reader := TDcuReader.Create(MakeBytes([3, Ord('a'), $01, Ord('b')]));
  try
    Assert.IsFalse(Reader.TryReadShortStringAt(0, S, Consumed));
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.TryReadShortStringAcceptsValidName;
var
  Consumed: Integer;
  Reader  : TDcuReader;
  S       : string;
begin
  Reader := TDcuReader.Create(MakeBytes([4, Ord('U'), Ord('n'), Ord('i'), Ord('t')]));
  try
    Assert.IsTrue(Reader.TryReadShortStringAt(0, S, Consumed));
    Assert.AreEqual('Unit', S);
    Assert.AreEqual(5, Consumed);
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.IndexOfFindsPattern;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([1, 2, 3, $00, $64, 4, 5]));
  try
    Assert.AreEqual(3, Reader.IndexOf([$00, $64]));
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.IndexOfReturnsMinusOneOnMiss;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([1, 2, 3]));
  try
    Assert.AreEqual(-1, Reader.IndexOf([$FF, $FF]));
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.IndexOfRespectsStartOffset;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([$00, $64, 1, $00, $64, 2]));
  try
    Assert.AreEqual(0, Reader.IndexOf([$00, $64]));
    Assert.AreEqual(3, Reader.IndexOf([$00, $64], 1));
  finally
    Reader.Free;
  end;
end;

procedure TTestDcuReader.HexPreviewProducesExpectedFormat;
var
  Reader: TDcuReader;
begin
  Reader := TDcuReader.Create(MakeBytes([$4D, $03, $00, $24]));
  try
    Assert.AreEqual('4D 03 00 24', Reader.HexPreview(0, 4));
    Assert.AreEqual('03 00', Reader.HexPreview(1, 2));
    Assert.AreEqual('', Reader.HexPreview(10, 4));
  finally
    Reader.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDcuReader);

end.
