// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Reader;

interface

uses

  System.Classes,
  System.SysUtils;

type

  /// <summary>
  ///   Minimal little-endian binary cursor over an in-memory byte buffer.
  ///   Built specifically for iteration-1 DCU header probing: random
  ///   access, length-prefixed short strings, simple pattern search.
  ///   Out-of-range reads return zero / empty without raising; callers
  ///   are expected to consult <c>Eof</c> or compare positions.
  /// </summary>
  TDcuReader = class
  private
    FContent : TBytes;
    FPosition: Integer;
    function GetByteAt(AIndex: Integer): Byte;
    function GetSize: Integer;
  public
    constructor Create(const AContent: TBytes); overload;
    constructor Create(const AFileName: string); overload;

    procedure Seek(APosition: Integer);
    function  Eof: Boolean;

    function  ReadByte: Byte;
    function  ReadWordLE: Word;
    function  ReadDWordLE: UInt32;
    function  ReadShortStringUtf8: string;

    /// <summary>
    ///   Tries to read a length-prefixed UTF-8 short string starting at
    ///   <c>AOffset</c>. Returns False when the candidate length is zero,
    ///   exceeds the buffer, or the content contains bytes that are not
    ///   plausible identifier characters (non-printable below space, or
    ///   bytes with the high bit set in the ASCII range we expect).
    /// </summary>
    function TryReadShortStringAt(AOffset: Integer; out AString: string;
      out ABytesConsumed: Integer): Boolean;

    /// <summary>
    ///   Searches for the first occurrence of the given byte sequence
    ///   starting at <c>AStart</c>. Returns -1 when not found.
    /// </summary>
    function IndexOf(const APattern: array of Byte; AStart: Integer = 0): Integer;

    /// <summary>
    ///   Returns up to <c>AByteCount</c> bytes formatted as space-separated
    ///   uppercase hex pairs starting at <c>AOffset</c>.
    /// </summary>
    function HexPreview(AOffset, AByteCount: Integer): string;

    property Content: TBytes read FContent;
    property Position: Integer read FPosition;
    property Size: Integer read GetSize;
    property Bytes[AIndex: Integer]: Byte read GetByteAt; default;
  end;

implementation

{ TDcuReader }

constructor TDcuReader.Create(const AContent: TBytes);
begin
  inherited Create;
  FContent := AContent;
  FPosition := 0;
end;

constructor TDcuReader.Create(const AFileName: string);
var
  Stream: TFileStream;
begin
  inherited Create;
  FPosition := 0;
  if not FileExists(AFileName) then
    Exit;
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(FContent, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(FContent[0], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function TDcuReader.GetSize: Integer;
begin
  Result := Length(FContent);
end;

function TDcuReader.GetByteAt(AIndex: Integer): Byte;
begin
  if (AIndex < 0) or (AIndex >= Length(FContent)) then
    Result := 0
  else
    Result := FContent[AIndex];
end;

procedure TDcuReader.Seek(APosition: Integer);
begin
  FPosition := APosition;
end;

function TDcuReader.Eof: Boolean;
begin
  Result := FPosition >= Length(FContent);
end;

function TDcuReader.ReadByte: Byte;
begin
  if Eof then
    Exit(0);
  Result := FContent[FPosition];
  Inc(FPosition);
end;

function TDcuReader.ReadWordLE: Word;
begin
  if FPosition + 2 > Length(FContent) then
  begin
    FPosition := Length(FContent);
    Exit(0);
  end;
  Result := FContent[FPosition] or (FContent[FPosition + 1] shl 8);
  Inc(FPosition, 2);
end;

function TDcuReader.ReadDWordLE: UInt32;
begin
  if FPosition + 4 > Length(FContent) then
  begin
    FPosition := Length(FContent);
    Exit(0);
  end;
  Result :=
    UInt32(FContent[FPosition])           or
    (UInt32(FContent[FPosition + 1]) shl  8) or
    (UInt32(FContent[FPosition + 2]) shl 16) or
    (UInt32(FContent[FPosition + 3]) shl 24);
  Inc(FPosition, 4);
end;

function TDcuReader.ReadShortStringUtf8: string;
var
  Buf : TBytes;
  Len : Integer;
begin
  Result := '';
  if Eof then
    Exit;
  Len := FContent[FPosition];
  if FPosition + 1 + Len > Length(FContent) then
  begin
    Inc(FPosition);
    Exit;
  end;
  SetLength(Buf, Len);
  if Len > 0 then
    Move(FContent[FPosition + 1], Buf[0], Len);
  Result := TEncoding.UTF8.GetString(Buf);
  Inc(FPosition, 1 + Len);
end;

function TDcuReader.TryReadShortStringAt(AOffset: Integer;
  out AString: string; out ABytesConsumed: Integer): Boolean;
var
  Buf      : TBytes;
  I        : Integer;
  Len      : Integer;
  PrintAble: Boolean;
begin
  Result := False;
  AString := '';
  ABytesConsumed := 0;
  if (AOffset < 0) or (AOffset >= Length(FContent)) then
    Exit;
  Len := FContent[AOffset];
  if Len = 0 then
    Exit;
  if AOffset + 1 + Len > Length(FContent) then
    Exit;
  PrintAble := True;
  for I := 0 to Len - 1 do
  begin
    var B := FContent[AOffset + 1 + I];
    // Restrict to ASCII printable. Source-file references and unit names
    // in modern DCUs are ASCII; this keeps the heuristic cheap and avoids
    // mis-classifying random binary as "valid string" data.
    if (B < 32) or (B > 126) then
    begin
      PrintAble := False;
      Break;
    end;
  end;
  if not PrintAble then
    Exit;
  SetLength(Buf, Len);
  Move(FContent[AOffset + 1], Buf[0], Len);
  try
    AString := TEncoding.UTF8.GetString(Buf);
  except
    AString := '';
    Exit;
  end;
  ABytesConsumed := 1 + Len;
  Result := True;
end;

function TDcuReader.IndexOf(const APattern: array of Byte; AStart: Integer): Integer;
var
  I        : Integer;
  J        : Integer;
  PatLen   : Integer;
  Mismatch : Boolean;
begin
  Result := -1;
  PatLen := Length(APattern);
  if PatLen = 0 then
    Exit;
  if AStart < 0 then
    AStart := 0;
  for I := AStart to Length(FContent) - PatLen do
  begin
    Mismatch := False;
    for J := 0 to PatLen - 1 do
      if FContent[I + J] <> APattern[J] then
      begin
        Mismatch := True;
        Break;
      end;
    if not Mismatch then
      Exit(I);
  end;
end;

function TDcuReader.HexPreview(AOffset, AByteCount: Integer): string;
var
  I  : Integer;
  Sb : TStringBuilder;
  Cnt: Integer;
begin
  if AOffset < 0 then
    AOffset := 0;
  Cnt := AByteCount;
  if AOffset + Cnt > Length(FContent) then
    Cnt := Length(FContent) - AOffset;
  if Cnt <= 0 then
    Exit('');
  Sb := TStringBuilder.Create(Cnt * 3);
  try
    for I := 0 to Cnt - 1 do
    begin
      if I > 0 then Sb.Append(' ');
      Sb.Append(IntToHex(FContent[AOffset + I], 2));
    end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

end.
