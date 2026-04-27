// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Index.Json;

interface

uses

  System.Classes,
  System.SysUtils,

  DPT.Dcu.Index;

type

  /// <summary>
  ///   Streaming serializer that writes a TDcuIndex as JSON without
  ///   building the whole tree in memory. Per-DCU rows are flushed
  ///   incrementally so the maximum heap usage stays bounded even on
  ///   trees with thousands of DCUs.
  /// </summary>
  TDcuIndexJsonWriter = class
  public
    class procedure WriteToStream(const AIndex: TDcuIndex; AStream: TStream);
    class procedure WriteToFile(const AIndex: TDcuIndex; const AFilePath: string);
    class function  WriteToString(const AIndex: TDcuIndex): string;
  end;

  /// <summary>
  ///   Loads a previously written index from disk. Uses System.JSON
  ///   under the hood, which holds the full tree in memory while
  ///   parsing - acceptable for query workloads which typically open
  ///   the file once and then run many lookups against the in-memory
  ///   structure.
  /// </summary>
  TDcuIndexJsonReader = class
  public
    class function LoadFromFile(const AFilePath: string): TDcuIndex;
    class function LoadFromString(const AJson: string): TDcuIndex;
  end;

implementation

uses

  System.DateUtils,
  System.IOUtils,
  System.JSON,

  mormot.core.collections,

  DPT.Dcu.Types;

function JsonEscape(const S: string): string;
var
  Sb: TStringBuilder;
  Ch: Char;
begin
  Sb := TStringBuilder.Create(Length(S) + 2);
  try
    for Ch in S do
      case Ch of
        '"' : Sb.Append('\"');
        '\' : Sb.Append('\\');
        #8  : Sb.Append('\b');
        #9  : Sb.Append('\t');
        #10 : Sb.Append('\n');
        #12 : Sb.Append('\f');
        #13 : Sb.Append('\r');
      else
        if Ord(Ch) < 32 then
          Sb.Append(System.SysUtils.Format('\u%.4x', [Ord(Ch)]))
        else
          Sb.Append(Ch);
      end;
    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function JsonStr(const S: string): string;
begin
  Result := '"' + JsonEscape(S) + '"';
end;

procedure WriteUtf8(AStream: TStream; const S: string);
var
  Bytes: TBytes;
begin
  if S = '' then
    Exit;
  Bytes := TEncoding.UTF8.GetBytes(S);
  AStream.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure WriteStringArray(AStream: TStream; AList: IList<string>);
var
  First: Boolean;
  Item : string;
begin
  WriteUtf8(AStream, '[');
  First := True;
  if AList <> nil then
    for Item in AList do
    begin
      if not First then WriteUtf8(AStream, ', ');
      WriteUtf8(AStream, JsonStr(Item));
      First := False;
    end;
  WriteUtf8(AStream, ']');
end;

procedure WriteCounters(AStream: TStream; const AIndex: TDcuIndex);
var
  CompilerHits: array[TDcuKnownCompiler] of Integer;
  Entry       : TDcuIndexEntry;
  First       : Boolean;
  K           : TDcuKnownCompiler;
  P           : TDcuPlatform;
  PlatformHits: array[TDcuPlatform] of Integer;
begin
  for K := Low(TDcuKnownCompiler) to High(TDcuKnownCompiler) do
    CompilerHits[K] := 0;
  for P := Low(TDcuPlatform) to High(TDcuPlatform) do
    PlatformHits[P] := 0;
  for Entry in AIndex.Units do
  begin
    Inc(CompilerHits[Entry.Compiler]);
    Inc(PlatformHits[Entry.Platform]);
  end;

  WriteUtf8(AStream, '  "perCompiler": {');
  First := True;
  for K := Low(TDcuKnownCompiler) to High(TDcuKnownCompiler) do
    if CompilerHits[K] > 0 then
    begin
      if not First then WriteUtf8(AStream, ', ');
      WriteUtf8(AStream, JsonStr(DcuKnownCompilerName[K])
        + ': ' + IntToStr(CompilerHits[K]));
      First := False;
    end;
  WriteUtf8(AStream, '},'#10);

  WriteUtf8(AStream, '  "perPlatform": {');
  First := True;
  for P := Low(TDcuPlatform) to High(TDcuPlatform) do
    if PlatformHits[P] > 0 then
    begin
      if not First then WriteUtf8(AStream, ', ');
      WriteUtf8(AStream, JsonStr(DcuPlatformName[P])
        + ': ' + IntToStr(PlatformHits[P]));
      First := False;
    end;
  WriteUtf8(AStream, '},'#10);
end;

procedure WriteUnitEntry(AStream: TStream; const AEntry: TDcuIndexEntry;
  AIsLast: Boolean);
begin
  WriteUtf8(AStream, '    {'#10);
  WriteUtf8(AStream, '      "unit": ' + JsonStr(AEntry.UnitName) + ','#10);
  WriteUtf8(AStream, '      "file": ' + JsonStr(AEntry.FilePath) + ','#10);
  WriteUtf8(AStream, '      "size": ' + IntToStr(AEntry.FileSize) + ','#10);
  WriteUtf8(AStream, '      "magic": ' + JsonStr(AEntry.Magic) + ','#10);
  WriteUtf8(AStream, '      "compiler": '
    + JsonStr(DcuKnownCompilerName[AEntry.Compiler]) + ','#10);
  WriteUtf8(AStream, '      "platform": '
    + JsonStr(DcuPlatformName[AEntry.Platform]) + ','#10);
  WriteUtf8(AStream, '      "primarySource": '
    + JsonStr(AEntry.PrimarySource) + ','#10);

  WriteUtf8(AStream, '      "includes": ');
  WriteStringArray(AStream, AEntry.Includes);
  WriteUtf8(AStream, ','#10);

  WriteUtf8(AStream, '      "interfaceUses": ');
  WriteStringArray(AStream, AEntry.InterfaceUses);
  WriteUtf8(AStream, ','#10);

  WriteUtf8(AStream, '      "implementationUses": ');
  WriteStringArray(AStream, AEntry.ImplementationUses);
  WriteUtf8(AStream, ','#10);

  WriteUtf8(AStream, '      "typeRefs": ');
  WriteStringArray(AStream, AEntry.TypeRefs);
  WriteUtf8(AStream, ','#10);

  WriteUtf8(AStream, '      "methodRefs": ');
  WriteStringArray(AStream, AEntry.MethodRefs);
  WriteUtf8(AStream, ','#10);

  WriteUtf8(AStream, '      "exportedTypes": ');
  WriteStringArray(AStream, AEntry.ExportedTypes);
  WriteUtf8(AStream, ','#10);

  WriteUtf8(AStream, '      "exportedRoutines": ');
  WriteStringArray(AStream, AEntry.ExportedRoutines);
  WriteUtf8(AStream, #10);

  if AIsLast then
    WriteUtf8(AStream, '    }'#10)
  else
    WriteUtf8(AStream, '    },'#10);
end;

procedure WriteStringMap(AStream: TStream; const AKey: string;
  AMap: IKeyValue<string, IList<string>>);
var
  Bucket   : IList<string>;
  First    : Boolean;
  Item     : string;
  Pair     : TPair<string, IList<string>>;
  PairFirst: Boolean;
begin
  WriteUtf8(AStream, '  "' + AKey + '": {'#10);
  PairFirst := True;
  if AMap <> nil then
    for Pair in AMap do
    begin
      if not PairFirst then WriteUtf8(AStream, ','#10);
      Bucket := Pair.Value;
      WriteUtf8(AStream, '    ' + JsonStr(Pair.Key) + ': [');
      First := True;
      if Bucket <> nil then
        for Item in Bucket do
        begin
          if not First then WriteUtf8(AStream, ', ');
          WriteUtf8(AStream, JsonStr(Item));
          First := False;
        end;
      WriteUtf8(AStream, ']');
      PairFirst := False;
    end;
  WriteUtf8(AStream, #10'  }'#10);
end;

procedure WriteReverseIndex(AStream: TStream; const AIndex: TDcuIndex);
begin
  WriteStringMap(AStream, 'reverseImportIndex', AIndex.ReverseImportIndex);
end;

procedure WriteSymbolDefIndex(AStream: TStream; const AIndex: TDcuIndex);
begin
  WriteStringMap(AStream, 'symbolToDefiningUnit', AIndex.SymbolToDefiningUnit);
end;

{ TDcuIndexJsonWriter }

class procedure TDcuIndexJsonWriter.WriteToStream(const AIndex: TDcuIndex;
  AStream: TStream);
var
  I: Integer;
begin
  WriteUtf8(AStream, '{'#10);
  WriteUtf8(AStream, '  "schema": ' + IntToStr(AIndex.Schema) + ','#10);
  WriteUtf8(AStream, '  "buildTimestamp": '
    + JsonStr(DateToISO8601(AIndex.BuildTimestamp, True)) + ','#10);

  WriteUtf8(AStream, '  "rootDirs": ');
  WriteStringArray(AStream, AIndex.RootDirs);
  WriteUtf8(AStream, ','#10);

  if AIndex.Units <> nil then
    WriteUtf8(AStream, '  "dcuCount": ' + IntToStr(AIndex.Units.Count) + ','#10)
  else
    WriteUtf8(AStream, '  "dcuCount": 0,'#10);

  WriteCounters(AStream, AIndex);

  WriteUtf8(AStream, '  "units": ['#10);
  if AIndex.Units <> nil then
    for I := 0 to AIndex.Units.Count - 1 do
      WriteUnitEntry(AStream, AIndex.Units[I], I = AIndex.Units.Count - 1);
  WriteUtf8(AStream, '  ],'#10);

  WriteReverseIndex(AStream, AIndex);
  WriteUtf8(AStream, '  ,'#10);
  WriteSymbolDefIndex(AStream, AIndex);

  WriteUtf8(AStream, '}'#10);
end;

class procedure TDcuIndexJsonWriter.WriteToFile(const AIndex: TDcuIndex;
  const AFilePath: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilePath, fmCreate);
  try
    WriteToStream(AIndex, Stream);
  finally
    Stream.Free;
  end;
end;

class function TDcuIndexJsonWriter.WriteToString(const AIndex: TDcuIndex): string;
var
  MS   : TMemoryStream;
  Bytes: TBytes;
begin
  MS := TMemoryStream.Create;
  try
    WriteToStream(AIndex, MS);
    SetLength(Bytes, MS.Size);
    if MS.Size > 0 then
    begin
      MS.Position := 0;
      MS.ReadBuffer(Bytes[0], MS.Size);
    end;
    Result := TEncoding.UTF8.GetString(Bytes);
  finally
    MS.Free;
  end;
end;

{ TDcuIndexJsonReader }

function ParseCompiler(const AName: string): TDcuKnownCompiler;
var
  K: TDcuKnownCompiler;
begin
  for K := Low(TDcuKnownCompiler) to High(TDcuKnownCompiler) do
    if SameText(DcuKnownCompilerName[K], AName) then
      Exit(K);
  Result := dccUnknown;
end;

function ParsePlatform(const AName: string): TDcuPlatform;
var
  P: TDcuPlatform;
begin
  for P := Low(TDcuPlatform) to High(TDcuPlatform) do
    if SameText(DcuPlatformName[P], AName) then
      Exit(P);
  Result := dpUnknown;
end;

procedure ReadStringArray(AArr: TJSONArray; AOut: IList<string>);
var
  V: TJSONValue;
begin
  if AArr = nil then
    Exit;
  for V in AArr do
    if V is TJSONString then
      AOut.Add(TJSONString(V).Value);
end;

function ReadEntry(AObj: TJSONObject): TDcuIndexEntry;

  function StrField(const AName: string): string;
  var
    V: TJSONValue;
  begin
    Result := '';
    V := AObj.GetValue(AName);
    if V is TJSONString then
      Result := TJSONString(V).Value;
  end;

  function Int64Field(const AName: string): Int64;
  var
    V: TJSONValue;
  begin
    Result := 0;
    V := AObj.GetValue(AName);
    if V is TJSONNumber then
      Result := TJSONNumber(V).AsInt64;
  end;

  function ArrField(const AName: string): TJSONArray;
  var
    V: TJSONValue;
  begin
    Result := nil;
    V := AObj.GetValue(AName);
    if V is TJSONArray then
      Result := TJSONArray(V);
  end;

begin
  Result := Default(TDcuIndexEntry);
  Result.UnitName := StrField('unit');
  Result.FilePath := StrField('file');
  Result.FileSize := Int64Field('size');
  Result.Magic := StrField('magic');
  Result.Compiler := ParseCompiler(StrField('compiler'));
  Result.Platform := ParsePlatform(StrField('platform'));
  Result.PrimarySource := StrField('primarySource');

  Result.Includes := Collections.NewList<string>;
  Result.InterfaceUses := Collections.NewList<string>;
  Result.ImplementationUses := Collections.NewList<string>;
  Result.TypeRefs := Collections.NewList<string>;
  Result.MethodRefs := Collections.NewList<string>;
  Result.ExportedTypes := Collections.NewList<string>;
  Result.ExportedRoutines := Collections.NewList<string>;
  ReadStringArray(ArrField('includes'), Result.Includes);
  ReadStringArray(ArrField('interfaceUses'), Result.InterfaceUses);
  ReadStringArray(ArrField('implementationUses'), Result.ImplementationUses);
  ReadStringArray(ArrField('typeRefs'), Result.TypeRefs);
  ReadStringArray(ArrField('methodRefs'), Result.MethodRefs);
  // Schema v2 additions; absent in v1 indices, in which case the
  // empty lists already created above are the correct default.
  ReadStringArray(ArrField('exportedTypes'), Result.ExportedTypes);
  ReadStringArray(ArrField('exportedRoutines'), Result.ExportedRoutines);
end;

class function TDcuIndexJsonReader.LoadFromFile(const AFilePath: string): TDcuIndex;
begin
  Result := LoadFromString(TFile.ReadAllText(AFilePath, TEncoding.UTF8));
end;

class function TDcuIndexJsonReader.LoadFromString(const AJson: string): TDcuIndex;
var
  Arr     : TJSONArray;
  Bucket  : IList<string>;
  Entry   : TDcuIndexEntry;
  Pair    : TJSONPair;
  Reverse : TJSONObject;
  Root    : TJSONValue;
  RootObj : TJSONObject;
  V       : TJSONValue;
begin
  Result := Default(TDcuIndex);
  Result.Schema := DcuIndexSchemaVersion;
  Result.RootDirs := Collections.NewList<string>;
  Result.Units := Collections.NewPlainList<TDcuIndexEntry>;
  Result.ReverseImportIndex := Collections.NewKeyValue<string, IList<string>>;
  Result.SymbolToDefiningUnit := Collections.NewKeyValue<string, IList<string>>;

  Root := TJSONObject.ParseJSONValue(AJson);
  if not (Root is TJSONObject) then
  begin
    Root.Free;
    raise EReadError.Create('Index file is not a JSON object');
  end;
  RootObj := TJSONObject(Root);
  try
    V := RootObj.GetValue('schema');
    if V is TJSONNumber then
      Result.Schema := TJSONNumber(V).AsInt;

    Arr := RootObj.GetValue('rootDirs') as TJSONArray;
    ReadStringArray(Arr, Result.RootDirs);

    Arr := RootObj.GetValue('units') as TJSONArray;
    if Arr <> nil then
      for V in Arr do
        if V is TJSONObject then
        begin
          Entry := ReadEntry(TJSONObject(V));
          Result.Units.Add(Entry);
        end;

    Reverse := RootObj.GetValue('reverseImportIndex') as TJSONObject;
    if Reverse <> nil then
      for Pair in Reverse do
        if Pair.JsonValue is TJSONArray then
        begin
          Bucket := Collections.NewList<string>;
          ReadStringArray(TJSONArray(Pair.JsonValue), Bucket);
          Result.ReverseImportIndex.Add(LowerCase(Pair.JsonString.Value), Bucket);
        end;

    // Schema v2 added the symbol-to-defining-unit map. v1 indices
    // simply omit this object - the empty IKeyValue created above
    // remains the default.
    Reverse := RootObj.GetValue('symbolToDefiningUnit') as TJSONObject;
    if Reverse <> nil then
      for Pair in Reverse do
        if Pair.JsonValue is TJSONArray then
        begin
          Bucket := Collections.NewList<string>;
          ReadStringArray(TJSONArray(Pair.JsonValue), Bucket);
          Result.SymbolToDefiningUnit.Add(LowerCase(Pair.JsonString.Value), Bucket);
        end;
  finally
    RootObj.Free;
  end;
end;

end.
